package rise

import arithexpr.arithmetic.{ArithExpr, BoolExpr, NamedVar, RangeAdd, RangeMul, RangeUnknown}
import rise.core.DSL.Type.NatFunctionWrapper
import rise.core._
import rise.core.types._
import shine.OpenCL.{GlobalSize, KernelExecutor, LocalSize, ScalaFunction, `(`, `)=>`, `,`}
import util.{Time, TimeSpan, gen, monads}
import java.io.{File, FileOutputStream, PrintWriter}
import java.security.Policy.Parameters

import arithexpr.arithmetic.BoolExpr.ArithPredicate
import rise.core.DSL.ToBeTyped
import rise.openCL.DSL.oclRun

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Random

package object autotune {
  type Parameters = Set[NatIdentifier]
  case class Sample(parameters: Map[NatIdentifier, Nat], runtime: Option[TimeSpan[Time.ms]], timestamp: Long)

  // should we allow tuning params to be substituted during type inference?
  // this could allow to restrict the search space at compile time
  def tuningParam[A](name: String, w: NatFunctionWrapper[A]): A =
    w.f(NatIdentifier(name, RangeUnknown, isExplicit = true, isTuningParam = true))
  def tuningParam[A](name: String, r: arithexpr.arithmetic.Range, w: NatFunctionWrapper[A]): A =
    w.f(NatIdentifier(name, r, isExplicit = true, isTuningParam = true))

  def search(e: Expr): Seq[Sample] = {
    // timestamp of starting point
    val start = System.currentTimeMillis()

    // collect parameters
    val parameters = collectParameters(e)

    // collect constraints
    val constraints = collectConstraints(e, parameters)

    // generate json and write to tmp directory
    val config_file = generateJSON(parameters)

    println("parameters: \n" + parameters)
    println("constraints: \n" + constraints)
    println("json: \n" + config_file)

    // open file
    val file = new PrintWriter(
      new FileOutputStream(new File("autotuning/tmp.json"), false))

    // write to file and close
    file.write(config_file)
    file.close()

    // compute function value as result for hypermapper
    val computeSample: (Array[String], Array[String]) => Sample = (header, parametersValues) => {
      val parametersValuesMap = header.zip(parametersValues).map { case (h, p) =>
        NatIdentifier(h, isExplicit = true) -> (p.toInt : Nat)
      }.toMap


      checkConstraints(constraints, parametersValuesMap) match {
        case true => {
          Sample(parametersValuesMap, execute(rise.core.substitute.natsInExpr(parametersValuesMap, e)), System.currentTimeMillis() - start)
        }
        case false => {
          Sample(parametersValuesMap, None, System.currentTimeMillis() - start)
        }
      }
    }

    val hypermapperBinary = os.Path.expandUser("~") / ".local" / "bin" / "hypermapper"

    val configFile = os.pwd / "autotuning" / "tmp.json"

    assert( os.isFile(hypermapperBinary) && os.isFile(configFile) )

    val hypermapper = os.proc(hypermapperBinary, configFile).spawn()


    // create output Seq
    var samples = new ListBuffer[Sample]()

    var done = false
    while (hypermapper.isAlive() && !done) {
      hypermapper.stdout.readLine() match {
        case "End of HyperMapper" =>
          done = true
          println("End of HyperMapper -- done")
        case "Best point found:" =>
          val headers = hypermapper.stdout.readLine()
          val values = hypermapper.stdout.readLine()
          hypermapper.stdout.readLine() // consume empty line
          println(s"Best point found\nHeaders: ${headers}Values: ${values}")
        case request if request.contains("warning") =>
          println(s"[Hypermapper] $request")
        case request =>
          println(s"Request: $request")
          val numberOfEvalRequests = request.split(" ")(1).toInt
          // read in header
          val header = hypermapper.stdout.readLine().split(",").map(x => x.trim())
          // start forming response
          var response = s"${header.mkString(",")},runtime,Valid\n"
          for (_ <- Range(0, numberOfEvalRequests)) {
            // read in parameters values
            val parametersValues = hypermapper.stdout.readLine().split(",").map(x => x.trim())
            // compute sample (including function value aka runtime)
            val sample = computeSample(header, parametersValues)
            // append sample to Samples
            samples += sample
            // append response
            sample.runtime match {
              case None => response += s"${parametersValues.mkString(",")},-1,False\n"
              case Some(value) => response += s"${parametersValues.mkString(",")},${value.value},True\n"
            }
          }
          print(s"Response: $response")
          // send response to Hypermapper
          hypermapper.stdin.write(response)
          hypermapper.stdin.flush()
      }
    }


    // delete tmp json file
//    val fileDelete = new File("autotuning/tmp.json")
//    fileDelete.delete()

    samples.toSeq
  }

  // wrap ocl run to a function
  def wrapOclRun(localSize: LocalSize, globalSize: GlobalSize)
                (expr: Expr): Expr = {
    expr match {
      // fun(x => e)
      case l@Lambda(x,e) =>
        Lambda(x, wrapOclRun(localSize, globalSize)(e))(l.t)
      // depFun(x => e)
      case dl@DepLambda(x, e) =>
        x match {
          case n: NatIdentifier =>
            DepLambda[NatKind](n, wrapOclRun(localSize, globalSize)(e))(dl.t)
          case dt: DataTypeIdentifier =>
            DepLambda[DataKind](dt, wrapOclRun(localSize, globalSize)(e))(dl.t)
          case a: AddressSpaceIdentifier =>
            DepLambda[AddressSpaceKind](a, wrapOclRun(localSize, globalSize)(e))(dl.t)
          case n2n: NatToNatIdentifier =>
            DepLambda[NatToNatKind](n2n, wrapOclRun(localSize, globalSize)(e))(dl.t)
          case n2d: NatToDataIdentifier =>
            DepLambda[NatToDataKind](n2d, wrapOclRun(localSize, globalSize)(e))(dl.t)
        }
      case e => oclRun(localSize, globalSize)(e)
    }
  }

  // check constraints for given values and returns boolean
  def checkConstraints(constraints: Set[Constraint], values: Map[NatIdentifier, Nat]): Boolean = {
    val map = values.asInstanceOf[Map[ArithExpr, ArithExpr]]
    constraints.forall(c => c.substitute(map).isSatisfied())
  }

  def execute(e: Expr): Option[TimeSpan[Time.ms]]  = {
//      val e2 = wrapOclRun(e)(LocalSize(1), GlobalSize(1))

      val m = gen.opencl.hosted.fromExpr(e)

      val main = """
    const int N = 32;
    int main(int argc, char** argv) {
      Context ctx = createDefaultContext();
      Buffer input = createBuffer(ctx, N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
      Buffer output = createBuffer(ctx, N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_WRITE);

      float* in = hostBufferSync(ctx, input, N * sizeof(float), HOST_WRITE);
      for (int i = 0; i < N; i++) {
        in[i] = 1;
      }

      foo(ctx, output, input, input);

      float* out = hostBufferSync(ctx, output, N * sizeof(float), HOST_READ);

//      for (int i = 0; i < N; i++) {
//        printf(" %f \n", out[i]);
//      }

      destroyBuffer(ctx, input);
      destroyBuffer(ctx, output);
      destroyContext(ctx);
      return EXIT_SUCCESS;
    }
    """


    val program = shine.OpenCL.Module.translateToString(m) + main

    // execute program
    try{
      val result = util.ExecuteOpenCL.executeWithRuntime(program, "zero_copy")
      Some(result)
    } catch{
      case e:Throwable => println("error: \n" + e)
        None
    }
  }


  def collectParameters(e: Expr): Parameters = {
    var params = scala.collection.mutable.Set[NatIdentifier]()
    traverse.traverse(e, new traverse.PureTraversal {
      override def nat: Nat => monads.Pure[Nat] = n =>
        return_(n.visitAndRebuild({
          case n: NatIdentifier if n.isTuningParam =>
            params += n
            n
          case ae => ae
        }))
    })
    params.toSet
  }

  private def collectInputNats(e: Expr): Set[NatIdentifier] = {
    @tailrec
    def iter(e: Expr, inputs: Set[NatIdentifier]): Set[NatIdentifier] = {
      e match {
        case DepLambda(x: NatIdentifier, e) => iter(e, inputs + x)
        case DepLambda(_, e) => iter(e, inputs)
        case Lambda(_, e) => iter(e, inputs)
        case _ => inputs
      }
    }
    iter(e, Set.empty)
  }

  private def isPowerOf(a: Int, b: Int): Boolean = {
    val p: Int = Math.round(Math.log(a) / Math.log(b)).toInt
    Math.round(Math.pow(b, p)) == a
  }

  sealed trait Constraint {
    def substitute(map: Map[ArithExpr, ArithExpr]): Constraint = this match {
      case PredicateConstraint(n) =>
        PredicateConstraint(n.substitute(map).getOrElse(n))
      case RangeConstraint(n, r) =>
        RangeConstraint(n.substitute(map).getOrElse(n), r.substitute(map).getOrElse(r))
    }

    def isSatisfied(): Boolean = this match {
      case PredicateConstraint(n: ArithPredicate) =>
        n.evaluate.contains(true)
      // TODO: it feels like checking if a nat is inside a range should be part of arithexpr
      case RangeConstraint(n, RangeAdd(start, stop, step)) =>
        ArithPredicate(start, n.min, ArithPredicate.Operator.<=).evaluate.contains(true) &&
        ArithPredicate(n.max, stop, ArithPredicate.Operator.<=).evaluate.contains(true) &&
        n % step == (0: Nat)
      case RangeConstraint(n, RangeMul(start, stop, mul)) =>
        ArithPredicate(start, n.min, ArithPredicate.Operator.<=).evaluate.contains(true) &&
        ArithPredicate(n.max, stop, ArithPredicate.Operator.<=).evaluate.contains(true) &&
        isPowerOf(n.eval, mul.eval)
      case _ =>
        throw new Exception(s"no support for checking $this")
    }
  }
  case class PredicateConstraint(n: BoolExpr) extends Constraint {
    override def toString: String = n.toString
  }
  case class RangeConstraint(n: Nat, r: arithexpr.arithmetic.Range) extends Constraint {
    override def toString: String = s"($n) in $r"
  }

  // we only look at constraints on top-level nats
  def collectConstraints(e: Expr, parameters: Parameters): Set[Constraint] = {
    import arithexpr.arithmetic._
    import BoolExpr._

    val paramOrInput = (parameters ++ collectInputNats(e)).map(_.name)
    val cs = collection.mutable.Set[Constraint]()

    def addPredicate(p: ArithPredicate): Unit = {
      if (!p.evaluate.contains(true)) {
        cs += PredicateConstraint(p)
      }
    }

    traverse.traverse(e, new traverse.PureTraversal {
      override def expr: Expr => monads.Pure[Expr] = { e =>
        e match {
          case DepApp(DepApp(DepApp(DepApp(DepApp(DepApp(
            rise.openCL.primitives.oclRunPrimitive(),
            ls0: Nat), ls1: Nat), ls2: Nat),
            gs0: Nat), gs1: Nat), gs2: Nat)
          =>
            for (s <- Seq(ls0, ls1, ls2, gs0, gs1, gs2)) {
              addPredicate(ArithPredicate(s, 1, ArithPredicate.Operator.>=))
            }
            for ((ls, gs) <- Seq((ls0, gs0), (ls1, gs1), (ls2, gs2))) {
              cs += RangeConstraint(gs, RangeAdd(0, PosInf, ls))
            }
          case _ =>
        }
        super.expr(e)
      }

      override def datatype: DataType => monads.Pure[DataType] = { t =>
        t match {
          case ArrayType(n, _) if n.varList.forall(v => paramOrInput(v.name)) =>
            addPredicate(ArithPredicate(n, 0, ArithPredicate.Operator.>=))
          case VectorType(n, _) if n.varList.forall(v => paramOrInput(v.name)) =>
            cs += RangeConstraint(n, RangeMul(2, 16, 2))
          case _ =>
        }
        super.datatype(t)
      }

      override def nat: Nat => monads.Pure[Nat] = n =>
        return_(n.visitAndRebuild { m =>
          if (m.varList.forall(v => paramOrInput(v.name))) { m match {
            case Prod(parts) =>
              var (num, denum) = parts.partition {
                case Pow(_, Cst(-1)) => false
                case _ => true
              }
              denum = denum.map { case Pow(b, Cst(-1)) => b }
              if (denum.nonEmpty) { // num /^ denum
                val aNum = num.fold(1: ArithExpr)(_ * _)
                val aDenum = denum.fold(1: ArithExpr)(_ * _)
                if (aNum % aDenum != Cst(0)) {
                  cs += RangeConstraint(aNum, RangeAdd(0, PosInf, aDenum))
                }
              }
            case Mod(x, _) =>
              addPredicate(ArithPredicate(x, 0, ArithPredicate.Operator.>=))
            case _ => ()
          }}
          m
        })
    })

    cs.toSet
  }


  def generateJSON(p: Parameters): String = {

    // create header for hypermapper configuration file
    // WARNING: configuration is partially hard coded
    val header =
    """{
      | "application_name" : "rise",
      | "optimization_objectives" : ["runtime"],
      | "hypermapper_mode" : {
      |   "mode" : "client-server"
      | },
      | "feasible_output" : {
      |   "enable_feasible_predictor" : true,
      |   "name" : "Valid",
      |   "true_value" : "True",
      |   "false_value" : "False"
      | },
      | "design_of_experiment" : {
      |   "doe_type" : "random sampling",
      |   "number_of_samples" : 100
      | },
      | "optimization_iterations" : 100,
      | "input_parameters" : {
      |""".stripMargin

    // create entry foreach parameter
    var parameter = ""

    p.foreach(elem => {

      val parameterRange = elem.range match {
        case RangeAdd(start, stop, step) =>  {
          // check if all elements are evaluable
          // Todo check start and stop

          // HACK
          // if step is not evaluable use 1 instead
          val stepWidth = step.isEvaluable match{
            case true => step.eval
            case false => 1
          }

          val x = List.range(start.evalInt, stop.evalInt+1)
          val values = x.filter(_ % stepWidth == 0)

          listToString(values)
        }
        case RangeMul(start, stop, mul) => {
          // check if all elements are evaluable
          // Todo check start and stop

          mul.isEvaluable match {
            case true => {
              val maxVal = scala.math.log(stop.evalInt)/scala.math.log(mul.evalDouble)
              val powers:List[Int] = List.range(start.evalInt, maxVal.toInt+1)
              val values:List[Int] = powers.map(power => scala.math.pow(mul.evalInt, power).toInt)

              listToString(values)
            }
            case false =>
              listToString(List.range(start.evalInt, stop.evalInt+1))
          }
        }
        case _ => {
          println("Not yet implemented")
          ""
        }
      }

      val parameterEntry =
        s"""   "${elem.name}" : {
           |       "parameter_type" : "ordinal",
           |       "values" : ${parameterRange}
           |   },
           |""".stripMargin

      parameter += parameterEntry
    })

    // remove last comma
    val parameterSection = parameter.dropRight(2) + "\n"

    val foot =
      """ }
        |}
        |""".stripMargin

    header + parameterSection + foot
  }

  def applyBest(e: Expr, samples: Seq[Sample]): Expr = {
    val best = getBest(samples)
    best match {
      case Some(_) => rise.core.substitute.natsInExpr(best.get.parameters, e)
      case None => e // maybe throw exception?
    }
  }

  def applySample(e: Expr, sample: Sample): Expr = {
    rise.core.substitute.natsInExpr(sample.parameters, e)
  }

  def getBest(samples: Seq[Sample]): Option[Sample] = {
    val best = samples.reduceLeft(min)
    best.runtime match {
      case Some(_) => Some(best)
      case None => None
    }
  }

  private def min(s1: Sample, s2: Sample): Sample = {
    s1.runtime match {
      case Some(s1Runtime) => {
        s2.runtime match {
          case Some(s2Runtime) => {
            s1Runtime.value < s2Runtime.value match {
              case true => s1
              case false => s2
            }
          }
          case None => s1
        }
      }
      case None => {
        s2.runtime match{
          case Some(_) => s2
          case None => s1
        }
      }
    }
  }

  def listToString(list: List[Int]): String = {

    var valueString = "["
    list.foreach(value => {
      valueString += "" + value + ", "
    })

    var valueStringFinal= valueString.dropRight(2)
    valueStringFinal += "]"

    valueStringFinal
  }

}