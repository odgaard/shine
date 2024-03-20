package rise

import arithexpr.arithmetic.ArithExpr.toInt
import arithexpr.arithmetic.{ArithExpr, RangeUnknown}
import rise.autotune.configFileGeneration._
import rise.autotune.constraints._
import rise.autotune.execution._
import rise.core.DSL.Type.NatFunctionWrapper
import rise.core._
import rise.core.types._
import rise.elevate.Rise
import rise.openCL.DSL.oclRun
import shine.OpenCL.{GlobalSize, LocalSize}
import util.{Time, TimeSpan, writeToPath}

import java.io.{File, FileOutputStream, PrintWriter}
import scala.util.{Try}
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.sys.process._

import akka.actor.ActorSystem
import akka.grpc.scaladsl.ServiceHandler
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.http.scaladsl.server.Route
import scala.concurrent.{ExecutionContext, Future, Promise, Await}
import scala.concurrent.duration.Duration
import scala.util.{Success, Failure}

import rise.config_service._ // Import gRPC and protobuf classes generated from your .proto file

package object autotune {

  case class Tuner(hostCode: HostCode = HostCode("", "", ""), // defines necessary host-code to execute program
                   inputSizes: Seq[Nat] = Seq(), // todo think about multi-dimensional inputs
                   samples: Int = 100, // number of parameter configurations (samples) to evaluate
                   name: String = "RISE", // todo this has to match name in config file!
                   output: String = "autotuning", // folder to store output files in
                   timeouts: Timeouts = Timeouts(15000, 15000, 15000), // timeouts for codegen, compilation and execution
                   executionIterations: Int = 10, // defines, how many times the program is executed to determine the runtime a sample
                   runtimeStatistic: RuntimeStatistic = Median, // specifies, how to determine the runtime from multiple iterations (Median/Minimum)
                   speedupFactor: Double = 100, // defines at which threshold the iterations are dropped, if the execution is slow compared to current best
                   configFile: Option[String] = None, // specifies the location of a config-file, otherwise, a config file is generated
                   hmConstraints: Boolean = false, // enable constraints feature in HM (experimental)
                   saveToFile: Boolean = false,
                   failureMode: FailureMode = IntMax,
                   strategyMode: Option[(Expr, Map[String, Int], Map[String, List[Int]]) => Either[String, Expr]] = None, // enable strategy mode
                   executor: Option[Expr => (Either[AutoTuningError, Double], Option[Double], Option[Double], Option[Double])] = None, // todo change this to exeuction result
                   disableChecking: Boolean = false,
                   feasibility: Boolean = true,
                   tunerRoot: String = "/home/baco",
                   tunerPath: String = "baco/run.py",
                   tunerPython: String = "python3.9"
                  )

  // necessary host-code parts to execute the program
  case class HostCode(init: String, // define and initialize input/output (buffers)
                      compute: String, // call the function with the input and output
                      finish: String) // check output, destroy input/output buffers

  // timeouts for sub-parts of the evaluation of a parameter configuration (sample)
  case class Timeouts(codegenerationTimeout: Long, // timeout for code-generation part
                      compilationTimeout: Long, // timeout for compilation part
                      executionTimeout: Long // timeout for execution part
                     )

  // result of a complete tuning run and used tuner
  case class TuningResult(samples: Seq[Sample],
                          tuner: Tuner
                         )

  // todo model parameter
  // parameter : either[NatIdentifier to Nat, String to List?]

  // tuning sample representing result of one specific parameter configuration
  case class Sample2(parameters: Map[NatIdentifier, Nat], // specific parameter configuration
                     runtime: Either[AutoTuningError, TimeSpan[Time.ms]], // runtime or error
                     timestamp: Long, // timestamp of sample
                     tuningTimes: TuningTimes // durations of sub-parts
                    )

  // tuning sample representing result of one specific parameter configuration
  case class Sample(parameters: Map[String, TuningParameterValues], // specific parameter configuration
                    runtime: Either[AutoTuningError, TimeSpan[Time.ms]], // runtime or error
                    timestamp: Long, // timestamp of sample
                    cpuEnergy: Double = 0, // energy consumption
                    gpuEnergy: Double = 0, // energy consumption
                    tuningTimes: TuningTimes // durations of sub-parts
                   )

  // just example code here
  val test = Map.empty[String, TuningParameterValues]

  test.foreach(elem => {
    val test = elem._2
    test match {
      case ClassicParameter(value) =>
      case PermutationParameter(value) =>
    }
  })

  // workaround to support permutation variables
  trait TuningParameterValues

  case class ClassicParameter(
                               value: Int
                             ) extends TuningParameterValues

  case class PermutationParameter(
                                   value: List[Int]
                                 ) extends TuningParameterValues

  // durations of sub-parts of a tuning sample
  case class TuningTimes(total: Option[TimeSpan[Time.ms]], // total time
                         codegen: Option[TimeSpan[Time.ms]], // duration of code-generation part
                         compilation: Option[TimeSpan[Time.ms]], // duration of compilation part
                         execution: Option[TimeSpan[Time.ms]] // duration of execution part
                        )

  case class TuningStatistics(
                               name: String,
                               totalSamples: Int,
                               executionIterations: Int,
                               totalExecutions: Int,
                               totalDuration: TimeSpan[Time.s],
                               averageDuration: TimeSpan[Time.s]

                             )

  type Parameters = Set[NatIdentifier]

  // should we allow tuning params to be substituted during type inference?
  // this could allow to restrict the search space at compile time
  def tuningParam[A](name: String, w: NatFunctionWrapper[A]): A =
    w.f(TuningParameter(name, RangeUnknown))

  def tuningParam[A](name: String, r: arithexpr.arithmetic.Range, w: NatFunctionWrapper[A]): A =
    w.f(TuningParameter(name, r))

  object ServerControl {
    // Promise to control server lifecycle
    var stopPromise: Promise[Unit] = _
    var stopFuture: Future[Unit] = _
  }


  object TunerControl {
    var tuner: Tuner = _
    var start: Long = _
    var e: Expr = _
    var constraints: Set[rise.autotune.constraints.Constraint] = _
  }

  class ConfigurationServiceImpl(implicit ec: ExecutionContext) extends ConfigurationService {
    def readRAPLEnergy(): Long = {
      val raplPath = "/sys/class/powercap/intel-rapl/intel-rapl:0/energy_uj" // Adjust based on your system
      try {
        val energy = Process(s"cat $raplPath").!!.trim.toLong
        energy
      } catch {
        case e: Exception => 
          println("Failed to read RAPL energy: " + e.getMessage)
          0L
      }
    }


    def readGPUEnergy(durationInMillis: Double): Double = {
      Thread.sleep(1000) // Wait a bit for the logging to flush to disk
        // Read lines from the file, try to convert each line to Double, and filter out failures
      val powerReadings = scala.io.Source.fromFile("gpu_power.log").getLines()
                         .flatMap(line => Try(line.toDouble).toOption)
                         .toList

      val averagePower = if (powerReadings.isEmpty) 0.0 else powerReadings.sum / powerReadings.length
      val energyUsed = averagePower * durationInMillis / 1000.0 // Convert ms to seconds and calculate energy
      new java.io.File("gpu_power.log").delete() // Clean up
      energyUsed
    }

    def startGPUPowerLogging(interval_in_ms: Integer): Process = {
      // Construct the command to pass to the shell
      val cmd = Seq("bash", "-c", s"nvidia-smi --query-gpu=power.draw --format=csv,noheader,nounits -lms $interval_in_ms > gpu_power.log")
      
      // Run the command using the shell
      cmd.run()
    }


    def measureEnergyConsumption[T](function: => T): (T, Option[TimeSpan[Time.ms]], Double, Double) = {
      // Start GPU power logging
      val interval_in_ms = 10
      val gpuLogger = startGPUPowerLogging(interval_in_ms)
      val cpuEnergyBefore = readRAPLEnergy()
      val startTime = System.nanoTime()

      //Wraps the function in a try catch statement
      var result: T = null.asInstanceOf[T]
      try {
        result = function
      } catch {
        case e: Exception =>
          println("Error during execution: " + e.getMessage)
      } finally {
        gpuLogger.destroy()
      }

      val durationInMillis: Double = (System.nanoTime() - startTime).toDouble / 1e6 // Convert nanoseconds to milliseconds
      val cpuEnergyAfter = readRAPLEnergy()

      val gpuEnergyUsed = readGPUEnergy(durationInMillis) // Convert nanoseconds to milliseconds
      val cpuEnergyUsed = (cpuEnergyAfter - cpuEnergyBefore) / 1e6 // Convert microjoules to Joules
      val totalTime = Some(TimeSpan.inMilliseconds(durationInMillis))

      (result, totalTime, cpuEnergyUsed, gpuEnergyUsed)
    }


    // Implement the RunConfigurationsClientServer method
    private def computeSample(header: Array[String], parametersValues: Array[String]): Sample = {

      val tuner = TunerControl.tuner
      val start = TunerControl.start
      val e = TunerControl.e
      val constraints = TunerControl.constraints
      val totalStart = System.currentTimeMillis()

      tuner.strategyMode match {
        case Some(fun) =>
          // parse elems?
          val values = header.zip(parseParameters(parametersValues.mkString(","))).toMap

          val tuningParameterValues: Map[String, TuningParameterValues] = values.map(elem => elem._2 match {
            case x if x.contains(",") => (elem._1, PermutationParameter(elem._2.split(",").toList.map(elem => elem.toFloat.toInt)))
            case y => (elem._1, ClassicParameter(y.toFloat.toInt))
          })

          val values2 = values.map(elem => elem._2 match {
            case x if x.contains(",") => (elem._1, elem._2.split(",").toList.map(elem => elem.toFloat.toInt))
            case y => (elem._1, List(y.toFloat.toInt))
          })

          val tuningParams = values2.filter(elem => elem._2.size == 1).map(elem => (elem._1, elem._2.last))
          val permutationParams = values2.filter(elem => elem._2.size != 1)

          val e2 = fun(e, tuningParams, permutationParams)


          e2 match {
            case Right(expression) =>

              val result = tuner.executor.get(expression)

              val totalTime = Some(TimeSpan.inMilliseconds(
                (System.currentTimeMillis() - totalStart).toDouble)
              )

              result._1 match {
                case Right(value) =>

                  Sample(
                    parameters = tuningParameterValues,
                    runtime = Right(TimeSpan.inMilliseconds(value)),
                    timestamp = System.currentTimeMillis() - start,
                    tuningTimes = TuningTimes(
                      totalTime, Some(TimeSpan.inMilliseconds(result._2.get)), Some(TimeSpan.inMilliseconds(result._3.get)), Some(TimeSpan.inMilliseconds(result._4.get)))
                  )

                case Left(error) =>

                  Sample(
                    parameters = tuningParameterValues,
                    runtime = Left(error),
                    timestamp = System.currentTimeMillis() - start,
                    tuningTimes = TuningTimes(
                      totalTime, Some(TimeSpan.inMilliseconds(result._2.get)), Some(TimeSpan.inMilliseconds(result._3.get)), Some(TimeSpan.inMilliseconds(result._4.get)))
                  )
              }
            case Left(error) =>

              val totalTime = Some(TimeSpan.inMilliseconds((System.currentTimeMillis() - totalStart).toDouble))
              Sample(
                parameters = tuningParameterValues,
                runtime = Left(AutoTuningError(SUBSTITUTION_ERROR, Some(error))),
                timestamp = System.currentTimeMillis() - start,
                tuningTimes = TuningTimes(totalTime, None, None, None)
              )
          }
        case None =>
          // parse here

          println("header: " + header.mkString(","))
          println("parametersValues: " + parametersValues.mkString(","))

          val parametersValuesMap: Map[NatIdentifier, Nat] = header.zip(parametersValues).map { case (h, p) =>
            NatIdentifier(h) -> (p.toFloat.toInt: Nat)
          }.toMap

          // check if we have to check
          val check = tuner.disableChecking match {
            case true => true
            case false => checkConstraints(constraints, parametersValuesMap)
          }

          if (check) {

            tuner.executor match {
              case Some(exec) =>
                val result = exec(rise.core.substitute.natsInExpr(parametersValuesMap.toMap[Nat, Nat], e))

                val totalTime = Some(TimeSpan.inMilliseconds(
                  (System.currentTimeMillis() - totalStart).toDouble)
                )

                result._1 match {
                  case Right(value) =>

                    Sample(
                      parameters = parametersValuesMap.map(elem => (elem._1.toString, ClassicParameter(toInt(elem._2)))),
                      runtime = Right(TimeSpan.inMilliseconds(value)),
                      timestamp = System.currentTimeMillis() - start,
                      tuningTimes = TuningTimes(
                        totalTime, Some(TimeSpan.inMilliseconds(result._2.get)), Some(TimeSpan.inMilliseconds(result._3.get)), Some(TimeSpan.inMilliseconds(result._4.get)))
                    )

                  case Left(error) =>

                    Sample(
                      parameters = parametersValuesMap.map(elem => (elem._1.toString, ClassicParameter(toInt(elem._2)))),
                      runtime = Left(error),
                      timestamp = System.currentTimeMillis() - start,
                      tuningTimes = TuningTimes(
                        totalTime, Some(TimeSpan.inMilliseconds(result._2.get)), Some(TimeSpan.inMilliseconds(result._3.get)), Some(TimeSpan.inMilliseconds(result._4.get)))
                    )
                }

              case None =>

                val (result, totalTime, cpuEnergyUsed, gpuEnergyUsed) = measureEnergyConsumption {
                  execute(
                  rise.core.substitute.natsInExpr(parametersValuesMap.toMap[Nat, Nat], e),
                  tuner.hostCode,
                  tuner.timeouts,
                  tuner.executionIterations,
                  tuner.speedupFactor,
                  tuner.runtimeStatistic
                )
                }

                Sample(
                  parameters = parametersValuesMap.map(elem => (elem._1.toString, ClassicParameter(toInt(elem._2)))),
                  runtime = result.runtime,
                  timestamp = System.currentTimeMillis() - start,
                  cpuEnergy = cpuEnergyUsed,
                  gpuEnergy = gpuEnergyUsed,
                  tuningTimes = TuningTimes(
                    totalTime, result.codegenTime, result.compilationTime, result.executionTime)
                    //totalTime, cpuEnergyUsed, gpuEnergyUsed, result.executionTime)
                )
            }
          } else {
            val totalTime = Some(TimeSpan.inMilliseconds((System.currentTimeMillis() - totalStart).toDouble))
            Sample(
              parameters = parametersValuesMap.map(elem => (elem._1.toString, ClassicParameter(toInt(elem._2)))),
              runtime = Left(AutoTuningError(CONSTRAINTS_ERROR, None)),
              timestamp = System.currentTimeMillis() - start,
              tuningTimes = TuningTimes(totalTime, None, None, None)
            )
          }
      }
    }

    override def runConfigurationsClientServer(request: ConfigurationRequest): Future[ConfigurationResponse] = {

      // Process the request to generate a response
      val conf: Option[Configuration] = request.configurations

      // Initialize mutable collections to accumulate keys and values
      val headers = scala.collection.mutable.ArrayBuffer[String]()
      val values = scala.collection.mutable.ArrayBuffer[String]()

      conf.foreach { configuration =>
        configuration.parameters.foreach { case (key, parameter) =>
          // Assuming `value` represents the extracted value from each parameter type
          parameter.paramType match {
            case paramType if paramType.integerParam.isDefined =>
              headers += key
              values += paramType.integerParam.get.value.toString
            case paramType if paramType.realParam.isDefined =>
              headers += key
              values += paramType.realParam.get.value.toString
            case paramType if paramType.categoricalParam.isDefined =>
              headers += key
              values += paramType.categoricalParam.get.value.toString
            case paramType if paramType.ordinalParam.isDefined =>
              headers += key
              values += paramType.ordinalParam.get.value.toString
            case paramType if paramType.stringParam.isDefined =>
              headers += key
              values += paramType.stringParam.get.value
            case paramType if paramType.permutationParam.isDefined =>
              headers += key
              // Adds a paranthesis before and after the values
              values += "(" + paramType.permutationParam.get.values.mkString(",") + ")"
          }
        }
      }

      // Convert the ArrayBuffer to Array if necessary
      val headerArray: Array[String] = headers.toArray
      val valuesArray: Array[String] = values.toArray

      // Creating comma-separated strings
      val headerString = headerArray.mkString(",")
      val valuesString = valuesArray.mkString(",")

      // Now you have your header and values as comma-separated strings
      println(s"Header: $headerString")
      println(s"Values: $valuesString")

      val sample: Sample = this.computeSample(headerArray, valuesArray)
      println(sample)

      val metrics: Seq[Metric] = Seq(
        Metric(Seq(sample.runtime match {
          case Right(timeSpan) => timeSpan.value.toDouble
          case Left(_) => 0.0
        }), "compute_time"),
        Metric(Seq(sample.cpuEnergy), "cpuEnergy"),
        Metric(Seq(sample.gpuEnergy), "energy"),
        Metric(Seq(sample.tuningTimes.codegen.getOrElse(util.TimeSpan(0, util.Time.Millisecond)).value.toDouble), "codegen"),
        Metric(Seq(sample.tuningTimes.compilation.getOrElse(util.TimeSpan(0, util.Time.Millisecond)).value.toDouble), "codegen"),
        Metric(Seq(sample.tuningTimes.execution.getOrElse(util.TimeSpan(0, util.Time.Millisecond)).value.toDouble), "execution")
      )
      println(metrics)
      val timestamps: Option[Timestamp] = Some(Timestamp(System.currentTimeMillis()))
      val feasible: Option[Feasible] = Some(Feasible(true))
      // Construct and return the ConfigurationResponse
      Future.successful(ConfigurationResponse(metrics, timestamps, feasible))
    }

    // Implement the Shutdown method
    override def shutdown(request: ShutdownRequest): Future[ShutdownResponse] = {
      // Logic to perform shutdown, e.g., releasing resources
      println("Shutdown requested")
      ServerControl.stopPromise.success(())
      Future.successful(ShutdownResponse(success = true))
    }
  }

  def search(tuner: Tuner)(e: Expr): TuningResult = {

    TunerControl.tuner = tuner
    TunerControl.start = System.currentTimeMillis()
    TunerControl.e = e

    ServerControl.stopPromise = Promise[Unit]()
    ServerControl.stopFuture = ServerControl.stopPromise.future

    val parameters = collectParameters(e)

    // inject input sizes into constraints
    val inputs = getInputs(e)
    val inputMap = (inputs zip tuner.inputSizes).toMap
    val constraints: Set[rise.autotune.constraints.Constraint] = collectConstraints(e, parameters)
      .map(constraint => constraint.substitute(inputMap.asInstanceOf[Map[ArithExpr, ArithExpr]]))
    
    TunerControl.constraints = constraints

    // generate json if necessary
    tuner.configFile match {
      case None =>
        //        println("generate configuration file")

        val filePath = tuner.saveToFile match {
          case true => tuner.output + "/" + tuner.name + ".json"
          case false => {
            ("mkdir -p tmp" !!)
            "/tmp/" + tuner.name + ".json"
          }
        }

        val configFileString = generateJSON(parameters, constraints, tuner)
        //        println("configFile: \n" + configFileString)
        val file = new PrintWriter(
          new FileOutputStream(
            new File(filePath), false))
        file.write(configFileString)
        file.close()
      case _ => // println("use given configuration file")
    }

    val configFile = tuner.configFile match {
      case Some(filename) =>
        filename.substring(0, 1) match {
          case "/" => os.Path.apply(filename)
          case _ => os.Path.apply(os.pwd.toString() + "/" + filename)
        }
      case None => os.Path.apply(
        tuner.saveToFile match {
          case true => os.pwd.toString() + "/" + tuner.output + "/" + tuner.name + ".json"
          case false => "/tmp/" + tuner.name + ".json"
        }
      )
    }

    implicit val system: ActorSystem = ActorSystem("HypermapperServer")
    implicit val ec: ExecutionContext = system.dispatcher

    // Assuming ConfigurationServiceHandler.partial(...) returns a function
    val serviceFunction: HttpRequest => Future[HttpResponse] =
      ConfigurationServiceHandler.partial(new ConfigurationServiceImpl)

    // Convert the function to a PartialFunction
    val service: PartialFunction[HttpRequest, Future[HttpResponse]] = {
      case req: HttpRequest => serviceFunction(req)
    }

    // Now, use the converted service with concatOrNotFound
    val handler: HttpRequest => Future[HttpResponse] = ServiceHandler.concatOrNotFound(service)

    // Bind the service to a port
    val bindingFuture = Http().newServerAt("0.0.0.0", 50051).bind(handler)
    println("Server started at localhost:50051")

    // Method to stop the server
    def stopServer(bindingFuture: Future[Http.ServerBinding]): Unit = {
      bindingFuture
        .flatMap(_.unbind()) // Trigger unbinding from the port
        .onComplete { _ =>
          println("Server stopped")
          system.terminate() // Shutdown the ActorSystem
        }
    }

    // Example of stopping the server after some condition is met
    ServerControl.stopFuture.onComplete { _ =>
      stopServer(bindingFuture)
    }

    // Block until the stop condition is met
    Await.result(ServerControl.stopFuture, Duration.Inf)

    val samples = ListBuffer[Sample]()
    val tuningResult = TuningResult(samples.toSeq, tuner)
    tuningResult
  }

  def getUniqueFilepath(path: String, ending: String): String = {
    new File(path).exists() match {
      case true => path.substring(0, path.length - ending.length) + "_" + System.currentTimeMillis() + ending
      case false => path
    }
  }

  // wrap ocl run to a function
  def wrapOclRun(localSize: LocalSize, globalSize: GlobalSize)
                (expr: Expr): Expr = {
    expr match {
      // fun(x => e)
      case l@Lambda(x, e) =>
        Lambda(x, wrapOclRun(localSize, globalSize)(e))(l.t)
      // depFun(x => e)
      case dl@DepLambda(kind, x, e) =>
        DepLambda(kind, x, wrapOclRun(localSize, globalSize)(e))(dl.t)
      case e =>
        oclRun(localSize, globalSize)(e)
    }
  }

  def getBest(samples: Seq[Sample]): Option[Sample] = {
    val best = samples.reduceLeft(min)
    best.runtime match {
      case Right(_) => Some(best)
      case Left(_) => None
    }
  }

  def getDuration(tuningResult: TuningResult): TimeSpan[Time.ms] = {
    val duration = tuningResult.samples.apply(tuningResult.samples.size).timestamp -
      tuningResult.samples.apply(0).timestamp

    TimeSpan.inMilliseconds(duration.toDouble)
  }

  def getSamples(tuningResult: TuningResult): Int = {
    tuningResult.samples.size
  }

  // helper functions
  private def min(s1: Sample, s2: Sample): Sample = {
    s1.runtime match {
      case Right(s1Runtime) =>
        s2.runtime match {
          case Right(s2Runtime) =>
            if (s1Runtime.value < s2Runtime.value) {
              s1
            } else {
              s2
            }
          case Left(_) => s1
        }
      case Left(_) =>
        s2.runtime match {
          case Right(_) => s2
          case Left(_) => s1
        }
    }
  }

  def getInputs(e: Expr): Seq[NatIdentifier] = {
    getInputsRec(Seq.empty[NatIdentifier], e)
  }

  def getInputsRec(inputs: Seq[NatIdentifier], e: Expr): Seq[NatIdentifier] = {
    e match {
      case DepLambda(NatKind, n: NatIdentifier, subexpr) => getInputsRec(inputs :+ n, subexpr)
      case _ => inputs
    }
  }

  def parseParameters(request: String): Seq[String] = {
    val it = request.replaceAll(""" +""", "").split(",").iterator


    var output = scala.collection.Seq.empty[String]
    //    var output = new ListBuffer[String]

    while (it.hasNext) {
      var value = scala.collection.Seq.empty[String]
      val elem = it.next().replaceAll(",", "")
      //      println("elem: " + elem)
      elem match {
        case x if x.contains("(") => {
          value = value ++ scala.collection.Seq(x.replaceAll("""\(""", ""))
          // add until )
          var inPerm = true
          while (it.hasNext && inPerm) {
            val perm2 = it.next().replaceAll(",", "")
            //            println("perm2: " + perm2)

            perm2 match {
              case y if y.contains(")") =>
                value = value ++ scala.collection.Seq(y.replaceAll("""\)""", ""))
                inPerm = false
              case _ => value = value ++ scala.collection.Seq(perm2)
            }
          }
        }
        case y => value = value ++ scala.collection.Seq(y)
      }

      output = output ++ scala.collection.Seq(value.mkString(","))
    }

    output.toSeq
  }
}
