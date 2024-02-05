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
                   timeouts: Timeouts = Timeouts(5000, 5000, 5000), // timeouts for codegen, compilation and execution
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
    val stopPromise: Promise[Unit] = Promise[Unit]()
    val stopFuture: Future[Unit] = stopPromise.future
  }


  object TunerControl {
    var tuner: Tuner = _
    var start: Long = _
    var e: Expr = _
    var constraints: Set[rise.autotune.constraints.Constraint] = _
  }

  class ConfigurationServiceImpl(implicit ec: ExecutionContext) extends ConfigurationService {
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

                // execute
                val result = execute(
                  rise.core.substitute.natsInExpr(parametersValuesMap.toMap[Nat, Nat], e),
                  tuner.hostCode,
                  tuner.timeouts,
                  tuner.executionIterations,
                  tuner.speedupFactor,
                  tuner.runtimeStatistic
                )
                val totalTime = Some(TimeSpan.inMilliseconds(
                  (System.currentTimeMillis() - totalStart).toDouble)
                )
                Sample(
                  parameters = parametersValuesMap.map(elem => (elem._1.toString, ClassicParameter(toInt(elem._2)))),
                  runtime = result.runtime,
                  timestamp = System.currentTimeMillis() - start,
                  tuningTimes = TuningTimes(
                    totalTime, result.codegenTime, result.compilationTime, result.executionTime)
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

      val totalStart = System.currentTimeMillis()
      val tuner = Tuner
      val parametersValuesMap = None
      /*
      val conf: Option[Configuration] = request.configurations
      conf.foreach { configuration =>
        configuration.parameters.foreach {
          case (key, parameter) => parameter.paramType match { // Assume `paramType` is the field holding the parameter type.
            case IntegerParam(value, _) => println(s"$key: $value")
            case PermutationParam(value, _) => println(s"$key: $value")
            case StringParam(value, _) => println(s"$key: $value")
            case OrdinalParam(value, _) => println(s"$key: $value")
            case CategoricalParam(value, _) => println(s"$key: $value")
            case RealParam(value, _) => println(s"$key: $value")
            // Add default case to handle unexpected cases
            case _ => println("Unknown parameter type for key $key")
          }
        }
      }
      */

/*
      val conf: Option[Configuration] = request.configurations

      conf.foreach { configuration =>
        configuration.parameters.foreach { case (key, parameter) =>
          parameter.paramType match {
            case IntegerParam(value) if value.isDefined =>
              println(s"$key: ${value.get}")
            case RealParam(value) if value.isDefined =>
              println(s"$key: ${value.get}")
            case CategoricalParam(value) if value.isDefined =>
              println(s"$key: ${value.get}")
            case OrdinalParam(value) if value.isDefined =>
              println(s"$key: ${value.get}")
            case StringParam(value) if value.isDefined =>
              println(s"$key: ${value.get}")
            case PermutationParam(value) if value.isDefined =>// Initialization logic
              println(s"$key: ${value.get}")
            case _ =>
              println(s"$key: Unknown or empty parameter type")
          }
        }
      }
      */
      /*
      val conf: Option[Configuration] = request.configurations

      conf.foreach { configuration =>
        configuration.parameters.foreach { case (key, parameter) =>
          parameter.paramType match {
            case paramType if paramType.integerParam.isDefined =>
              println(s"$key: ${paramType.integerParam.get}")
            case paramType if paramType.realParam.isDefined =>
              println(s"$key: ${paramType.realParam.get}")
            case paramType if paramType.categoricalParam.isDefined =>
              println(s"$key: ${paramType.categoricalParam.get}")
            case paramType if paramType.ordinalParam.isDefined =>
              println(s"$key: ${paramType.ordinalParam.get}")
            case paramType if paramType.stringParam.isDefined =>
              println(s"$key: ${paramType.stringParam.get}")
            case paramType if paramType.permutationParam.isDefined =>
              println(s"$key: ${paramType.permutationParam.get}")
            case _ =>
              println(s"$key: Unknown or empty parameter type")
          }
        }
      }
      */
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
              //values += paramType.permutationParam.get.value.toString
              values += paramType.permutationParam.get.values.mkString(", ")
          }
        }
      }

      // Convert the ArrayBuffer to Array if necessary
      val headerArray: Array[String] = headers.toArray
      val valuesArray: Array[String] = values.toArray

      val sample: Sample = this.computeSample(headerArray, valuesArray)
      println(sample)


      // Creating comma-separated strings
      val headerString = headerArray.mkString(",")
      val valuesString = valuesArray.mkString(",")

      // Now you have your header and values as comma-separated strings
      println(s"Header: $headerString")
      println(s"Values: $valuesString")

/*

                // execute
                val result = execute(
                  rise.core.substitute.natsInExpr(parametersValuesMap.toMap[Nat, Nat], e),
                  tuner.hostCode,
                  tuner.timeouts,
                  tuner.executionIterations,
                  tuner.speedupFactor,
                  tuner.runtimeStatistic
                )
                val totalTime = Some(TimeSpan.inMilliseconds(
                  (System.currentTimeMillis() - totalStart).toDouble)
                )
                Sample(
                  parameters = parametersValuesMap.map(elem => (elem._1.toString, ClassicParameter(toInt(elem._2)))),
                  runtime = result.runtime,
                  timestamp = System.currentTimeMillis() - start,
                  tuningTimes = TuningTimes(
                    totalTime, result.codegenTime, result.compilationTime, result.executionTime)
                )

*/
      val metrics: Seq[Metric] = Seq(
        Metric(Seq(sample.runtime match {
          case Right(timeSpan) => timeSpan.value.toDouble // Assuming TimeSpan has a `value` that can be converted to Double
          case Left(_) => 0.0 // Or some other default/error value
        })),
        Metric(Seq(sample.tuningTimes.codegen.getOrElse(util.TimeSpan(0, util.Time.Millisecond)).value.toDouble)),
        Metric(Seq(sample.tuningTimes.compilation.getOrElse(util.TimeSpan(0, util.Time.Millisecond)).value.toDouble)),
        Metric(Seq(sample.tuningTimes.execution.getOrElse(util.TimeSpan(0, util.Time.Millisecond)).value.toDouble))
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
    val parameters = collectParameters(e)

    // inject input sizes into constraints
    val inputs = getInputs(e)
    val inputMap = (inputs zip tuner.inputSizes).toMap
    val constraints: Set[rise.autotune.constraints.Constraint] = collectConstraints(e, parameters)
      .map(constraint => constraint.substitute(inputMap.asInstanceOf[Map[ArithExpr, ArithExpr]]))
    
    TunerControl.constraints = constraints

    if (tuner.saveToFile) {
      ("mkdir -p " + tuner.output !!)
      //      ("mkdir -p " + tuner.output + "/" + tuner.name + "_hm" !!)
      //      ("mkdir -p " + tuner.output + "/" + "log" !!)
    }

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

    //    println("parameters: \n" + parameters)
    //    println("constraints: \n" + constraints)

    // compute function value as result for hypermapper
    /*val computeGRPCSample: (Array[TuningParameterValues]) => Sample = (params) => {
      ClassicParameter()
    }*/
    /*
    val computeSample: (Array[String], Array[String]) => Sample = (header, parametersValues) => {

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

                // execute
                val result = execute(
                  rise.core.substitute.natsInExpr(parametersValuesMap.toMap[Nat, Nat], e),
                  tuner.hostCode,
                  tuner.timeouts,
                  tuner.executionIterations,
                  tuner.speedupFactor,
                  tuner.runtimeStatistic
                )
                val totalTime = Some(TimeSpan.inMilliseconds(
                  (System.currentTimeMillis() - totalStart).toDouble)
                )
                Sample(
                  parameters = parametersValuesMap.map(elem => (elem._1.toString, ClassicParameter(toInt(elem._2)))),
                  runtime = result.runtime,
                  timestamp = System.currentTimeMillis() - start,
                  tuningTimes = TuningTimes(
                    totalTime, result.codegenTime, result.compilationTime, result.executionTime)
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
    */

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

    //    println("configFile: " + configFile)

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

    // Instantiate your service implementation
    //val service: HttpRequest => Future[HttpResponse] =
    //  ConfigurationServiceHandler.partial(new ConfigurationServiceImpl)

    // Combine the service handlers as needed
    //val handler: HttpRequest => Future[HttpResponse] = ServiceHandler.concatOrNotFound(service)

    // Bind the service to a port
    val bindingFuture = Http().newServerAt("localhost", 50051).bind(handler)
    println("Server started at localhost:50051")

    // Block until the stop condition is met
    Await.result(ServerControl.stopFuture, Duration.Inf)
    
    // Add shutdown hook, cleanup resources, etc.
    // check if config file exists
    /*
    assert(os.isFile(configFile))

    val hypermapper = os.proc(tuner.tunerPython, tuner.tunerRoot + "/" + tuner.tunerPath, configFile).spawn()

    var i = 1
    // main tuning loop
    var samples = new ListBuffer[Sample]()
    var done = false
    while (hypermapper.isAlive() && !done) {
      hypermapper.stdout.readLine() match {
        case null =>
          done = true
        //          println("End of HyperMapper -- error")
        case "End of HyperMapper" =>
          done = true
        //          println("End of HyperMapper -- done")
        case "Best point found:" =>
          val headers = hypermapper.stdout.readLine()
          val values = hypermapper.stdout.readLine()
          hypermapper.stdout.readLine() // consume empty line
        //          println(s"Best point found\nHeaders: ${headers}Values: $values")
        case request if request.contains("warning") =>
          println(s"[Hypermapper] $request")
        case request if request.contains("Request") =>
          //          println(s"Request: $request")
          val numberOfEvalRequests = request.split(" ")(1).toInt
          // read in header
          val header = hypermapper.stdout.readLine().split(",").map(x => x.trim())
          // start forming response
          var response = tuner.feasibility match {
            case true => s"${header.mkString(",")},runtime,Valid\n"
            case false => s"${header.mkString(",")},runtime\n"
          }

          //          var response = s"${header.mkString(",")},runtime,Valid\n"

          for (_ <- Range(0, numberOfEvalRequests)) {
            // read in parameters values
            val parametersValues = hypermapper.stdout.readLine().split(",").map(x => x.trim())
            // compute sample (including function value aka runtime)
            print("[" + i.toString + "/" + numberOfEvalRequests + "] : ")
            val sample = computeSample(header, parametersValues)
            println(sample.runtime)
            //            println(sample)
            //            println()
            i += 1
            // append sample to Samples
            samples += sample
            // append response
            sample.runtime match {
              case Left(value) =>
                // make sure to response int values

                // check output mode
                val runtime: String = tuner.failureMode match {
                  case `-1` => "-1"
                  case IntMax => "2147483647"
                }

                val add = tuner.feasibility match {
                  case true => {
                    s"${
                      parametersValues.map(x => {
                        try {
                          x.toFloat.toInt.toString
                        } catch {
                          case e: Throwable => x
                        }
                      }).mkString(",")
                    },${
                      runtime
                    },False\n"
                  }
                  case false => s"${
                    parametersValues.map(x => {
                      try {
                        x.toFloat.toInt.toString
                      } catch {
                        case e: Throwable => x
                      }
                    }).mkString(",")
                  },${
                    runtime
                  }\n"
                }

                response += add

              case Right(value) =>

                // make sure to response int values
                val add = tuner.feasibility match {
                  case true =>
                    s"${
                      parametersValues.map(x => {
                        try {
                          x.toFloat.toInt.toString
                        } catch {
                          case e: Throwable => x
                        }
                      }).mkString(",")
                    },${
                      value.value
                    },True\n"

                  case false =>
                    s"${
                      parametersValues.map(x => {
                        try {
                          x.toFloat.toInt.toString
                        } catch {
                          case e: Throwable => x
                        }
                      }).mkString(",")
                    },${
                      value.value
                    }\n"

                }
                response += add
            }
          }

          // send response to Hypermapper
          hypermapper.stdin.write(response)
          hypermapper.stdin.flush()
        case message => println("message: " + message)
      }
    }

    */
    val samples = ListBuffer[Sample]()
    val tuningResult = TuningResult(samples.toSeq, tuner)
    //saveTuningResult(tuningResult)
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

  // todo adjust this
  //  def applyBest(e: Expr, samples: Seq[Sample]): Expr = {
  //    val best = getBest(samples)
  //    best match {
  //      case Some(_) => rise.core.substitute.natsInExpr(best.get.parameters.toMap[Nat, Nat], e)
  //      case None => e
  //    }
  //  }

  // todo adjust this
  //  def applySample(e: Expr, sample: Sample): Expr = {
  //    rise.core.substitute.natsInExpr(sample.parameters.toMap[Nat, Nat], e)
  //  }

  def getDuration(tuningResult: TuningResult): TimeSpan[Time.ms] = {
    val duration = tuningResult.samples.apply(tuningResult.samples.size).timestamp -
      tuningResult.samples.apply(0).timestamp

    TimeSpan.inMilliseconds(duration.toDouble)
  }

  def getSamples(tuningResult: TuningResult): Int = {
    tuningResult.samples.size
  }

  def saveTuningResult(tuningResult: TuningResult) = {
    val tuner = tuningResult.tuner

    // save results to file
    if (tuner.saveToFile) {

      // get unique filepath
      val path = tuner.output + "/" + tuner.name + ".csv"
      val file = new File(path)
      val timeAppendix = if (file.exists()) {
        "_" + System.currentTimeMillis().toString
      } else {
        ""
      }

      //      // save samples to file
      //      saveSamples(
      //        tuner.output + "/" + tuner.name + "/" + tuner.name + timeAppendix + ".csv",
      //        tuningResult
      //      )

      // save hm output file
      //      ("mv " + tuner.name + "_output_samples.csv" + " " +
      //        tuner.output + "/" + tuner.name + "_hm/" + tuner.name + timeAppendix + "_hm" + ".csv" !!)

      // save hm output file
      ("mv " + tuner.name + "_output_samples.csv" + " " +
        tuner.output + "/" + tuner.name + timeAppendix + ".csv" !!)

      // save logfile and configfile
      if (tuner.configFile.isDefined) {

        // parse logfile name from json or use default name
        val logfile = try {
          parseFromJson(tuner.configFile.get, "log_file")
        } catch {
          case e: NoSuchElementException => "tuner_logfile.log"
        }

        //         move logfile to output folder
        //        ("mv " + logfile + " " +
        //          tuner.output + "/log/" + logfile.substring(0, logfile.length - 4) + timeAppendix + ".log" !!)

        // copy config file to output folder
        //        ("cp " + tuner.configFile.get + " " + tuner.output !!)
      } else {

        // move logfile to output folder
        //        ("mv " + tuner.name + ".log" + " " +
        //          tuner.output + "/log/" + tuner.name + timeAppendix + ".log" !!) // get unique filename

      }

      // create plots
      //      plotTuning(tuner)

    } else {
      // remove logfile and generated config file
      if (tuner.configFile.isDefined) {

        val logfile = try {
          parseFromJson(tuner.configFile.get, "log_file")
        } catch {
          case e: NoSuchElementException => "tuner_logfile.log"
        }

        ("rm " + logfile !!)

      } else {

        ("rm " + tuner.name + ".log" !!)
        ("rm " + "/tmp/" + tuner.name + ".json" !!)

      }
    }

    // todo save meta

  }

  // write tuning results into csv file
  def saveSamples(path: String, tuningResult: TuningResult): String = {
    // create unique filepath
    val file = new File(path)
    val uniqueFilepath = if (file.exists()) {
      val timeAppendix = System.currentTimeMillis().toString
      path.substring(0, path.length - 4) + "_" + timeAppendix + ".csv"
    } else {
      path
    }

    // write header
    var header = tuningResult.samples.head.parameters.map(elem => elem._1).mkString("", ",", ",")

    header += "runtime" + ","
    header += "timestamp" + ","
    header += "total" + ","
    header += "code generation" + ","
    header += "compilation" + ","
    header += "execution" + ","
    header += "\n"

    // write content
    var content = ""
    tuningResult.samples.foreach(sample => {

      // write parameter
      sample.parameters.foreach(param => {
        param._2 match {
          case ClassicParameter(value) => content += value.toString + ","
          case PermutationParameter(value) => content += value.mkString("\"(", ",", ")\"") + ","
        }
      })

      // write runtime
      sample.runtime match {
        case Right(runtime) => content += runtime.value.toString + ","
        case Left(error) =>

          val errorMessage = error.message match {
            case None => ""
            case Some(value) => ": " + value
          }

          content += error.errorLevel.toString + errorMessage + ","

      }

      // write timestamp
      content += sample.timestamp.toString + ","

      sample.tuningTimes.total match {
        case Some(value) => content += value.value.toString + ","
        case None => content += "-1" + ","
      }
      sample.tuningTimes.codegen match {
        case Some(value) => content += value.value.toString + ","
        case None => content += "-1" + ","
      }
      sample.tuningTimes.compilation match {
        case Some(value) => content += value.value.toString + ","
        case None => content += "-1" + ","
      }
      sample.tuningTimes.execution match {
        case Some(value) => content += value.value.toString + ","
        case None => content += "-1" + ","
      }

      // finish line
      content += "\n"
    })

    writeToPath(uniqueFilepath, header + content)


    uniqueFilepath
  }

  def plotTuning(tuner: Tuner) = {

    // get config file
    val configFile: String = tuner.configFile match {
      case Some(value) => value
      case None => tuner.output + "/" + tuner.name + ".json"
    }

    // plot results using hypermapper



    (s"${tuner.tunerPython} ${tuner.tunerRoot}/${tuner.tunerPath.split("/")(0)}/plot/plot_optimization_results.py " +
      "-j " + configFile + " " +
      "-i " + tuner.output + "/" + tuner.name + "_hm" + " " +
      "-o" + tuner.output + "/" + tuner.name + ".pdf" + " " +
      "-log --y_label \"Log Runtime(ms)\"" !!)
  }

  // todo finish implementation
  def saveMeta(path: String, tuningResult: TuningResult, tuner: Tuner): String = {

    // todo save tuner information to file

    // todo collect statistics from tuningResult
    val duration = (tuningResult.samples.apply(tuningResult.samples.size).timestamp - tuningResult.samples.apply(0).timestamp)
    val samples = tuningResult.samples.size

    // save statistics to csv file (don't overwrite -> append)

    // return unique filename
    ""
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
