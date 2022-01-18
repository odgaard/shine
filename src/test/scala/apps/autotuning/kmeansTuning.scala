package apps.autotuning

import apps.autotuning
import arithexpr.arithmetic.RangeMul
import rise.autotune
import rise.autotune.{HostCode, Median, Minimum, Timeouts, Tuner, tuningParam, wrapOclRun}
import rise.core.DSL.Type._
import rise.core._
import rise.core.types.{Nat, _}
import shine.OpenCL.{GlobalSize, LocalSize}

class kmeansTuning extends test_util.Tests {

  val kmeans: Expr =
    tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
      tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
        wrapOclRun(LocalSize(ls0), GlobalSize(gs0))(apps.kmeans.kmeansOcl)))

  // scalastyle:off
  val init: (Int, Int, Int) => String = (p, c, f) => {
    s"""
       |  const int P = ${p};
       |  const int C = ${c};
       |  const int F = ${f};
       |  srand(time(NULL));
       |  Buffer features = createBuffer(ctx, F * P * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer clusters = createBuffer(ctx, C * F * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer output = createBuffer(ctx, P * sizeof(float), HOST_READ | DEVICE_WRITE);
       |
       |  float* in_features = hostBufferSync(ctx, features, F * P * sizeof(float), HOST_WRITE);
       |  for (int i = 0; i < F * P ; i++) {
       |    in_features[i] = (float)(rand() % 100);
       |  }
       |
       |  float* in_clusters = hostBufferSync(ctx, clusters, C * F * sizeof(float), HOST_WRITE);
       |    for (int i = 0; i < F * P ; i++) {
       |      in_clusters[i] = (float)(rand() % 100);
       |    }
       |
       |  deviceBufferSync(ctx, features, F * P * sizeof(float), DEVICE_READ);
       |  deviceBufferSync(ctx, clusters, C * F * sizeof(float), DEVICE_READ);
       |""".stripMargin
  }
  val compute =
    s"""
       |    fun_init_run(ctx, output, P, C, F, features, clusters);
       |""".stripMargin

  val finish =
    s"""
       |  // could add error checking
       |  destroyBuffer(ctx, features);
       |  destroyBuffer(ctx, clusters);
       |  destroyBuffer(ctx, output);
       |""".stripMargin
  // scalastyle:on


  test("execute kmeans") {
    val params: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (32: Nat),
      TuningParameter("gs0") -> (1024: Nat)
    )

    val kmeans_replaced = rise.core.substitute.natsInExpr(params, kmeans)

    val result = autotune.execution.execute(
      expression = kmeans_replaced,
      hostCode = HostCode(init(1024, 5, 34), compute, finish),
      timeouts = Timeouts(10000, 10000, 100000),
      executionIterations = 10,
      speedupFactor = 100,
      execution = Median
    )

    println("result: " + result)
  }

  ignore("search kmeans with generated config file") {

    val tuner = Tuner(
      hostCode = HostCode(init(1024, 5, 34), compute, finish),
      inputSizes = Seq(1024, 5, 34),
      samples = 10,
      name = "kmeans",
      output = "autotuning/kmeans",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      runtimeStatistic = Median,
      speedupFactor = 100,
      configFile = None,
      hmConstraints = false,
    )

    val tuningResult = autotune.search(tuner)(kmeans)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: \n" + bestSample)
    println("runtime: \n" + bestSample.get.runtime)
  }

  test("search kmeans with manual config file") {

    val tuner = Tuner(
      hostCode = HostCode(init(1024, 5, 34), compute, finish),
      inputSizes = Seq(1024, 5, 34),
      samples = 20,
      name = "kmeans",
      output = "autotuning/kmeans",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = Some("autotuning/config/kmeans/kmeans_exhaustive.json"),
      //      configFile = None,
      hmConstraints = true,
      runtimeStatistic = Minimum
    )

    val tuningResult = autotune.search(tuner)(kmeans)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: \n" + bestSample)
    println("runtime: \n" + bestSample.get.runtime)
  }

  def runExperiments(configFiles: Seq[String], iterations: Int) = {
    for(i <- 1 to iterations) {
      configFiles.foreach(runTuning)
    }
  }

  def runTuning(configFile: String) = {
    val version = autotuning.parseName(configFile)

    val tuner = Tuner(
      hostCode = HostCode(init(1024, 5, 34), compute, finish),
      inputSizes = Seq(1024, 5, 34),
      samples = 20, // defined by config file
      name = version,
      output = s"autotuning/kmeans/${version}",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = Some(configFile),
      hmConstraints = true,
      saveToFile = true
    )
    autotune.search(tuner)(kmeans)
  }

  test("run kmeans autotuning"){

    val configs = Seq(
        "autotuning/config/kmeans/kmeans_rs_cot.json",
        "autotuning/config/kmeans/kmeans_rs_emb.json",
        "autotuning/config/kmeans/kmeans_ls_cot.json",
        "autotuning/config/kmeans/kmeans_atf_emb.json"
      )

    runExperiments(configFiles = configs, iterations = 3)
  }

}