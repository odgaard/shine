package apps.autotuning

import apps.mm.mmNVIDIAWithParams
import arithexpr.arithmetic.{RangeAdd, RangeMul}
import rise.autotune
import rise.autotune._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.types._
import shine.OpenCL.{GlobalSize, LocalSize}

import scala.language.postfixOps

// todo update this to tensor tuning
class mmTensorTuning extends test_util.Tests {

  val mmTuning: ToBeTyped[Expr] =
    tuningParam("v3", RangeAdd(1, 1024, 1), (v3: Nat) =>
      tuningParam("v4", RangeAdd(1, 1024, 1), (v4: Nat) =>
        tuningParam("v5", RangeAdd(1, 1024, 1), (v5: Nat) =>
          tuningParam("v6", RangeAdd(1, 1024, 1), (v6: Nat) =>
            tuningParam("v7", RangeAdd(1, 1024, 1), (v7: Nat) =>
              tuningParam("v8", RangeAdd(1, 1024, 1), (v8: Nat) =>
                mmNVIDIAWithParams(v3, v4, v5, v6, v7, v8)
              ))))))

  val mmTuning_4096: ToBeTyped[Expr] =
    tuningParam("v3", RangeAdd(1, 4096, 1), (v3: Nat) =>
      tuningParam("v4", RangeAdd(1, 4096, 1), (v4: Nat) =>
        tuningParam("v5", RangeAdd(1, 4096, 1), (v5: Nat) =>
          tuningParam("v6", RangeAdd(1, 4096, 1), (v6: Nat) =>
            tuningParam("v7", RangeAdd(1, 4096, 1), (v7: Nat) =>
              tuningParam("v8", RangeAdd(1, 4096, 1), (v8: Nat) =>
                mmNVIDIAWithParams(v3, v4, v5, v6, v7, v8)
              ))))))


  val mm: Expr =
    tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
      tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
        tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
          tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(mmTuning)
          ))))

  val mm_4096: Expr =
    tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
      tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
        tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
          tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(mmTuning_4096)
          ))))


  // scalastyle:off
  val init: (Int, Int, Int) => String = (N, M, O) => {
    s"""
       |const int N = ${N};
       |const int M = ${M};
       |const int O = ${O};
       |
       |srand(time(NULL));
       |
       |Buffer inputA = createBuffer(ctx, N * M * sizeof(float), HOST_WRITE | DEVICE_READ);
       |Buffer inputB = createBuffer(ctx, M * O * sizeof(float), HOST_WRITE | DEVICE_READ);
       |Buffer outputC = createBuffer(ctx, N * O *  sizeof(float), HOST_READ | DEVICE_WRITE);
       |
       |float* inA = hostBufferSync(ctx, inputA, N * M * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < N * M ; i++) {
       |  inA[i] = (float)(rand());
       |}
       |
       |float* inB = hostBufferSync(ctx, inputB, M * O * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < M * O; i++) {
       |  inB[i] = (float)(rand());
       |}
       |
       |""".stripMargin
  }

  val compute =
    s"""
       |fun_run(ctx, &fun, outputC, M, N, O, inputA, inputB);
       |""".stripMargin

  val finish =
    s"""
       |// TODO: could check output here
       |
       |destroyBuffer(ctx, inputA);
       |destroyBuffer(ctx, inputB);
       |destroyBuffer(ctx, outputC);
       |""".stripMargin
  // scalastyle:on

  ignore("mm example config") {
    val mm: Expr =
      tuningParam("ls0", (ls0: Nat) => tuningParam("ls1", (ls1: Nat) =>
        tuningParam("gs0", (gs0: Nat) => tuningParam("gs1", (gs1: Nat) =>
          wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(mmTuning)))))

    val params0:Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (16: Nat),
      TuningParameter("ls1") -> (16: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (256: Nat),
      TuningParameter("v3") -> (4: Nat),
      TuningParameter("v4") -> (1: Nat),
      TuningParameter("v5") -> (4: Nat),
      TuningParameter("v6") -> (64: Nat),
      TuningParameter("v7") -> (256: Nat),
      TuningParameter("v8") -> (32: Nat)
    )

    val mm0 = rise.core.substitute.natsInExpr(params0, mm)
    val result0 = autotune.execution.execute(
      expression = mm0,
      hostCode = HostCode(init(1024, 1024, 1024), compute, finish),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 100,
      speedupFactor = 100,
      execution = Median
    )
    println("result0: " + result0.runtime)
    assert(result0.runtime.isRight)

    val params1:Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (4: Nat),
      TuningParameter("ls1") -> (32: Nat),
      TuningParameter("gs0") -> (128: Nat),
      TuningParameter("gs1") -> (64: Nat),
      TuningParameter("v3") -> (4: Nat),
      TuningParameter("v4") -> (1: Nat),
      TuningParameter("v5") -> (32: Nat),
      TuningParameter("v6") -> (128: Nat),
      TuningParameter("v7") -> (8: Nat),
      TuningParameter("v8") -> (128: Nat)
    )

    val mm1 = rise.core.substitute.natsInExpr(params1, mm)
    val result1 = autotune.execution.execute(
      expression = mm1,
      hostCode = HostCode(init(1024, 1024, 1024), compute, finish),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 100,
      speedupFactor = 100,
      execution = Median
    )
    println("result1: " + result1.runtime)
    assert(result1.runtime.isRight)

    val params2:Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (8: Nat),
      TuningParameter("ls1") -> (128: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (1024: Nat),
      TuningParameter("v3") -> (4: Nat),
      TuningParameter("v4") -> (1: Nat),
      TuningParameter("v5") -> (32: Nat),
      TuningParameter("v6") -> (256: Nat),
      TuningParameter("v7") -> (64: Nat),
      TuningParameter("v8") -> (32: Nat)
    )

    val mm2 = rise.core.substitute.natsInExpr(params2, mm)
    val result2 = autotune.execution.execute(
      expression = mm2,
      hostCode = HostCode(init(1024, 1024, 1024), compute, finish),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 100,
      speedupFactor = 100,
      execution = Minimum
    )

    println("result2: " + result2.runtime)
    assert(result2.runtime.isRight)
  }

  // standard hypermapper
  ignore("mm tuning 128") {
    val mm: Expr =
      tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
        tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
          tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
            tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(mmTuning)))))

    val tuner = Tuner(
      hostCode = HostCode(init(64, 128, 128), compute, finish),
      inputSizes = Seq(64, 128, 128),
      name = "rs_cot_128",
      output = "autotuning/mm_128",
      timeouts = Timeouts(5000, 5000, 1000),
      configFile = None,
      hmConstraints = false
    )

    val tuningResult = autotune.search(tuner)(mm)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: " + bestSample)
  }

  ignore("mm tuning 1024 with generated config file") {
    val mm: Expr =
      tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
        tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
          tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
            tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(mmTuning)
            ))))

    val tuner = Tuner(
      hostCode = HostCode(init(1024, 1024, 1024), compute, finish),
      inputSizes = Seq(1024, 1024, 1024),
      samples = 10,
      name = "rs_cot_1024",
      output = "autotuning/mm_1024",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = None, // we don't inject usage of local memory as constraints - many configs fail
      hmConstraints = true,
      runtimeStatistic = Minimum
    )

    val tuningResult = autotune.search(tuner)(mm)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: \n" + bestSample)
  }

  // hierarchical hypermapper
  ignore("mm tuning 128 hierarchical") {
    val mm: Expr =
      tuningParam("ls0", RangeMul(1, 128, 2), (ls0: Nat) =>
        tuningParam("ls1", RangeMul(1, 64, 2), (ls1: Nat) =>
          tuningParam("gs0", RangeMul(1, 128, 2), (gs0: Nat) =>
            tuningParam("gs1", RangeMul(1, 64, 2), (gs1: Nat) =>
              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(mmTuning)
            ))))

    val tuner = Tuner(
      hostCode = HostCode(init(64, 128, 128), compute, finish),
      inputSizes = Seq(64, 128, 128),
      samples = 100,
      name = "rs_cot_128",
      output = "autotuning/mm_128",
      timeouts = Timeouts(5000, 5000, 1000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = Some("autotuning/config/mm/rs_cot_128.json"),
      hmConstraints = true
    )

    val tuningResult = autotune.search(tuner)(mm)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: " + bestSample)
    println("runtime: " + bestSample.get.runtime)
  }

  // we do not support hierarchical hypermapper
  ignore("mm tuning 1024 with generated config file hierarchical") {
    val mm: Expr =
      tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
        tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
          tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
            tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(mmTuning)
            ))))

    val tuner = Tuner(
      hostCode = HostCode(init(1024, 1024, 1024), compute, finish),
      inputSizes = Seq(1024, 1024, 1024),
      samples = 10,
      name = "rs_cot_1024",
      output = "autotuning/mm_1024",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = None, // we don't inject usage of local memory as constraints - many configs fail
      hmConstraints = true,
      runtimeStatistic = Minimum
    )

    val tuningResult = autotune.search(tuner)(mm)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: \n" + bestSample)
    println("runtime: \n" + bestSample.get.runtime)
  }

  // we do not support hierarchical hypermapper
  ignore("mm tuning 4096 with generated config file hierarchical") {
    val mm: Expr =
      tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
        tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
          tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
            tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(mmTuning_4096)
            ))))

    val tuner = Tuner(
      hostCode = HostCode(init(4096, 4096, 4096), compute, finish),
      inputSizes = Seq(4096, 4096, 4096),
      samples = 10,
      name = "rs_cot_4096",
      output = "autotuning/mm_4096",
      timeouts = Timeouts(codegenerationTimeout = 10000, compilationTimeout = 10000, executionTimeout = 20000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = None, // we don't inject usage of local memory as constraints - many configs fail
      hmConstraints = true,
      runtimeStatistic = Minimum
    )

    val tuningResult = autotune.search(tuner)(mm)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: \n" + bestSample)
    println("runtime: \n" + bestSample.get.runtime)
  }


  ignore("execute expert configuration"){
    // execute config with "expert parameter configuration"
    val mm: Expr =
      tuningParam("ls0", (ls0: Nat) => tuningParam("ls1", (ls1: Nat) =>
        tuningParam("gs0", (gs0: Nat) => tuningParam("gs1", (gs1: Nat) =>
          wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(mmTuning)))))

    // expert config for 128x64 * 128x128
    val params0:Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (32: Nat),
      TuningParameter("ls1") -> (8: Nat),
      TuningParameter("gs0") -> (256: Nat),
      TuningParameter("gs1") -> (128: Nat),
      TuningParameter("v3") -> (4: Nat),
      TuningParameter("v4") -> (8: Nat),
      TuningParameter("v5") -> (64: Nat), // tile-width A
      TuningParameter("v6") -> (128: Nat), // divides v8 x v5
      TuningParameter("v7") -> (128: Nat), // tile-width B
      TuningParameter("v8") -> (16: Nat) // tile-height A,B
    )

    val mm0 = rise.core.substitute.natsInExpr(params0, mm)
    val result0 = autotune.execution.execute(
      expression = mm0,
      hostCode = HostCode(init(1024, 1024, 1024), compute, finish),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 100,
      speedupFactor = 100,
      execution = Median
    )
    println("result0: " + result0.runtime)
    //    assert(result0.runtime.isRight)

    // expert config for 128x64 * 128x128
    val params1:Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (16: Nat),
      TuningParameter("ls1") -> (16: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (1024: Nat),
      TuningParameter("v3") -> (1: Nat),
      TuningParameter("v4") -> (1: Nat),
      TuningParameter("v5") -> (32: Nat), // tile-width A
      TuningParameter("v6") -> (32: Nat), // divides v8 x v5
      TuningParameter("v7") -> (32: Nat), // tile-width B
      TuningParameter("v8") -> (32: Nat) // tile-height A,B
    )

    val mm1 = rise.core.substitute.natsInExpr(params1, mm)
    val result1 = autotune.execution.execute(
      expression = mm1,
      hostCode = HostCode(init(1024, 1024, 1024), compute, finish),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 100,
      speedupFactor = 100,
      execution = Median
    )
    println("result1: " + result1.runtime)
    assert(result1.runtime.isRight)

  }


//  ignore("tune mm 128"){
//
//    val configs = Seq(
//      "autotuning/config/mm/128/rs_cot_128.json",
//      "autotuning/config/mm/128/rs_emb_128.json",
//      "autotuning/config/mm/128/ls_cot_128.json",
//      "autotuning/config/mm/128/atf_emb_128.json",
//      "autotuning/config/mm/128/borf_cot_128.json",
//      "autotuning/config/mm/128/bogp_cot_128.json"
//    )
//
//    runExperiment(
//      name = "mm_128",
//      configFiles = configs,
//      iterations = 2,
//      "autotuning/mm_128",
//      mm,
//      HostCode(init(128, 128, 128), compute, finish),
//      Seq(128, 128, 128)
//    )
//  }

  ignore("tune mm 128"){
    val inputSize: Int = 128

    val configs = Seq(
      s"autotuning/config/mm/${inputSize.toString}/rs_cot_${inputSize.toString}.json",
      s"autotuning/config/mm/${inputSize.toString}/rs_emb_${inputSize.toString}.json",
      s"autotuning/config/mm/${inputSize.toString}/ls_cot_${inputSize.toString}.json",
      s"autotuning/config/mm/${inputSize.toString}/atf_emb_${inputSize.toString}.json",
      s"autotuning/config/mm/${inputSize.toString}/bogp_cot_${inputSize.toString}.json",
      s"autotuning/config/mm/${inputSize.toString}/bogplog_cot_${inputSize.toString}.json"
    )

    runExperiment(
      name = s"mm_${inputSize}",
      configFiles = configs,
      iterations = 5,
      s"autotuning/mm_${inputSize}",
      mm,
      HostCode(init(inputSize, inputSize, inputSize), compute, finish),
      Seq(inputSize, inputSize, inputSize)
    )
  }

  test("tune mm 1024"){
    val inputSize: Int = 1024

    val configs = Seq(
      s"autotuning/config/mm/${inputSize.toString}/rs_cot_${inputSize.toString}.json",
      s"autotuning/config/mm/${inputSize.toString}/rs_emb_${inputSize.toString}.json",
      s"autotuning/config/mm/${inputSize.toString}/ls_cot_${inputSize.toString}.json",
      s"autotuning/config/mm/${inputSize.toString}/atf_emb_${inputSize.toString}.json",
      s"autotuning/config/mm/${inputSize.toString}/bogp_cot_${inputSize.toString}.json",
      s"autotuning/config/mm/${inputSize.toString}/bogplog_cot_${inputSize.toString}.json"
    )

    runExperiment(
      name = s"mm_${inputSize}",
      configFiles = configs,
      iterations = 10,
//      s"autotuning/mm_${inputSize}",
      s"experiment/results/mm_${inputSize}_log_init",
      mm,
      HostCode(init(inputSize, inputSize, inputSize), compute, finish),
      Seq(inputSize, inputSize, inputSize)
//      true
    )
  }

  ignore("tune mm 4096"){
    val inputSize: Int = 4096

    val configs = Seq(
      s"autotuning/config/mm/${inputSize.toString}/rs_cot_${inputSize.toString}.json",
      s"autotuning/config/mm/${inputSize.toString}/rs_emb_${inputSize.toString}.json",
      s"autotuning/config/mm/${inputSize.toString}/ls_cot_${inputSize.toString}.json",
      s"autotuning/config/mm/${inputSize.toString}/atf_emb_${inputSize.toString}.json",
      s"autotuning/config/mm/${inputSize.toString}/bogp_cot_${inputSize.toString}.json",
      s"autotuning/config/mm/${inputSize.toString}/bogplog_cot_${inputSize.toString}.json"
    )

    runExperiment(
      name = s"mm_${inputSize}",
      configFiles = configs,
      iterations = 2,
      s"autotuning/mm_${inputSize}",
      mm_4096,
      HostCode(init(inputSize, inputSize, inputSize), compute, finish),
      Seq(inputSize, inputSize, inputSize)
    )
  }


}