package apps.autotuning

import apps.mm.mmNVIDIAWithParams
import arithexpr.arithmetic.{RangeAdd, RangeMul}
import rise.core._
import rise.core.types._
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.autotune
import shine.OpenCL.{GlobalSize, LocalSize}
import rise.autotune._

class mmTuning extends test_util.Tests {

  val mmTuning: ToBeTyped[Expr] =
    tuningParam("v3", RangeAdd(1, 1024, 1), (v3: Nat) =>
      tuningParam("v4", RangeAdd(1, 1024, 1), (v4: Nat) =>
        tuningParam("v5", RangeAdd(1, 1024, 1), (v5: Nat) =>
          tuningParam("v6", RangeAdd(1, 1024, 1), (v6: Nat) =>
            tuningParam("v7", RangeAdd(1, 1024, 1), (v7: Nat) =>
              tuningParam("v8", RangeAdd(1, 1024, 1), (v8: Nat) =>
                mmNVIDIAWithParams(v3, v4, v5, v6, v7, v8)
              ))))))

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

  test("mm example config") {
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
  }

  // standard hypermapper
  test("mm tuning 128") {
    val mm: Expr =
      tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
        tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
          tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
            tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(mmTuning)))))

    val tuner = Tuner(
      hostCode = HostCode(init(64, 128, 128), compute, finish),
      inputSizes = Seq(64, 128, 128),
      samples = 100,
      name = "rs_cot_128",
      output = "autotuning/mm_128",
      timeouts = Timeouts(5000, 5000, 1000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = None,
      hmConstraints = false
    )

    val tuningResult = autotune.search(tuner)(mm)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: " + bestSample)
  }

  test("mm tuning 1024 with generated config file") {
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
      hmConstraints = false,
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

  ignore("mm tuning 1024 hierarchical") {
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
      samples = 20,
      name = "rs_cot_1024",
      output = "autotuning/mm_1024",
      timeouts = Timeouts(1000, 1000, 1000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = Some("autotuning/config/mm/rs_cot_1024.json"),
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
}