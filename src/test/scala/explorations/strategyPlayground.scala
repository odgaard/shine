package explorations


import apps.tvmGemm.{innermost, outermost}
import arithexpr.arithmetic.RangeMul
import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.heuristic_search.util.Solution
import elevate.macros.RuleMacro.rule
import exploration.neighborhoods.NTreeChildrenChoice
import exploration.runner.{AutoTuningExecutor, CExecutor}
import exploration.{ExecutorConfig, MetaheuristicConfig, NeighborhoodConfig}
import rise.autotune
import rise.autotune.{HostCode, Tuner, tuningParam, wrapOclRun}
import rise.core.DSL.{fun, lf32}
import rise.core.primitives._
import rise.core.types.DataType.{ArrayType, f32}
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering._
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.algorithmic.reorder
import rise.elevate.strategies.lowering._
import rise.elevate.strategies.normalForm._
import rise.elevate.strategies.predicate._
import rise.elevate.strategies.tiling._
import rise.elevate.strategies.traversal._
import rise.elevate.{NormalizedThen, Rise, tunable}
import rise.core._
import rise.core.DSL._
import rise.core.primitives.{let => _, _}
import rise.core.DSL.Type._
import rise.core.types._
import rise.core.types.DataType._
import rise.openCL.DSL._
import rise.openCL.primitives.oclReduceSeq
import shine.OpenCL.{GlobalSize, LocalSize}

class strategyPlayground extends test_util.Tests {

  // define mm
  val N = 1024

  val mm: Rise =
    fun(ArrayType(N, ArrayType(N, f32)))(a =>
      fun(ArrayType(N, ArrayType(N, f32)))(b =>
        a |> map(fun(ak =>
          transpose(b) |> map(fun(bk =>
            zip(ak)(bk) |>
              map(fun(x => fst(x) * snd(x))) |>
              reduce(add)(lf32(0.0f))
          ))
        ))
      ))

  test("splitJoin inverted") {

    val splitJoinTuning: Strategy[Rise] = tunable(splitJoin)

    val mm_split = (splitJoinTuning `@` topDown[Rise]).apply(mm).get

    val mm_plain = (joinSplit `@` topDown[Rise]).apply(mm_split).get

    println("mm: \n" + mm)
    println("mm_split: \n" + mm_split)
    println("mm_plain: \n" + mm_plain)

  }

}
