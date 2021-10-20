package shine.GAP8

import rise.GAP8.DSL.{gap8Run, hwce}
import rise.GAP8.primitives.gap8hwConv3x3
import rise.core.DSL.HighLevelConstructs._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types._
import rise.elevate.Rise
import shine.GAP8

class hwce extends test_util.Tests {

  private def checkHwceCall(code: String, filterSize: String) = {
    assert(
      ("HWCE_ProcessOneTile" + filterSize).r.findAllIn(code).length >= 1
    )
  }



  ignore("Minimal example") {
    val w: Nat = 6
    val h: Nat = 6

    val fW: Nat = 3
    val fH: Nat = 3
    /**
      * HWCE_ProcessOneTile3x3_MultiOut(e1, output, NULL, NULL, e2, 0, n, m, 0x7)
      * */
    //TODO: Pad filter with one 0 or do that in data prep step on backend (codegen)
    val expr: ToBeTyped[Rise] = {
      fun((w`.`h`.`i16) ->: (fW`.`fH`.`i16) ->: ((w - 2)`.`(h - 2)`.`i16))((in, filter) =>
        in |>
          slide2D(3, 1) |>
          mapSeq(mapSeq(fun(sub => {
            zip(sub |> join)(filter |> join) |>
              map(fun(x => fst(x) * snd(x))) |>
              reduceSeq(add)(li16(0))
          })))
      )
    }

    println(util.gen.gap8.function("cluster_core_task").asStringFromExpr(expr))
  }

  test("Minimal example 2") {
    val w: Nat = 6
    val h: Nat = 6

    /**
      * HWCE_ProcessOneTile3x3_MultiOut(e1, output, NULL, NULL, e2, 0, n, m, 0x7)
      * */
    //TODO: Pad filter with one 0 or do that in data prep step on backend (codegen)
    val expr: ToBeTyped[Rise] = {
      fun((w`.`h`.`i16) ->: (3`.`3`.`i16) ->: ((w - 2)`.`(h - 2)`.`i16))((in, filter) =>
        gap8Run(8)(
          gap8hwConv3x3(0)(in)(filter)
        )
      )
    }

    //println(expr.toExpr)
    //println(expr.toExpr.t)

    val hostedModule = util.gen.gap8.hosted.fromExpr(expr)
    val code = GAP8.Module.translateToString(hostedModule)

    checkHwceCall(code, "3x3")

    println(code)
  }

  ignore("Hwce RISE primitive") {
    val n: Nat = 6
    val m: Nat = 6
    val fW: Nat = 3
    val fH: Nat = 3
    val oW: Nat = 4
    val oH: Nat = 4

    val expr: ToBeTyped[Rise] =
      fun((n`.`m`.`i16) ->: (fW`.`fH`.`i16) ->: (oW`.`oH`.`i16))((in, filter) =>
        hwce()(in, filter)
      )
  }
}