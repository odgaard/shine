package apps

import rise.GAP8.DSL.gap8Run
import rise.GAP8.primitives.{allocL1, copyToL1, copyToL2, gap8hwConv3x3}
import rise.core.DSL.HighLevelConstructs.{slide2D, zipND}
import rise.core.DSL.Type.TypeConstructors
import rise.core.DSL.Type.`.`
import rise.core.DSL.{ToBeTyped, TypeAnnotationHelper, depFun, foreignFun, fun, let, letf, li16, lu8}
import rise.core.primitives.{add, cast, fst, join, mapSeq, padCst, slide, snd, transpose, zip}
import rise.core.types.DataType.{ArrayType, i16, u8}
import rise.core.types.Nat
import rise.elevate.Rise
import rise.openMP.primitives.mapPar
import shine.GAP8.Module.translateToString

// scalastyle: off
object GAP8HwceDma {
  def main(args: Array[String]): Unit = {
    val gapSqrt = foreignFun("gap_sqrt",
      Seq("a_nInput"),
      """
        | {
        |   uint32_t op  = a_nInput;
        |   uint32_t res = 0;
        |
        |   uint32_t one = 1uL << 30;
        |   while (one > op){
        |     one >>= 2;
        |   }
        |   while (one != 0) {
        |     if (op >= res + one){
        |       op = op - (res + one);
        |       res = res +  2 * one;
        |     }
        |     res >>= 1;
        |     one >>= 2;
        |   }
        |   return res;
        | }
        |""".stripMargin,
      i16 ->: i16
    )

    val size: Nat = 4
    val fixSize: ToBeTyped[Rise] = fun(ArrayType(size, u8) ->: ArrayType(size, u8))(in =>
        gap8Run(8)(
          in |>
            copyToL1 |>
            allocL1 |>
            mapSeq(fun(x => x)) |>
            allocL1 |>
            copyToL2
        )
      )
    //println(translateToString(util.gen.gap8.hosted.fromExpr(fixSize)))

    val varSize: ToBeTyped[Rise] = depFun((sz: Nat) =>
      fun(ArrayType(sz, u8) ->: ArrayType(sz, u8))(in =>
        gap8Run(8)(
          in |>
            copyToL1 |>
            allocL1 |>
            mapSeq(fun(x => x)) |>
            allocL1 |>
            copyToL2
        )))
    //println(translateToString(util.gen.gap8.hosted.fromExpr(varSize)))

    val fixSizeDmaHwce: ToBeTyped[Rise] = fun(
      ArrayType(size, ArrayType(size, i16)) ->:
        ArrayType(3, ArrayType(3, i16)) ->:
        ArrayType(size - 2, ArrayType(size - 2, i16)))((in, filter) =>
      gap8Run(8)(
        in |> copyToL1 |> allocL1 |> letf(innerin =>
          filter |> copyToL1 |> allocL1 |> letf(innerf =>
            gap8hwConv3x3(0)(innerin, innerf) |> allocL1 |> copyToL2
          )
        )
      )
    )
    //println(translateToString(util.gen.gap8.hosted("conv").fromExpr(fixSizeDmaHwce)))

    //Passes
    val theSmallestSlide: ToBeTyped[Rise] = fun(
      ArrayType(5, i16) ->: ArrayType(3, ArrayType(3, i16))
    )(arr =>
      gap8Run(8)(
        arr |> slide(3)(1) |> mapSeq(mapSeq(fun(elem => add(elem)(li16(1)))))
      )
    )
    //printf(translateToString(util.gen.gap8.hosted("theSmallest").fromExpr(theSmallestSlide)))
    translateToString(util.gen.gap8.hosted("theSmallest").fromExpr(theSmallestSlide))
    println("======================================================================")

    val inBetweenSlideExample: ToBeTyped[Rise] = fun(
      ArrayType(5, i16) ->: ArrayType(3, ArrayType(3, i16))
    )(arr =>
      gap8Run(8)(
        arr |> slide(3)(1) |> mapSeq(fun(tile =>
          tile |> letf(tstletf =>
            tstletf |> mapSeq(fun(elem => add(elem)(li16(1))))
          )
        ))
      )
    )
    println(translateToString(util.gen.gap8.hosted("inBetween").fromExpr(inBetweenSlideExample)))
    //translateToString(util.gen.gap8.hosted("inBetween").fromExpr(inBetweenSlideExample))
    println("======================================================================")
    //Does not pass
    val evenSmallerSlideExample: ToBeTyped[Rise] = fun(
      ArrayType(5, i16) ->: ArrayType(3, ArrayType(3, i16))
    )(arr =>
      gap8Run(8)(
        arr |> slide(3)(1) |> mapSeq(fun(tile =>
          tile |> copyToL1 |> allocL1 |> letf(tilel1 =>
            tilel1 |> mapSeq(fun(elem => add(elem)(li16(1)))) |> allocL1 |> copyToL2
          )
        ))
      )
    )
    //println(translateToString(util.gen.gap8.hosted("evenSmallerSlideExample").fromExpr(evenSmallerSlideExample)))
    //translateToString(util.gen.gap8.hosted("evenSmallerSlideExample").fromExpr(evenSmallerSlideExample))

    //No way
    val minSlideExample: ToBeTyped[Rise] = fun(
      ArrayType(5, ArrayType(5, i16)) ->:
        ArrayType(3, ArrayType(5, i16)) ->:
      ArrayType(9, ArrayType(5, i16)))((sth, filter) =>
      gap8Run(8)(
        sth |> slide(3)(1) |> mapSeq(fun(tile =>
          tile |> copyToL1 |> allocL1 |> letf(tilel1 =>
            zipND(2)(tilel1, filter) |> mapPar(mapPar(fun(elems =>
              add(fst(elems))(snd(elems))
            ))) |> allocL1 |> copyToL2
          )
        )) |> join
      )
    )
    //println(translateToString(util.gen.gap8.hosted("minSlide").fromExpr(minSlideExample)))

    //old w=322, slide(18)(16)
    val w: Nat = 320
    val h: Nat = 240
    val numStripes: Nat = 12
    val stripeSize: Nat = w * (h / numStripes)
    val stripeHeight: Nat = h / numStripes
    //Are you serious?
    val tiledFixSizeDmaHwce: ToBeTyped[Rise] =
      fun(
        ArrayType(h, ArrayType(w, i16)) ->:
          ArrayType(3, ArrayType(3, i16)) ->:
          ArrayType(3, ArrayType(3, i16)) ->:
          ArrayType(h - 2, ArrayType(w - 2, i16)))((pic, hw, vw) =>
        gap8Run(8)(
          hw |> copyToL1 |> allocL1 |> letf(l1hw =>
            vw |> copyToL1 |> allocL1 |> letf(l1vw =>
              pic |> slide(16)(14) |>
                mapSeq(fun(stripe =>
                  //stripe
                  //stripe is 2D
                  stripe |> copyToL1 |> allocL1 |> letf(l1stripe =>
                    l1stripe |> // |> mapPar(fun(row => row |> padCst(1)(1)(li16(0)))) |>
                      letf(l1convstripe =>
                        gap8hwConv3x3(0)(l1convstripe, l1hw) |> allocL1 |> letf(hconvres =>
                          gap8hwConv3x3(0)(l1convstripe, l1vw) |> allocL1 |> letf(vconvres =>
                            zipND(2)(hconvres)(vconvres) |> mapPar(mapPar(fun(elems =>
                              //gapSqrt
                              //pad left,right somewhere?
                              add(fst(elems))(snd(elems))
                            ))) |> allocL1 |> copyToL2
                          )
                        )
                      )
                  )
                  )
                ) |> join
              )
            )
        )
      )
    //println(translateToString(util.gen.gap8.hosted("tiledConv").fromExpr(tiledFixSizeDmaHwce)))

    val simpleNoSlide: ToBeTyped[Rise] =
      fun(
      ArrayType(size, ArrayType(size, i16)) ->:
        ArrayType(3, ArrayType(3, i16)) ->:
        ArrayType(3, ArrayType(3, i16)) ->:
        ArrayType(size - 2, ArrayType(size - 2, i16)))((pic, hw, vw) =>
      gap8Run(8)(
        hw |> copyToL1 |> allocL1 |> letf(l1hw =>
          vw |> copyToL1 |> allocL1 |> letf(l1vw =>
            pic |> copyToL1 |> allocL1 |> letf(l1pic =>
              gap8hwConv3x3(0)(l1pic, l1hw) |> allocL1 |> letf (hconvres => // hconvres |> copyToL2
                gap8hwConv3x3(0)(l1pic, l1vw) |> allocL1 |> letf (vconvres =>
                  zipND(2)(hconvres)(vconvres) |> mapPar(mapPar(fun(elems =>
                    add(fst(elems))(snd(elems))
                  ))) |> allocL1 |> copyToL2
                  /*zip(hconvres)(vconvres) |> mapPar(fun(rows =>
                    zip(fst(rows))(snd(rows)) |> mapPar(fun(elems =>
                      add(fst(elems))(snd(elems))
                    ))
                  )) |> allocL1 |> copyToL2*/
                )
              )
            )
          )
        )
      )
    )
    //println(translateToString(util.gen.gap8.hosted("doubleconv").fromExpr(simpleNoSlide)))
  }
}