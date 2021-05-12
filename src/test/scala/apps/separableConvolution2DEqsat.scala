package apps

import apps.separableConvolution2D._
import apps.separableConvolution2DCheck.wrapExpr
import rise.core.DSL._
import rise.core.semantics.FloatData
import rise.core.types._
import rise.eqsat.rules
import rise.eqsat.Basic.{proveEquivBENF, proveEquivCNF}

class separableConvolution2DEqsat extends test_util.Tests {
  val weights2d = binomialWeights2d
  val weightsV = binomialWeightsV
  val weightsH = binomialWeightsH

  private val (separateDot, separateDotT) = {
    import rise.eqsat.NamedRewrite
    import rise.eqsat.NamedRewriteDSL._

    def mulT(xName: String) = lam(xName, app(app(mul, app(fst, xName)), app(snd, xName)))
    def *(p: Pattern) = app(map, p)
    val sum = app(app(reduce, add), l(FloatData(0)))
    def dot(xName: String, a: Pattern, b: Pattern) =
      app(sum, app(*(mulT(xName)), app(app(zip, a), b)))
    val w2d: Pattern = weights2d
    val wV: Pattern = weightsV
    val wH: Pattern = weightsH

    (NamedRewrite.init("separate-dot-hv",
      dot("x", app(join, w2d :: ("t": Type)), app(join, ("nbh": Pattern) :: ("t": Type)))
        -->
      dot("x2", wV, app(*(lam("y", dot("x3", wH, "y"))), "nbh"))
    ),
    NamedRewrite.init("separate-dot-vh",
      dot("x", app(join, w2d :: ("t": Type)), app(join, ("nbh": Pattern) :: ("t": Type)))
        -->
      dot("x2", wH, app(*(lam("y", dot("x3", wV, "y"))), app(transpose, "nbh")))
    ))
  }

  private val (separateDotCNF, separateDotTCNF) = {
    import rise.eqsat.NamedRewrite
    import rise.eqsat.NamedRewriteDSL._

    def mulT(xName: String) = lam(xName, app(app(mul, app(fst, xName)), app(snd, xName)))
    def *(p: Pattern) = app(map, p)
    val sum = app(app(reduce, add), l(FloatData(0)))
    def dot(xName: String, a: Pattern) =
      app(zip, a) >> *(mulT(xName)) >> sum
    val w2d: Pattern = weights2d
    val wV: Pattern = weightsV
    val wH: Pattern = weightsH

    (NamedRewrite.init("separate-dot-hv",
      ((join :: t("t")) >> dot("x", app(join :: t("t"), w2d)))
        -->
      (*(dot("xh", wH)) >> dot("xv", wV))
    ),
    NamedRewrite.init("separate-dot-vh",
      ((join :: t("t")) >> dot("x", app(join :: t("t"), w2d)))
        -->
      (transpose >> *(dot("xv", wV)) >> dot("xh", wH))
    ))
  }

  // -- algorithmic

  test("base to factorised") {
    proveEquivBENF(wrapExpr(base(weights2d)), wrapExpr(factorised(weightsV)(weightsH)),
      Seq(separateDot))
  }

  test("base to factorised (CNF)") {
    proveEquivCNF(wrapExpr(base(weights2d)), wrapExpr(factorised(weightsV)(weightsH)),
      Seq(rules.combinatory.compositionElim,
        rules.combinatory.compositionAssoc1,
        rules.combinatory.compositionAssoc2,
        rules.combinatory.compositionIntro,
        separateDotCNF))
  }

  test("base to factorised (VH)") {
    proveEquivBENF(wrapExpr(base(weights2d)), wrapExpr(factorisedVH(weightsV)(weightsH)),
      Seq(separateDotT))
  }

  test("base to factorised (VH, CNF)") {
    proveEquivCNF(wrapExpr(base(weights2d)), wrapExpr(factorisedVH(weightsV)(weightsH)),
      Seq(rules.combinatory.compositionElim,
        rules.combinatory.compositionAssoc1,
        rules.combinatory.compositionAssoc2,
        rules.combinatory.compositionIntro,
        separateDotTCNF))
  }

  test("base to scanline") {
    // FIXME: rules.beta does not work here
    proveEquivBENF(wrapExpr(base(weights2d)), wrapExpr(scanline(weightsV)(weightsH)), Seq(
      rules.eta, rules.betaExtract, rules.removeTransposePair,
      rules.mapFusion, rules.mapFission,
      rules.slideBeforeMap, rules.mapSlideBeforeTranspose, rules.slideBeforeMapMapF,
      separateDotT
    ))
  }

  test("base to scanline (CNF)") {
    proveEquivCNF(wrapExpr(base(weights2d)), wrapExpr(scanline(weightsV)(weightsH)), Seq(
      rules.combinatory.compositionElim,
      rules.combinatory.compositionAssoc1,
      rules.combinatory.compositionAssoc2,
      rules.combinatory.compositionIntro,
      rules.combinatory.removeTransposePair,
      rules.combinatory.compositionLeftId,
      rules.combinatory.compositionRightId,
      rules.combinatory.mapFusion,
      rules.combinatory.mapFission,
      rules.combinatory.slideBeforeMap,
      rules.combinatory.mapSlideBeforeTranspose,
      rules.combinatory.slideBeforeMapMapF,
      separateDotTCNF
    ))
  }

  test("scanline to separated") {
    proveEquivBENF(wrapExpr(scanline(weightsV)(weightsH)),
      wrapExpr(separated(weightsV)(weightsH)), Seq(
      rules.eta, rules.betaExtract, rules.mapFission, rules.mapFusion
    ))
  }

  test("scanline to separated (CNF)") {
    proveEquivCNF(wrapExpr(scanline(weightsV)(weightsH)),
      wrapExpr(separated(weightsV)(weightsH)), Seq(
        rules.combinatory.compositionAssoc1,
        rules.combinatory.compositionAssoc2,
        rules.combinatory.mapFission,
        rules.combinatory.mapFusion
      ))
  }

  // -- lowering

  test("base to baseSeq") {
    proveEquivBENF(wrapExpr(base(weights2d)), wrapExpr(baseSeq(weights2d)), Seq(
      rules.reduceSeqUnroll, rules.mapSeq
    ))
  }

  test("factorised to factorisedSeq") {
    proveEquivBENF(wrapExpr(factorised(weightsV)(weightsH)),
      wrapExpr(factorisedSeq(weightsV)(weightsH)), Seq(
      rules.reduceSeqUnroll, rules.mapSeq
    ))
  }

  test("separated to separatedSeq") {
    proveEquivBENF(wrapExpr(separated(weightsV)(weightsH)),
      wrapExpr(separatedSeq(weightsV)(weightsH)), Seq(
      rules.reduceSeqUnroll, rules.mapSeq, rules.toMemAfterMapSeq
    ))
  }

  test("scanline to scanlineSeq") {
    proveEquivBENF(wrapExpr(scanline(weightsV)(weightsH)),
      wrapExpr(scanlineSeq(weightsV)(weightsH)), Seq(
      rules.reduceSeqUnroll, rules.mapSeq
    ))
  }

  test("scanline to regRotSeq") {
    proveEquivBENF(wrapExpr(scanline(weightsV)(weightsH)),
      wrapExpr(regRotSeq(weightsV)(weightsH)), Seq(
      rules.reduceSeqUnroll, rules.mapSeq, rules.rotateValuesScalar, rules.iterateStream
    ))
  }
}

