package apps

import rise.core.types._
import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.rise._
import elevate.rise.rules._
import elevate.rise.rules.traversal._
import elevate.rise.rules.algorithmic._
import elevate.rise.rules.movement._
import cameraPipeRewrite.{
  afterTopLevel, gentlyReducedForm, stronglyReducedForm,
  isAppliedZip
}

object harrisCornerDetectionHalideRewrite {
  private def rewriteSteps(steps: Seq[Strategy[Rise]]): Strategy[Rise] = a => {
    var nRewrite = 0
    steps.foldLeft[RewriteResult[Rise]](Success(a))({ case (r, s) =>
      r.flatMapSuccess { e =>
        nRewrite += 1
        val result = util.printTime(s"rewrite $nRewrite", s(e))
        util.dotPrintTmp(s"rewrite$nRewrite", result)
        result
      }
    })
  }

  val unrollDots = normalize.apply(lowering.reduceSeqUnroll)

  def afterDefs(s: Strategy[Rise]): Strategy[Rise] = p => {
    (function(body(afterDefs(s))) <+ s)(p)
  }

  def argumentsTd(s: Strategy[Rise]): Strategy[Rise] = p =>
    (s <+
      argument(argumentsTd(s)) <+
      (isAppliedZip `;` function(argument(argumentsTd(s))))
      )(p)

  def normalizeInput: Strategy[Rise] =
    repeat(argumentsTd(
      gentleBetaReduction <+ etaReduction <+ mapFusion <+
      slideBeforeMap <+ slideInsideZip <+ mapMapFBeforeTranspose
    )) `;`
    cameraPipeRewrite.normalizeInput

  def storeToPrivate(e: Rise): Strategy[Rise] =
    subexpressionElimination(e) `;` {
      // TODO: use rewrite rules
      case rise.core.App(f, v) => Success(
        rise.OpenCL.TypedDSL.toPrivate(v) |> rise.core.TypedDSL.let(f))
      case _ => ???
    }

  object ocl {
    val unrollDots = normalize.apply(
      lowering.ocl.reduceSeqUnroll(AddressSpace.Private))

    val lineBuffer = lowering.ocl.circularBuffer(AddressSpace.Global)

    def harrisBufferedShape: Seq[Strategy[Rise]] = Seq(
      gentlyReducedForm,

      afterTopLevel(afterDefs(
        normalizeInput `;` stronglyReducedForm
      )) `;` gentlyReducedForm,

      afterTopLevel(afterDefs(argument(
        slideOutsideZip `;`
          argument(argument(normalizeInput `;` stronglyReducedForm))
      ))) `;` gentlyReducedForm
    )

    def harrisBufferedLowering: Strategy[Rise] = {
      oncetd(lowering.iterateStream) `;`
      repeatNTimes(2, argumentsTd(function(lineBuffer))) `;`
      normalize.apply(lowering.ocl.circularBufferLoadFusion) `;`
      gentlyReducedForm `;`
      argument(argument(oncetd(lowering.mapSeq))) `;`
      argument(function(argument(
        oncetd(mapOutsidePair) `;` oncetd(zipSame) `;`
        stronglyReducedForm `;` oncetd(lowering.mapSeq)
      ))) `;`
      function(argument(oncetd(
        function(lowering.mapSeq) `;`
        argument(lambdaBodyWithName(x => {
          import rise.core.DSL._
          storeToPrivate(fst(x)) `;`
          storeToPrivate(fst(snd(x))) `;`
          storeToPrivate(snd(snd(x)))
        }))
      ))) `;`
      unrollDots
    }

    def harrisBuffered: Strategy[Rise] = {
      rewriteSteps(harrisBufferedShape :+ (
        afterTopLevel(harrisBufferedLowering) `;` gentlyReducedForm
      ))
    }

    def harrisBufferedSplitPar(n: Int): Strategy[Rise] = {
      rewriteSteps(Seq(
        harrisBufferedShape.reduce(_`;`_),

        afterTopLevel(
          oncetd(splitJoin(32)) `;`
          gentlyReducedForm `;`
          argumentsTd(slideBeforeSplit) `;`
          argumentsTd(slideBeforeMap) `;`
          argumentsTd(slideBeforeSlide) `;`
          argumentsTd(slideBeforeMap) `;`
          gentlyReducedForm `;`
          oncetd(lowering.mapGlobal()) `;`
          oncetd(harrisBufferedLowering)
        )
      ))
    }
  }
}