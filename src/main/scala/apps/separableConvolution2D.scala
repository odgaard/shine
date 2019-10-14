package apps

import lift.core._
import lift.core.DSL._
import lift.core.types._
import lift.core.semantics._
import lift.core.primitives._
import lift.OpenCL.primitives._
import lift.core.HighLevelConstructs._

object separableConvolution2D {
  private def weights2d(scale: Float, ws: Seq[Seq[Int]]): Expr =
    larr(ws.map(r => ArrayData(r.map(x => FloatData(x * scale)))))
  private def weights1d(scale: Float, ws: Seq[Int]): Expr =
    larr(ws.map(x => FloatData(x * scale)))

  // Binomial filter, convolution is separable:
  //
  // 1 2 1   1
  // 2 4 2 ~ 2 x 1 2 1
  // 1 2 1   1
  val binomialWeights2d: Expr = weights2d(1.0f / 16.0f, Seq(
    Seq(1, 2, 1),
    Seq(2, 4, 2),
    Seq(1, 2, 1)
  ))
  val binomialWeightsV: Expr = weights1d(1.0f / 4.0f, Seq(
    1, 2, 1
  ))
  val binomialWeightsH: Expr = binomialWeightsV

  // Sobel filters, convolutions are separable:
  //
  // Sx:
  // -1  0 +1     1
  // -2  0 +2  ~  2 x -1  0 +1
  // -1  0 +1     1
  val sobelXWeights2d: Expr = weights2d(1.0f, Seq(
    Seq(-1, 0, +1),
    Seq(-2, 0, +2),
    Seq(-1, 0, +1)
  ))
  val sobelXWeightsV: Expr = weights1d(1.0f, Seq(
    1, 2, 1
  ))
  val sobelXWeightsH: Expr = weights1d(1.0f, Seq(
    -1, 0, +1
  ))
  // Sy:
  // -1 -2 -1     -1
  //  0  0  0  ~   0 x 1 2 1
  // +1 +2 +1     +1
  val sobelYWeights2d: Expr = weights2d(1.0f, Seq(
    Seq(-1, -2, -1),
    Seq( 0,  0,  0),
    Seq(+1, +2, +1)
  ))
  val sobelYWeightsV: Expr = sobelXWeightsH
  val sobelYWeightsH: Expr = sobelXWeightsV

  // Separable convolution expressions

  val id: Expr = fun(x => x)
  val mulT: Expr = fun(x => fst(x) * snd(x))
  val dot: Expr = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> reduce(add)(l(0.0f))
  ))
  val dotSeq: Expr = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> reduceSeq(add)(l(0.0f))
  ))
  val dotSeqUnroll: Expr = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> reduceSeqUnroll(add)(l(0.0f))
  ))
  val weightsSeqVecUnroll: Expr = fun(weights => fun(vectors =>
    zip(map(vectorFromScalar)(weights))(vectors) |>
      map(mulT) |> oclReduceSeqUnroll(AddressSpace.Private)(add)(vectorFromScalar(l(0.0f)))
  ))

  val base: Expr = fun(3`.`3`.`float)(weights2d =>
    padClamp2D(1) >> slide2D(3, 1) >>
      map(map(fun(nbh => dot(join(weights2d))(join(nbh)))))
  )
  val baseSeq: Expr = fun(3`.`3`.`float)(weights2d =>
    padClamp2D(1) >> slide2D(3, 1) >>
      mapSeq(mapSeq(fun(nbh => dotSeq(join(weights2d))(join(nbh)))))
  )

  val factorised: Expr = fun(3`.`float)(weightsV => fun(3`.`float)(weightsH =>
    padClamp2D(1) >> slide2D(3, 1) >>
      map(map(map(dot(weightsH)) >> dot(weightsV)))
  ))
  val factorisedSeq: Expr = fun(3`.`float)(weightsV => fun(3`.`float)(weightsH =>
    padClamp2D(1) >> slide2D(3, 1) >>
      mapSeq(mapSeq(map(dotSeq(weightsH)) >> dotSeq(weightsV)))
  ))

  val separated: Expr = fun(3`.`float)(weightsV => fun(3`.`float)(weightsH => {
    val horizontal = map(slide(3)(1) >> map(dot(weightsH)))
    val vertical = slide(3)(1) >> map(transpose >> map(dot(weightsV)))
    padClamp2D(1) >> vertical >> horizontal
  }))
  val separatedSeq: Expr =  fun(3`.`float)(weightsV => fun(3`.`float)(weightsH => {
    val horizontal = mapSeq(slide(3)(1) >> mapSeq(dotSeq(weightsH)))
    val vertical = slide(3)(1) >> mapSeq(transpose >> mapSeq(dotSeq(weightsV)))
    padClamp2D(1) >> vertical >> horizontal
  }))

  val scanline: Expr = fun(3`.`float)(weightsV => fun(3`.`float)(weightsH =>
    padClamp2D(1) >> slide(3)(1) >> map(transpose >>
      map(dot(weightsV)) >>
      slide(3)(1) >>
      map(dot(weightsH))
    )
  ))
  val scanlineSeq: Expr = fun(3`.`float)(weightsV => fun(3`.`float)(weightsH =>
    padClamp2D(1) >> slide(3)(1) >> mapSeq(transpose >>
      mapSeq(dotSeq(weightsV)) >>
      slide(3)(1) >>
      mapSeq(dotSeq(weightsH))
    )
  ))

  val regRotSeq: Expr = fun(3`.`float)(weightsV => fun(3`.`float)(weightsH =>
    padClamp2D(1) >> slide(3)(1) >> mapSeq(transpose >>
      map(dotSeq(weightsV)) >>
      slideSeq(slideSeq.Values)(3)(1)(id)(dotSeq(weightsH))
    )
  ))
  val regRotPar: Expr = fun(3`.`float)(weightsV => fun(3`.`float)(weightsH => {
    val Dv = weightsSeqVecUnroll(weightsV)
    val Dh = weightsSeqVecUnroll(weightsH)
    val shuffle =
      asScalar >> drop(3) >> take(6) >> slide(4)(1) >> join >> asVector(4)
    // map(padClamp(4)(4) >> asVectorAligned(4)) >> padClamp(1)(1) >>
    map(implN(w => fun(w`.`float)(x =>
      x |> asVectorAligned(4)
        |> padCst(1)(0)(vectorFromScalar(x `@` lidx(0, w)))
        |> padCst(0)(1)(vectorFromScalar(x `@` lidx(w - 1, w)))
    ))) >> padClamp(1)(1) >>
    slide(3)(1) >> mapGlobal(transpose >>
      map(Dv) >>
      oclSlideSeq(slideSeq.Values)(AddressSpace.Private)(3)(1)(id)(shuffle >> Dh) >>
      asScalar
    )
  }))

  def computeGold(h: Int, w: Int,
                  input: Array[Array[Float]],
                  weights: Array[Array[Float]]): Array[Array[Float]] = {
    val output = Array.fill(h, w)(Float.NaN)
    val lastY = h - 1
    val lastX = w - 1

    for (y <- input.indices) {
      val r0 = if (y > 0) input(y - 1) else input(0)
      val r1 = input(y)
      val r2 = if (y < lastY) input(y + 1) else input(lastY)
      for (x <- r1.indices) {
        val c0 = if (x > 0) x - 1 else 0
        val c1 = x
        val c2 = if (x < lastX) x + 1 else lastX
        output(y)(x) =
          weights(0)(0) * r0(c0) + weights(0)(1) * r0(c1) + weights(0)(2) * r0(c2) +
            weights(1)(0) * r1(c0) + weights(1)(1) * r1(c1) + weights(1)(2) * r1(c2) +
            weights(2)(0) * r2(c0) + weights(2)(1) * r2(c1) + weights(2)(2) * r2(c2)
      }
    }

    output
  }

  def computeGold(h: Int, w: Int,
                  input: Array[Array[Float]],
                  weights: Expr): Array[Array[Float]] = {
    import lift.core.semantics._
    weights match {
      case Literal(ArrayData(a)) => computeGold(h, w, input,
        a.map(r => r.asInstanceOf[ArrayData].a.map(x => x.asInstanceOf[FloatData].f).toArray).toArray)
      case _ => ???
    }
  }
}
