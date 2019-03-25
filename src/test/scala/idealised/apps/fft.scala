package idealised.apps

import idealised.SurfaceLanguage.DSL._
import idealised.OpenCL.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker

class fft extends idealised.util.Tests {
  def createStockhamIterationLambda(p: Int, LPrevIter: Int, N: Int): Expr = {
    val r = N / (LPrevIter * p)

    val cmultandsum = fun(vt => fun(acc => {
      val lres = acc._1 + (vt._1._1 * vt._2._1 - vt._1._2 * vt._2._2)
      val rres = acc._2 + (vt._1._2 * vt._2._1 + vt._1._1 * vt._2._2)
      tuple(lres, rres)
    }))

    // TODO compare with previous implementations again
    val reorderedB =
      generate(fun(IndexType(LPrevIter))(i =>
        generate(fun(IndexType(p))(j =>
          generate(fun(IndexType(p))(k => {
            // TODO use treatNatExprAsNat properly (look at it again)
            val exponentWoMinus2 =
              mapNatExpr(mapNatExpr(indexAsNat(j), j => j * LPrevIter) +
                mapNatExpr(indexAsNat(i), i => i) * mapNatExpr(indexAsNat(k), k => k / (p * LPrevIter)), x => x)
            val exponent = cast(double, exponentWoMinus2) * -2.0
            tuple(cast(float, oclFun("cospi", double, double, exponent)),
              cast(float, oclFun("sinpi", double, double, exponent)))
          }))))))

    val modPReorder = join() o transpose() o split(p)
    val createY = transpose() o modPReorder o split(r)

    fun(ArrayType(N, TupleType(float, float)))(x =>
      join() o transpose() o mapSeq(join() o transpose()) o
        split(LPrevIter) o
          join() o mapGlobal(mapSeq(fun(yChunkWithBrow => {

            val yChunk = yChunkWithBrow._1
            val Brow = yChunkWithBrow._2
            mapSeq(fun(Bchunk => reduceSeq(cmultandsum, tuple(0.0f, 0.0f)) $ zip(yChunk, Bchunk)

            )) $ Brow

    }))) o map(fun(yChunkRow => zip(yChunkRow, reorderedB))) o map(transpose() o split(LPrevIter)) o createY $ x)
  }

  val GOLD_STOCK_ITER_P2_LPREV4_N8: Array[Double] =
    Array((1.0, 0.0), (4.121320343559643, -2.121320343559643), (4.0, -5.0),
      (1.05025253169416733, -4.94974746830583267),
      (-1.0, 0.0), (-0.121320343559643, 2.121320343559543), (4.0, 5.0),
      (10.949747468305832671, 4.949747468305832671)).flatMap(t => List(t._1, t._2))

  val GOLD_FFT_8: Array[(Double, Double)] =
    Array((28.00000000000000, 28.00000000000000),
          (-13.65685424949238, 5.65685424949238),
          (-8.00000000000000, -0.00000000000000),
          (-5.65685424949238, -2.34314575050762),
          (-4.00000000000000, -4.00000000000000),
          (-2.34314575050762, -5.65685424949238),
          (0.00000000000000, -8.00000000000000),
          (5.65685424949238, -13.65685424949238))


  /*
  test("Correct FFT iteration code can be generated in OpenMP.") {
    val N = 8
    val LPrevIter = 1
    val p = 2

    val fftiter = createStockhamIterationLambda(p, LPrevIter, N)
    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(fftiter, Map()))
    val kernel = idealised.OpenMP.ProgramGenerator.makeCode(phrase)
    println(kernel.code)

    SyntaxChecker(kernel.code)
  }

  test("single small FFT iteration computes correct result in OpenMP") {
    val p = 2
    val LPrevIter = 4
    val N = 8

    val stockhamIter = createStockhamIterationLambda(p, LPrevIter, N)

    val inputVec = Array.tabulate(N) {i => (1.0 * i, 0.0)}
    val (output, _) = Execute(32,32)[Array[Double]](stockhamIter, inputVec)

    output.size shouldBe GOLD_STOCK_ITER_P2_LPREV4_N8.size
    for (i <- 0 until output.size) output(i) should be (GOLD_STOCK_ITER_P2_LPREV4_N8(i) +- 1e-13)
  }
  */
}