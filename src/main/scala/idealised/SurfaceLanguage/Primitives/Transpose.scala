package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.{PrimitiveExpr, VisitAndRebuild}
import idealised.SurfaceLanguage.Types.{ArrayType, DataType, TypeInference}

final case class Transpose(array: DataExpr,
                           override val t: Option[DataType])
  extends PrimitiveExpr
{
  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    array.t match {
      case Some(ArrayType(n, ArrayType(m, dt))) =>
        import idealised.DPIA.FunctionalPrimitives._
        import idealised.DPIA.DSL._
        import idealised.DPIA.Types._

        val transposeFunction =
          λ(ExpType(IndexType(n * m)))(i => {
            UnsafeAsIndex(n * m, treatNatExprAsNat(AsNat(n * m, i), j => {
              val col = (j % n) * m
              val row = j / n
              row + col
            }))
          })

        val transposeInverseFunction =
          λ(ExpType(IndexType(n * m)))(i => {
            UnsafeAsIndex(n * m, treatNatExprAsNat(AsNat(n * m, i), j => {
              val col = (j % m) * n
              val row = j / m
              row + col
            }))
          })

        Split(n, m, dt,
          Reorder(n*m, dt, transposeFunction, transposeInverseFunction,
            Join(n, m, dt, array.toPhrase[ExpType])))

      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Transpose = {
    import TypeInference._
    TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(ArrayType(n, ArrayType(m, dt))) => Transpose(array, Some(ArrayType(m, ArrayType(n, dt))))
        case x => error(expr = s"Transpose($array)", found = s"`${x.toString}'", expected = "n.m.dt")
      })
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): DataExpr =
    Transpose(VisitAndRebuild(array, f), t)
}
