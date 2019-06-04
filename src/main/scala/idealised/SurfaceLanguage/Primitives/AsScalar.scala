package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage
import idealised.SurfaceLanguage.Types.{ArrayType, DataType, TypeInference, VectorType}
import idealised.SurfaceLanguage.{Expr, PrimitiveExpr}

final case class AsScalar(array: Expr, override val t: Option[DataType] = None)
  extends PrimitiveExpr
{

  override def inferType(subs: TypeInference.SubstitutionMap): AsScalar = {
    import TypeInference._
    val array_ = TypeInference(array, subs)
    array_.t match {
      case Some(ArrayType(n, VectorType(m, dt))) =>
        AsScalar(array_, Some(ArrayType(n*m, dt)))
      case x => error(this.toString, s"`${x.toString}'", "n.<m.dt>")
    }
  }

  override def children: Seq[Any] = Seq(array, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(array: Expr, t: Option[DataType]@unchecked) => AsScalar(array, t)
  }

}
