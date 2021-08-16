// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.DPIA.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import rise.core.types.{ FunType => _, DepFunType => _, TypePlaceholder => _, TypeIdentifier => _, ExprType => _, _ }
import rise.core.types.DataType._
import rise.core.types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class Map(val n: Nat, val dt1: DataType, val dt2: DataType, val a: Access, val f: Phrase[FunType[ExpType, ExpType]], val array: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    f :: FunType(expT(dt1, a), expT(dt2, a))
    array :: expT(ArrayType(n, dt1), a)
    true
  }
  override val t: ExpType = expT(ArrayType(n, dt2), a)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Map = new Map(v.nat(n), v.data(dt1), v.data(dt2), v.access(a), VisitAndRebuild(f, v), VisitAndRebuild(array, v))
}
