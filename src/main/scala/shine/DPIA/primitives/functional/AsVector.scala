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
final case class AsVector(val n: Nat, val m: Nat, val dt: DataType, val a: Access, val array: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    array :: expT(ArrayType(m * n, dt), a)
    true
  }
  override val t: ExpType = expT(ArrayType(m, VectorType(n, dt)), a)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): AsVector = new AsVector(v.nat(n), v.nat(m), v.data(dt), v.access(a), VisitAndRebuild(array, v))
}
