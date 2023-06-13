// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.GAP8.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import rise.core.types.{ FunType => _, DepFunType => _, TypePlaceholder => _, TypeIdentifier => _, ExprType => _, _ }
import rise.core.types.DataType._
import rise.core.types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class Copy2DOffsetToL1(val dt: DataType, val h: Nat, val w: Nat, val offsetH: Nat, val offsetW: Nat, val input: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    input :: expT(ArrayType(h, ArrayType(w, dt)), read)
    true
  }
  override val t: ExpType = expT(ArrayType(h + 2 * offsetH, ArrayType(w + 2 * offsetW, dt)), write)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Copy2DOffsetToL1 = new Copy2DOffsetToL1(v.data(dt), v.nat(h), v.nat(w), v.nat(offsetH), v.nat(offsetW), VisitAndRebuild(input, v))
}