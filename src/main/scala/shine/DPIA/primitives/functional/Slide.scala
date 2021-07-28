// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.DPIA.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import rise.core.types.{ FunType => _, DepFunType => _, TypePlaceholder => _, TypeIdentifier => _, Type => _, _ }
import rise.core.types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class Slide(val n: Nat, val sz: Nat, val sp: Nat, val dt: DataType, val input: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    input :: expT(ArrayType(sp * n + sz, dt), read)
    true
  }
  override val t: ExpType = expT(ArrayType(1 + n, ArrayType(sz, dt)), read)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Slide = new Slide(v.nat(n), v.nat(sz), v.nat(sp), v.data(dt), VisitAndRebuild(input, v))
}
