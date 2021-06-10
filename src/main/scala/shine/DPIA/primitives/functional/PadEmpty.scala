// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.DPIA.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA.Types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class PadEmpty(val n: Nat, val r: Nat, val dt: DataType, val array: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    array :: expT(ArrayType(n, dt), write)
    true
  }
  override val t: ExpType = expT(ArrayType(n + r, dt), write)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): PadEmpty = new PadEmpty(v.nat(n), v.nat(r), v.data(dt), VisitAndRebuild(array, v))
}
