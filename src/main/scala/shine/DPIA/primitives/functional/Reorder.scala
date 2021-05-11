// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.DPIA.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
final case class Reorder(val n: Nat, val dt: DataType, val a: AccessType, val idxF: NatToNat, val idxFiv: NatToNat, val input: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    input :: expT(ArrayType(n, dt), a)
    true
  }
  override val t: ExpType = expT(ArrayType(n, dt), a)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Reorder = new Reorder(v.nat(n), v.data(dt), v.access(a), v.natToNat(idxF), v.natToNat(idxFiv), VisitAndRebuild(input, v))
}
