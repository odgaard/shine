// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.GAP8.primitives.imperative
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import rise.core.types.{ FunType => _, DepFunType => _, TypePlaceholder => _, TypeIdentifier => _, ExprType => _, _ }
import rise.core.types.DataType._
import rise.core.types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class Dma2DOffsetCopy(tt: shine.GAP8.DMATransferType)(val dt: DataType, val n: Nat, val m: Nat, val offsetY: Nat, val offsetX: Nat, val src: Phrase[ExpType], val dst: Phrase[AccType]) extends CommandPrimitive {
  assert {
    src :: expT(ArrayType(n, ArrayType(m, dt)), read)
    dst :: accT(ArrayType(n + 2 * offsetY, ArrayType(m + 2 * offsetX, dt)))
    true
  }
  override val t: CommType = comm
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Dma2DOffsetCopy = new Dma2DOffsetCopy(tt)(v.data(dt), v.nat(n), v.nat(m), v.nat(offsetY), v.nat(offsetX), VisitAndRebuild(src, v), VisitAndRebuild(dst, v))
  def unwrap: (DataType, Nat, Nat, Nat, Nat, Phrase[ExpType], Phrase[AccType]) = (dt, n, m, offsetY, offsetX, src, dst)
}