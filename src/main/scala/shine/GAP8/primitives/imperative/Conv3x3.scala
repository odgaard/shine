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
final case class Conv3x3(val w: Nat, val h: Nat, val bias: Nat, val dt: DataType, val in: Phrase[ExpType], val filter: Phrase[ExpType], val out: Phrase[AccType]) extends CommandPrimitive {
  assert {
    in :: expT(ArrayType(w, ArrayType(h, dt)), read)
    filter :: expT(ArrayType(10, dt), read)
    out :: accT(ArrayType(w - 2, ArrayType(h - 2, dt)))
    true
  }
  override val t: CommType = comm
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Conv3x3 = new Conv3x3(v.nat(w), v.nat(h), v.nat(bias), v.data(dt), VisitAndRebuild(in, v), VisitAndRebuild(filter, v), VisitAndRebuild(out, v))
}
