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
final case class Conv5x5(val w: Nat, val h: Nat, val bias: Nat, val dt: DataType, val in: Phrase[ExpType], val filter: Phrase[ExpType], val out: Phrase[AccType]) extends CommandPrimitive {
  assert {
    in :: expT(ArrayType(h, ArrayType(w, dt)), read)
    filter :: expT(ArrayType(26, dt), read)
    out :: accT(ArrayType(h - 4, ArrayType(w - 4, dt)))
    true
  }
  override val t: CommType = comm
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Conv5x5 = new Conv5x5(v.nat(w), v.nat(h), v.nat(bias), v.data(dt), VisitAndRebuild(in, v), VisitAndRebuild(filter, v), VisitAndRebuild(out, v))
}
