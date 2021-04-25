// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.DPIA.primitives.imperative
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
final case class Assign(val dt: DataType, val lhs: Phrase[AccType], val rhs: Phrase[ExpType]) extends CommandPrimitive {
  {
    lhs :: accT(dt)
    rhs :: expT(dt, read)
  }
  override val t: CommType = comm
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Assign = new Assign(v.data(dt), VisitAndRebuild(lhs, v), VisitAndRebuild(rhs, v))
}
