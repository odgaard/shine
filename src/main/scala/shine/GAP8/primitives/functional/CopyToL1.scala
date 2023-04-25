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
final case class CopyToL1(val dt: DataType, val input: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    input :: expT(dt, write)
    true
  }
  override val t: ExpType = expT(dt, read)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): CopyToL1 = new CopyToL1(v.data(dt), VisitAndRebuild(input, v))
}
