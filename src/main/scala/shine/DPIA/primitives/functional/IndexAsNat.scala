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
final case class IndexAsNat(val n: Nat, val e: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    e :: expT(IndexType(n), read)
    true
  }
  override val t: ExpType = expT(NatType, read)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): IndexAsNat = new IndexAsNat(v.nat(n), VisitAndRebuild(e, v))
}
