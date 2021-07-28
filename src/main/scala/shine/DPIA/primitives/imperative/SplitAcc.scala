// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.DPIA.primitives.imperative
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import rise.core.types.{ FunType => _, DepFunType => _, TypePlaceholder => _, TypeIdentifier => _, Type => _, _ }
import rise.core.types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class SplitAcc(val n: Nat, val m: Nat, val dt: DataType, val array: Phrase[AccType]) extends AccPrimitive {
  assert {
    array :: accT(ArrayType(m, ArrayType(n, dt)))
    true
  }
  override val t: AccType = accT(ArrayType(n * m, dt))
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): SplitAcc = new SplitAcc(v.nat(n), v.nat(m), v.data(dt), VisitAndRebuild(array, v))
}
