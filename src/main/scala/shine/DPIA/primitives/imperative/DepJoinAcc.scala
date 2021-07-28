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
final case class DepJoinAcc(val n: Nat, val lenF: NatToNat, val dt: DataType, val array: Phrase[AccType]) extends AccPrimitive {
  assert {
    array :: accT(ArrayType(BigSum(from = 0, upTo = n - 1, (i: Nat) => lenF(i)), dt))
    true
  }
  override val t: AccType = accT(DepArrayType(n, n2dtFun { (i: NatIdentifier) => ArrayType(lenF(i), dt) }))
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): DepJoinAcc = new DepJoinAcc(v.nat(n), v.natToNat(lenF), v.data(dt), VisitAndRebuild(array, v))
}
