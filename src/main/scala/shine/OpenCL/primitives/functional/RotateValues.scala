// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.OpenCL.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import rise.core.types.{ FunType => _, DepFunType => _, TypePlaceholder => _, TypeIdentifier => _, Type => _, _ }
import rise.core.types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class RotateValues(val a: AddressSpace, val n: Nat, val sz: Nat, val dt: DataType, val wrt: Phrase[FunType[ExpType, ExpType]], val input: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    wrt :: FunType(expT(dt, read), expT(dt, write))
    input :: expT(ArrayType(n - 1 + sz, dt), read)
    true
  }
  override val t: ExpType = expT(ArrayType(n, ArrayType(sz, dt)), read)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): RotateValues = new RotateValues(v.addressSpace(a), v.nat(n), v.nat(sz), v.data(dt), VisitAndRebuild(wrt, v), VisitAndRebuild(input, v))
}
