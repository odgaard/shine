// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.OpenCL.primitives.imperative
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import rise.core.types.{ FunType => _, DepFunType => _, TypePlaceholder => _, TypeIdentifier => _, Type => _, _ }
import rise.core.types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class ParFor(level: shine.OpenCL.ParallelismLevel, dim: Int, unroll: Boolean, prefix: String)(val init: Nat, val n: Nat, val step: Nat, val dt: DataType, val out: Phrase[AccType], val body: Phrase[FunType[ExpType, FunType[AccType, CommType]]]) extends CommandPrimitive {
  assert {
    out :: accT(ArrayType(n, dt))
    body :: FunType(expT(IndexType(n), read), FunType(accT(dt), comm))
    true
  }
  override val t: CommType = comm
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): ParFor = new ParFor(level, dim, unroll, prefix)(v.nat(init), v.nat(n), v.nat(step), v.data(dt), VisitAndRebuild(out, v), VisitAndRebuild(body, v))
  def unwrap: (Nat, Nat, Nat, DataType, Phrase[AccType], Phrase[FunType[ExpType, FunType[AccType, CommType]]]) = (init, n, step, dt, out, body)
}
