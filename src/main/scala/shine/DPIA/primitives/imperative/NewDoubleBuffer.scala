// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.DPIA.primitives.imperative
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import rise.core.types.{ FunType => _, DepFunType => _, TypePlaceholder => _, TypeIdentifier => _, ExprType => _, _ }
import rise.core.types.DataType._
import rise.core.types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class NewDoubleBuffer(val dt1: DataType, val dt2: DataType, val dt3: DataType, val n: Nat, val in: Phrase[ExpType], val out: Phrase[AccType], val f: Phrase[FunType[PhrasePairType[PhrasePairType[PhrasePairType[ExpType, AccType], CommType], CommType], CommType]]) extends CommandPrimitive {
  assert {
    in :: expT(dt1, read)
    out :: accT(dt2)
    f :: FunType(PhrasePairType(PhrasePairType(PhrasePairType(expT(ArrayType(n, dt3), read), accT(ArrayType(n, dt3))), comm), comm), comm)
    true
  }
  override val t: CommType = comm
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): NewDoubleBuffer = new NewDoubleBuffer(v.data(dt1), v.data(dt2), v.data(dt3), v.nat(n), VisitAndRebuild(in, v), VisitAndRebuild(out, v), VisitAndRebuild(f, v))
}
