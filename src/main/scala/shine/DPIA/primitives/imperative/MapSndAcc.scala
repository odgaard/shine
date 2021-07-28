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
final case class MapSndAcc(val dt1: DataType, val dt2: DataType, val dt3: DataType, val f: Phrase[FunType[AccType, AccType]], val record: Phrase[AccType]) extends AccPrimitive {
  assert {
    f :: FunType(accT(dt3), accT(dt2))
    record :: accT(PairType(dt1, dt3))
    true
  }
  override val t: AccType = accT(PairType(dt1, dt2))
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): MapSndAcc = new MapSndAcc(v.data(dt1), v.data(dt2), v.data(dt3), VisitAndRebuild(f, v), VisitAndRebuild(record, v))
}
