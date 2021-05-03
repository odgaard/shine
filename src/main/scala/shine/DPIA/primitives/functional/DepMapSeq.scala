// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.DPIA.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
final case class DepMapSeq(unroll: Boolean)(val n: Nat, val ft1: NatToData, val ft2: NatToData, val f: Phrase[DepFunType[NatKind, FunType[ExpType, ExpType]]], val array: Phrase[ExpType]) extends ExpPrimitive {
  {
    f :: ({
      val k = f.t.x
      DepFunType[NatKind, PhraseType](k, FunType(expT(NatToDataApply(ft1, k), read), expT(NatToDataApply(ft2, k), write)))
    })
    array :: expT(DepArrayType(n, ft1), read)
  }
  override val t: ExpType = expT(DepArrayType(n, ft2), write)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): DepMapSeq = new DepMapSeq(unroll)(v.nat(n), v.natToData(ft1), v.natToData(ft2), VisitAndRebuild(f, v), VisitAndRebuild(array, v))
  def unwrap: (Nat, NatToData, NatToData, Phrase[DepFunType[NatKind, FunType[ExpType, ExpType]]], Phrase[ExpType]) = (n, ft1, ft2, f, array)
}
