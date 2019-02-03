package idealised.OpenMP
import idealised.DPIA.DSL.λ
import idealised.DPIA.IntermediatePrimitives.MapVecI
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType, VectorType}

class TranslationContext() extends idealised.C.TranslationContext {
  override def assign(dt: DataType,
                      A: Phrase[AccType],
                      E: Phrase[ExpType]): Phrase[CommandType] = {
    dt match {
      case VectorType(n, st) =>
        MapVecI(n, st, st, λ(ExpType(st))(x => λ(AccType(st))(a => assign(st, a, x) )), E, A)

      case _ => super.assign(dt, A, E)
    }
  }
}
