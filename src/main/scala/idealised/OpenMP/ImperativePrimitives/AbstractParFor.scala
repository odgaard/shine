package idealised.OpenMP.ImperativePrimitives


import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

abstract class AbstractParFor[T <: DataType](val n: Nat,
                                             val dt: T,
                                             val out: Phrase[AccType],
                                             val body: Phrase[ExpType -> (AccType -> CommandType)])
  extends CommandPrimitive {

  override lazy val `type`: CommandType =
    (n: Nat) -> (dt: DataType) ->
      (out :: acc"[$n.$dt]") ->
      (body :: t"exp[idx($n)] -> acc[$dt] -> comm") ->
      comm

  override def eval(s: Store): Store = {
    val nE = evalIndexExp(s, Literal(IndexData(n, IndexType(n))))
    val bodyE = OperationalSemantics.eval(s, body)(OperationalSemantics.BinaryFunctionEvaluator)

    (0 until nE.eval).foldLeft(s)((s1, i) => {
      OperationalSemantics.eval(s1,
        bodyE(Literal(i))(out `@` Literal(i)))
    })
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    makeParFor(fun(n), fun(dt), VisitAndRebuild(out, fun), VisitAndRebuild(body, fun))
  }

  override def prettyPrint: String =
    s"(${this.getClass.getSimpleName} $n ${PrettyPhrasePrinter(out)} ${PrettyPhrasePrinter(body)})"


  override def xmlPrinter: Elem =
    <parFor n={ToString(n)} dt={ToString(dt)}>
      <output type={ToString(AccType(ArrayType(n, dt)))}>
        {Phrases.xmlPrinter(out)}
      </output>
      <body type={ToString(ExpType(IndexType(n)) -> (AccType(dt) -> CommandType()))}>
        {Phrases.xmlPrinter(body)}
      </body>
    </parFor>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })

  def makeParFor: (Nat, T, Phrase[AccType], Phrase[ExpType -> (AccType -> CommandType)]) => AbstractParFor[T]

}