// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package rise.GAP8.primitives
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core._
import rise.core.types._
import rise.core.types.DataType._
import arithexpr.arithmetic._
object gap8hwConv7x7 extends Builder {
  private final case class Primitive()(override val t: ExprType = TypePlaceholder) extends rise.core.Primitive {
    override val name: String = "gap8hwConv7x7"
    override def setType(ty: ExprType): Primitive = Primitive()(ty)
    override def primEq(obj: rise.core.Primitive): Boolean = obj.getClass == getClass
    override def typeScheme: ExprType = expl { (bias: Nat) => impl { (iw: Nat) => impl { (ih: Nat) => impl { (ow: Nat) => impl { (oh: Nat) => impl { (s: DataType) => ArrayType(iw, ArrayType(ih, s)) ->: ArrayType(7, ArrayType(7, s)) ->: ArrayType(ow, ArrayType(oh, s)) } } } } } }
  }
  override def toString: String = "gap8hwConv7x7"
  override def primitive: rise.core.Primitive = Primitive()()
  override def apply: ToBeTyped[rise.core.Primitive] = toBeTyped(Primitive()())
  override def unapply(arg: Expr): Boolean = arg match {
    case _: Primitive => true
    case _ => false
  }
}
