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
object gap8hwConv3x3 extends Builder {
  private final case class Primitive()(override val t: ExprType = TypePlaceholder) extends rise.core.Primitive {
    override val name: String = "gap8hwConv3x3"
    override def setType(ty: ExprType): Primitive = Primitive()(ty)
    override def primEq(obj: rise.core.Primitive): Boolean = obj.getClass == getClass
    override def typeScheme: ExprType = impl { (h: Nat) => impl { (w: Nat) => impl { (dt: DataType) => expl { (bias: Nat) => ArrayType(h, ArrayType(w, dt)) ->: ArrayType(3, ArrayType(3, dt)) ->: ArrayType(h - 2, ArrayType(w - 2, dt)) } } } }
  }
  override def toString: String = "gap8hwConv3x3"
  override def primitive: rise.core.Primitive = Primitive()()
  override def apply: ToBeTyped[rise.core.Primitive] = toBeTyped(Primitive()())
  override def unapply(arg: Expr): Boolean = arg match {
    case _: Primitive => true
    case _ => false
  }
}
