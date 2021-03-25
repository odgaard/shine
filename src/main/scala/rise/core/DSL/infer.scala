package rise.core.DSL

import util.monads._
import Type.freshTypeIdentifier
import rise.core.traverse._
import rise.core.{traverse=>_, _}
import rise.core.types.InferenceException.error
import rise.core.types._

import scala.collection.mutable

object infer {
  case class V(sol: Solution) extends SolutionVisitor(sol) {
    override def `type`[T <: Type] : T => Pure[T] = t => return_(sol(t).asInstanceOf[T])
    override def expr: Expr => Pure[Expr] = {
      case Opaque(e, t) => for {t1 <- `type`(t)} yield Opaque(e, t1)
      case TypeAnnotation(e, t) => for {t1 <- `type`(t)} yield TypeAnnotation(e, t1)
      case e => super.expr(e)
    }
  }
  private [DSL] def apply(e: Expr,
            printFlag: Flags.PrintTypesAndTypeHoles = Flags.PrintTypesAndTypeHoles.Off,
            explDep: Flags.ExplicitDependence = Flags.ExplicitDependence.Off): Expr = {
    val (collected_e, ftvSubs) = collectAssertOpaque(e)
    val frozen_e = V(freeze(ftvSubs)).expr(collected_e).unwrap
    // Constraints of the form `implicit type var == explicit type var` result in substitutions
    // `implicit type var -> explicit type var`. We (ab)use that fact to create directed constraints out of
    // type assertions and opaque types. To do so, we make the type identifiers on one side of the constraint explicit,
    // and we return a `ftvSubs` map that maps these explicit type identifiers back to implicit type identifiers.
    val (typed_e, constraints) = constrainTypes(Map())(frozen_e)
    // Applies ftvSubs to the constraint solutions
    val solution = Constraint.solve(constraints, Seq(), Seq())(explDep) ++ ftvSubs
    val res = traverse(typed_e, Visitor(solution))
    if (printFlag == Flags.PrintTypesAndTypeHoles.On) {
      printTypesAndTypeHoles(res)
    }
    res
  }

  def printTypesAndTypeHoles(expr: Expr): Unit = {
    val hasHoles = new PureAccumulatorTraversal[Boolean] {
      override val accumulator = OrMonoid
      override def expr: Expr => Pair[Expr] = {
        case h@primitives.typeHole(msg) =>
          println(s"found type hole ${msg}: ${h.t}")
          accumulate(true)(h : Expr)
        case p@primitives.printType(msg) =>
          println(s"$msg : ${p.t} (Rise level)")
          return_(p: Expr)
        case e => super.expr(e)
      }
    }
    if (traverse(expr, hasHoles)._1) {
      error("type holes were found")(Seq())
    }
  }

  implicit class TrivialSolutionConcat(a: Solution) {
    def <>(b: Solution): Solution =
      new Solution(
        a.ts ++ b.ts,
        a.ns ++ b.ns,
        a.as ++ b.as,
        a.ms ++ b.ms,
        a.fs ++ b.fs,
        a.n2ds ++ b.n2ds,
        a.n2ns ++ b.n2ns,
        a.natColls ++ b.natColls
      )
  }

  private def implToExpl[K <: Kind.Identifier with Kind.Explicitness] : K => (K, K) = i =>
    (i.asImplicit.asInstanceOf[K], i.asExplicit.asInstanceOf[K])

  private def freeze(ftvSubs: Solution): Solution =
    Solution(
      ftvSubs.ts.keySet.map(_.asInstanceOf[DataTypeIdentifier]).map(implToExpl).toMap,
      ftvSubs.ns.keySet.map(implToExpl).toMap,
      ftvSubs.as.keySet.map(implToExpl).toMap,
      ftvSubs.ms.keys.map(implToExpl).toMap,
      ftvSubs.fs.keys.map(implToExpl).toMap,
      ftvSubs.n2ds.keySet.map(implToExpl).toMap,
      ftvSubs.n2ns.keySet.map(implToExpl).toMap,
      ftvSubs.natColls.keySet.map(implToExpl).toMap,
    )

  private def explToImpl[K <: Kind.Identifier with Kind.Explicitness] : K => Map[K, K] = i =>
    Map(i.asExplicit.asInstanceOf[K] -> i.asImplicit.asInstanceOf[K])

  private def getFTVSubs(t: Type): Solution = {
    getFTVs(t).foldLeft(Solution())((solution, ftv) =>
      solution match {
        case s@Solution(ts, ns, as, ms, fs, n2ds, n2ns, natColls) =>
          ftv match {
            case _: TypeIdentifier => throw TypeException("TypeIdentifier cannot be frozen")
            case i: DataTypeIdentifier      => s.copy(ts = ts ++ explToImpl(i))
            case i: NatIdentifier           => s.copy(ns = ns ++ explToImpl(i))
            case i: AddressSpaceIdentifier  => s.copy(as = as ++ explToImpl(i))
            case i: MatrixLayoutIdentifier  => s.copy(ms = ms ++ explToImpl(i))
            case i: FragmentKindIdentifier  => s.copy(fs = fs ++ explToImpl(i))
            case i: NatToDataIdentifier     => s.copy(n2ds = n2ds ++ explToImpl(i))
            case i: NatToNatIdentifier      => s.copy(n2ns = n2ns ++ explToImpl(i))
            case i: NatCollectionIdentifier => s.copy(natColls = natColls ++ explToImpl(i))
            case i => throw TypeException(s"${i.getClass} is not supported yet")
          }
      }
    )
  }

  val FTVGathering = new PureAccumulatorTraversal[Seq[Kind.Identifier]] {
    override val accumulator = SeqMonoid
    override def typeIdentifier[I <: Kind.Identifier]: VarType => I => Pair[I] = _ => {
      case i: Kind.Explicitness => accumulate(if (!i.isExplicit) Seq(i) else Seq())(i.asInstanceOf[I])
      case i => accumulate(Seq(i))(i)
    }
    override def nat: Nat => Pair[Nat] = ae => {
      val ftvs = mutable.ListBuffer[Kind.Identifier]()
      val r = ae.visitAndRebuild({
        case i: NatIdentifier if !i.isExplicit => ftvs += i; i
        case n => n
      })
      accumulate(ftvs.toSeq)(r)
    }
  }

  def getFTVs(t: Type): Seq[Kind.Identifier] = {
    traverse(t, FTVGathering)._1.distinct
  }

  def getFTVsRec(e: Expr): Seq[Kind.Identifier] = {
    traverse(e, FTVGathering)._1.distinct
  }

  private val genType : Expr => Type = e => if (e.t == TypePlaceholder) freshTypeIdentifier else e.t

  private val collectAssertOpaque : Expr => (Expr, Solution) = {
    case expr : Identifier => (expr, Solution())
    case expr@Lambda(x, e) =>
      val (e1, s) = collectAssertOpaque(e)
      (Lambda(x, e1)(expr.t), s)
    case expr@App(f, e) =>
      val (f1, fs) = collectAssertOpaque(f)
      val (e1, es) = collectAssertOpaque(e)
      (App(f1, e1)(expr.t), fs <> es)
    case expr@DepLambda(x, e) =>
      val (e1, s) = collectAssertOpaque(e)
      x match {
        case n: NatIdentifier => (DepLambda[NatKind](n, e1)(expr.t), s)
        case dt: DataTypeIdentifier => (DepLambda[DataKind](dt, e1)(expr.t), s)
        case ad: AddressSpaceIdentifier => (DepLambda[AddressSpaceKind](ad, e1)(expr.t), s)
        case n2n: NatToNatIdentifier => (DepLambda[NatToNatKind](n2n, e1)(expr.t), s)
      }
    case expr@DepApp(f, x) =>
      val (f1, s) = collectAssertOpaque(f)
      (DepApp(f1, x)(expr.t), s)
    case expr@Literal(d) => (expr, Solution())
    case TypeAssertion(e, t) => // Transform assertions into annotations, collect FTVs
      val (e1, s) = collectAssertOpaque(e)
      (TypeAnnotation(e1, t), s <> getFTVSubs(t))
    case Opaque(e, t) => (Opaque(e, t), getFTVSubs(t)) // Preserve opaques, collect FTVs
    case expr: Primitive => (expr, Solution())
  }

  private val constrainTypes : Map[String, Type] => Expr => (Expr, Seq[Constraint]) = env => {
    case i: Identifier =>
      val t = env.getOrElse(i.name,
        if (i.t == TypePlaceholder) error(s"$i has no type")(Seq()) else i.t )
      val c = TypeConstraint(t, i.t)
      (i.setType(t), Nil :+ c)

    case expr@Lambda(x, e) =>
      val tx = x.setType(genType(x))
      val env1 : Map[String, Type] = env + (tx.name -> tx.t)
      val (te, cs) = constrainTypes(env1)(e)
      val ft = FunType(tx.t, te.t)
      val exprT = genType(expr)
      val c = TypeConstraint(exprT, ft)
      (Lambda(tx, te)(ft), cs :+ c)

    case expr@App(f, e) =>
      val (tf, csF) = constrainTypes(env)(f)
      val (te, csE) = constrainTypes(env)(e)
      val exprT = genType(expr)
      val c = TypeConstraint(tf.t, FunType(te.t, exprT))
      (App(tf, te)(exprT), csF :++ csE :+ c)

    case expr@DepLambda(x, e) =>
      val (te, csE) = constrainTypes(env)(e)
      val exprT = genType(expr)
      val tf = x match {
        case n: NatIdentifier =>
          DepLambda[NatKind](n, te)(DepFunType[NatKind, Type](n, te.t))
        case dt: DataTypeIdentifier =>
          DepLambda[DataKind](dt, te)(DepFunType[DataKind, Type](dt, te.t))
        case ad: AddressSpaceIdentifier =>
          DepLambda[AddressSpaceKind](ad, te)(DepFunType[AddressSpaceKind, Type](ad, te.t))
        case n2n: NatToNatIdentifier =>
          DepLambda[NatToNatKind](n2n, te)(DepFunType[NatToNatKind, Type](n2n, te.t))
      }
      val c = TypeConstraint(exprT, tf.t)
      (tf, csE :+ c)

    case expr@DepApp(f, x) =>
      val (tf, csF) = constrainTypes(env)(f)
      val exprT = genType(expr)
      val c = DepConstraint(tf.t, x, exprT)
      (DepApp(tf, x)(exprT), csF :+ c)

    case TypeAnnotation(e, t) =>
      val (te, csE) = constrainTypes(env)(e)
      val c = TypeConstraint(te.t, t)
      (te, csE :+ c)

    case TypeAssertion(e, t) =>
      val (te, csE) = constrainTypes(env)(e)
      val c = TypeConstraint(te.t, t)
      (te, csE :+ c)

    case o: Opaque => (o, Nil)
    case l: Literal => (l, Nil)
    case p: Primitive => (p.setType(p.typeScheme), Nil)
  }

  private case class Visitor(sol: Solution) extends PureTraversal {
    override def expr : Expr => Pure[Expr] = {
      case Opaque(x, _) => return_(x)
      case TopLevel(x, inst) => TopLevel.Visitor(inst, sol).expr(x)
      case e => super.expr(e)
    }
    override def nat : Nat => Pure[Nat] = n => return_(sol(n))
    override def `type`[T <: Type] : T => Pure[T] = t => return_(sol(t).asInstanceOf[T])
    override def addressSpace : AddressSpace => Pure[AddressSpace] = a => return_(sol(a))
    override def natToData : NatToData => Pure[NatToData] = n2d => return_(sol(n2d))
    override def natToNat : NatToNat => Pure[NatToNat] = n2n => return_(sol(n2n))
  }
}

object inferDependent {
  def apply(e: ToBeTyped[Expr],
            printFlag: Flags.PrintTypesAndTypeHoles = Flags.PrintTypesAndTypeHoles.Off): Expr = infer(e match {
    case ToBeTyped(e) => e
  }, printFlag, Flags.ExplicitDependence.On)
}
