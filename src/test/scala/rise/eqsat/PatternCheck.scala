package rise.eqsat

import rise.core.types.{Nat, NatIdentifier, NatKind}

class PatternCheck extends test_util.Tests {
  test("simple match") {
    val egraph = EGraph.emptyWithAnalysis(NoAnalysis)

    val (add1, add2) = {
      import ExprDSL._
      (egraph.addExpr(app(app(add, %(0)), %(1))),
       egraph.addExpr(app(app(add, %(2)), %(3))))
    }

    egraph.union(add1, add2)
    egraph.rebuild()

    val commuteAdd = {
      import PatternDSL._
      Rewrite.init[()]("commute-add",
        app(app(add, ?("a")), ?("b")).compile(),
        app(app(add, ?("b")), ?("a")).compile())
    }

    val matches = commuteAdd.search(egraph)
    val nMatches = matches.map(m => m.substs.size).sum
    assert(nMatches == 2)

    val applications = commuteAdd.apply(egraph, matches)
    egraph.rebuild()
    assert(applications.size == 2)

    egraph.dot().toFile("/tmp/simple-match.dot")
  }

  test("compile program with depApps") {
    import PatternDSL._

    val x: NatPattern = ?("x")
    val pattern = nApp(nApp(add, x), x).compile()

    import ematching._
    assert(
      pattern.prog.instructions == Vec(
        Bind(NatApp((), ()), Reg(0), Reg(1), NatReg(0)),
        Bind(NatApp((), ()), Reg(1), Reg(2), NatReg(1)),
        NatCompare(NatReg(1), NatReg(0)),
        Bind(Primitive(rise.core.primitives.add.primitive), Reg(2), Reg(3), NatReg(2))
      )
    )

    val egraph = EGraph.emptyWithAnalysis(NoAnalysis)
/* TODO
    {
      import ExprDSL._
      egraph.addExpr(natApp(natApp(add, NatVar("x")), 1: Nat))
      egraph.addExpr(natApp(natApp(add, NatIdentifier("x")), NatIdentifier("y")))
      egraph.addExpr(natApp(natApp(add, NatIdentifier("x")), NatIdentifier("x")))
      egraph.addExpr(natApp(natApp(mul, NatIdentifier("x")), NatIdentifier("x")))
    }
*/
    egraph.rebuild()

    assert(pattern.search(egraph).length == 1)
  }
}