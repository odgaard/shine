package elevate.core

import lift.core._
import lift.core.DSL._
import lift.core.primitives.map
import rules._
import rules.algorithmic.{mapFusion, mapLastFission}
import strategies._
import strategies.algorithmic.{mapFirstFission, mapFullFission}
import strategies.traversal._

class fission_fusion extends idealised.util.Tests {
  val norm = normalize(betaReduction <+ etaReduction)

  def eq(a: Expr, b: Expr): Unit = {
    if (!StructuralEquality(norm(a), norm(b))) {
      throw new Exception(s"expected structural equality:\n$a\n$b")
    }
  }

  def check(a: Expr, fis: Strategy, b: Expr, fus: Strategy): Unit = {
    eq(fis(a), b)
    eq(fus(b), a)
  }

  test("last fission, chain of 2") {
    check(
      fun(f1 => fun(f2 => map(f1 >> f2))),
      depthFirst(position(2)(mapLastFission)),
      fun(f1 => fun(f2 => map(f1) >> map(f2))),
      depthFirst(find(mapFusion)))
  }

  test("last fission, chain of 3") {
    check(
      fun(f1 => fun(f2 => fun(f3 => map(f1 >> f2 >> f3)))),
      depthFirst(position(3)(mapLastFission)),
      fun(f1 => fun(f2 => fun(f3 => map(f1 >> f2) >> map(f3)))),
      depthFirst(find(mapFusion)))
  }

  test("first fission, chain of 2") {
    check(
      fun(f1 => fun(f2 => map(f1 >> f2))),
      depthFirst(position(2)(mapFirstFission)),
      fun(f1 => fun(f2 => map(f1) >> map(f2))),
      depthFirst(find(mapFusion)))
  }

  test("first fission, chain of 3") {
    check(
      fun(f1 => fun(f2 => fun(f3 => map(f1 >> f2 >> f3)))),
      depthFirst(position(3)(mapFirstFission)),
      fun(f1 => fun(f2 => fun(f3 => map(f1) >> map(f2 >> f3)))),
      depthFirst(find(mapFusion)))
  }

  test("full fission, chain of 2") {
    check(
      fun(f1 => fun(f2 => map(f1 >> f2))),
      depthFirst(position(2)(mapFullFission)),
      fun(f1 => fun(f2 => map(f1) >> map(f2))),
      depthFirst(find(mapFusion)))
  }

  test("full fission, chain of 3") {
    check(
      fun(f1 => fun(f2 => fun(f3 => map(f1 >> f2 >> f3)))),
      depthFirst(position(3)(mapFullFission)),
      fun(f1 => fun(f2 => fun(f3 => map(f1) >> map(f2) >> map(f3)))),
      normalize(mapFusion))
  }
}
