package rise.core

import rise.core.DSL._
import Type._
import rise.core.types._
import rise.core.primitives._

class dependentTypes extends test_util.Tests {
  test("Infer int addition type") {
    val e =
      depFun((n: Nat) =>
        fun(
          DepArrayType(n, n2dtFun(i => (i + 1) `.` f32)) ->: DepArrayType(
            n,
            n2dtFun(i => (i + 1) `.` f32)
          )
        )(xs => xs |> depMapSeq(depFun((_: Nat) => mapSeq(fun(x => x)))))
      )
    val inferred: Expr = inferDependent(e)
    println(inferred)
    println(inferred.t)
  }

  test("Dependent pair construct") {
    val e = depFun((n: Nat) =>
      fun(n `.` f32)(x => dpair(n)(x))
    )
    val inferred: Expr = inferDependent(e)
    println(inferred)
    print(inferred.t)
  }

  test("Dependent pair match") {
    val e = fun(Nat `**` (n => n`.`f32))(pair =>
      dmatch(pair)(depFun((n: Nat) => fun(x => dpair(n)(x))))
    )
    val inferred: Expr = inferDependent(e)
    println(inferred)
    print(inferred.t)
  }

  test("Dependent pair match with reduction") {
    val e = fun(Nat `**` (n => n`.`f32))(pair =>
      dmatch(pair)(depFun((_: Nat) => fun(xs =>
        reduceSeq(fun(x => fun(y => x + y)))(l(1.0f))(xs))
      ))
    )
    val inferred: Expr = inferDependent(e)
    println(inferred)
    print(inferred.t)
  }


  test("Simple nested") {
    val e = depFun((n: Nat) => fun(n `*.` (i => (i+1) `.` f32))(array =>
        depMapSeq(depFun((_: Nat) => mapSeq(fun(x => x))))(array)
      ))

    val inferred: Expr = inferDependent(e)
    println(inferred)
    print(inferred.t)
  }

  test("Simple reduce") {
    val e = depFun((n: Nat) => fun(n `*.` (i => (i+1) `.` f32))(array =>
      depMapSeq(depFun((_: Nat) => reduceSeq(fun(x => fun(y => x + y)))(l(0.0f))))(array)
    ))

    val inferred: Expr = inferDependent(e)
    println(inferred)
    print(inferred.t)
  }

  test("List of list dot product") {
    val e = depFun((n: Nat) =>
      fun(n `.` f32)(vector =>
      fun(n `.` NatType)(lengths =>
      fun(n `*.` (i => (lengths `#` i) `.` (f32 `x` IndexType(n))))(array => {
        depMapSeq(depFun((_: Nat) => fun(
          row =>
            reduceSeq(
              fun(x => fun(y => x + y))
            )(l(0.0f))(mapSeq(fun(entry => {
              val number = entry._1
              val index = entry._2
              number * (vector `@` index)
            }))(row))
        )
        ))(array)
      }
    ))))

    val inferred: Expr = inferDependent(e)
    println(inferred)
    print(inferred.t)
  }

}
