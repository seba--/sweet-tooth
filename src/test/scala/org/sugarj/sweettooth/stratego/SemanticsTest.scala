package org.sugarj.sweettooth.stratego

import org.scalatest._

import Syntax._
import Semantics._
import org.sugarj.sweettooth.stratego.lib.{Generic, Num}

import language.implicitConversions

/**
 * Created by seba on 30/07/14.
 */
class SemanticsTest extends FunSuite {

  val DEFS = Generic.DEFS ++ Num.DEFS

  implicit def mkNatTrm(n: Int) = eval(Num.mkNat(n), Trm.App('Foo, List()), DEFS)

  def assertTrm(expected: Trm)(actual: =>Trm) {
    try {
      val res = actual
      assertResult(expected)(res)
    } catch {
      case Fail(s, msg) => assert(false, s"Execution failed:\n  Message: $msg\n  Strategy: $s\n  Expected: $expected")
    }
  }

  def assertFail(actual: =>Trm) { assertFail(actual, "Evaluation should have failed") }
  def assertFail(actual: =>Trm, msg: String) {
    try {
      val res = actual
      assert(false, s"$msg, but produced $res")
    } catch {
      case Fail(_,_) => {}
    }
  }

  test ("testId") {
    assertTrm(0)(eval(Call('id), 0, DEFS))
    assertTrm(1)(eval(Call('id), 1, DEFS))
  }

  test ("zero") {
    assertTrm(0)(eval(Call('zero), 0, DEFS))
    assertTrm(0)(eval(Call('zero), 0, DEFS))
  }

  test ("succ") {
    for (i <- 1 to 20)
      assertTrm(i + 1)(eval(Call('succ), i, DEFS))
  }

  test ("plus") {
    for {m <- 1 to 5;
         n <- 1 to 5}
      assertTrm(m + n)(eval(Call('plus), Trm.App('_, m, n), DEFS))
  }

  test ("app") {
    assertTrm(0)(eval(Call('app, List(Build('Zero@@())), List()), 1, DEFS))
  }



  test ("strategy arg binds") {
    val s = Seqs(
      !!('Foo@@()),
      Call('app, List(??('x)), List()),
      !!('x)
    )
    assertTrm(Trm.App('Foo))(eval(s, 1, DEFS))
  }

  test ("strategy arg binds/match") {
    val s = Seqs(
      Call('app, List(??('x)), List()),
      !!('Foo@@()),
      Call('app, List(??('x)), List())
    )
    assertTrm(Trm.App('Foo))(eval(s, Trm.App('Foo), DEFS))
  }

  test ("strategy arg binds no rebind") {
    val s = Seqs(
      Call('app, List(??('x)), List()),
      !!('Bar@@()),
      Call('app, List(??('x)), List())
    )
    assertFail(eval(s, Trm.App('Foo), DEFS), s"strategy should fail to rebind x")
  }

  test ("def: no escape") {
    val s = Seqs(
      Call('id),
      !!('x)
    )
    assertFail(eval(s, 1, DEFS), "Variable x should not be bound outside of 'id")
  }

  test ("def: no inbreak") {
    val s = Seqs(
      ??('x),
      !!('Zero@@()),
      Call('id)
    )
    assertTrm(0)(eval(s, 1, DEFS))
  }

  test ("args share single store") {
    val twice = 'twice -> Def(List('s1, 's2), List(),
      Seqs(
        ??('_@@('x1, 'x2)),
        !!('x1),
        SVar('s1),
        ??('y1),
        !!('x2),
        SVar('s2),
        ??('y2),
        !!('_@@('y1, 'y2))
      ))
    val s1 = Call('twice, List(??('x), !!('x)), List())
    assertTrm(Trm.App('_, 0, 0))(eval(s1, Trm.App('_, 0, 1), DEFS + twice))

    val s2 = Call('twice, List(??('x), ??('x)), List())
    assertTrm(Trm.App('_, 0, 0))(eval(s2, Trm.App('_, 0, 0), DEFS + twice))
    assertFail(eval(s2, Trm.App('_, 0, 1), DEFS + twice))
  }

  test ("scoped: no escape") {
    val s = Seqs(
      ??('x),
      !!('Bar@@()),
      Call('app, List(Scoped('x, ??('x))), List()),
      !!('x)
    )
    assertTrm(Trm.App('Foo))(eval(s, Trm.App('Foo), DEFS))
  }

  test ("scoped: no inbreak") {
    val s = Seqs(
      ??('x),
      !!('Bar@@()),
      Call('app, List(Scoped('x, !!('x))), List())
    )
    assertFail(eval(s, Trm.App('Foo), DEFS))
  }
}
