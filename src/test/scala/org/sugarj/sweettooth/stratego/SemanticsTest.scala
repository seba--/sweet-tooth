package org.sugarj.sweettooth.stratego

import org.scalatest._
import org.sugarj.sweettooth.stratego.Semantics._
import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.lib.{Generic, Num}

import scala.language.implicitConversions

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
    assertTrm(0)(eval(Call('id_0_0), 0, DEFS))
    assertTrm(1)(eval(Call('id_0_0), 1, DEFS))
  }

  test ("zero") {
    assertTrm(0)(eval(Call('zero_0_0), 0, DEFS))
    assertTrm(0)(eval(Call('zero_0_0), 0, DEFS))
  }

  test ("succ") {
    for (i <- 1 to 20)
      assertTrm(i + 1)(eval(Call('succ_0_0), i, DEFS))
  }

  test ("plus") {
    for {m <- 1 to 5;
         n <- 1 to 5}
      assertTrm(m + n)(eval(Call('plus_0_0), Trm.App('_, m, n), DEFS))
  }

  test ("app") {
    assertTrm(0)(eval(Call('app_1_0, List(Build('Zero@@())), List()), 1, DEFS))
  }



  test ("strategy arg binds") {
    val s = Seqs(
      !!('Foo@@()),
      Call('app_1_0, List(??('x)), List()),
      !!('x)
    )
    assertTrm(Trm.App('Foo))(eval(s, 1, DEFS))
  }

  test ("strategy arg binds/match") {
    val s = Seqs(
      Call('app_1_0, List(??('x)), List()),
      !!('Foo@@()),
      Call('app_1_0, List(??('x)), List())
    )
    assertTrm(Trm.App('Foo))(eval(s, Trm.App('Foo), DEFS))
  }

  test ("strategy arg binds no rebind") {
    val s = Seqs(
      Call('app_1_0, List(??('x)), List()),
      !!('Bar@@()),
      Call('app_1_0, List(??('x)), List())
    )
    assertFail(eval(s, Trm.App('Foo), DEFS), s"strategy should fail to rebind x")
  }

  test ("def: no escape") {
    val s = Seqs(
      Call('id_0_0),
      !!('x)
    )
    assertFail(eval(s, 1, DEFS), "Variable x should not be bound outside of 'id")
  }

  test ("def: no inbreak") {
    val s = Seqs(
      ??('x),
      !!('Zero@@()),
      Call('id_0_0)
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
      Call('app_1_0, List(Scoped('x, ??('x))), List()),
      !!('x)
    )
    assertTrm(Trm.App('Foo))(eval(s, Trm.App('Foo), DEFS))
  }

  test ("scoped: no inbreak") {
    val s = Seqs(
      ??('x),
      !!('Bar@@()),
      Call('app_1_0, List(Scoped('x, !!('x))), List())
    )
    assertFail(eval(s, Trm.App('Foo), DEFS))
  }

  test ("equal") {
    for (i <- 1 to 5; j <- 1 to 5) {
      val trm = Trm.App('_, i, j)
      if (i == j)
        assertTrm(trm)(eval(Call('equal_0_0), trm, DEFS))
      else
        assertFail(eval(Call('equal_0_0), trm, DEFS))
    }
  }
}
