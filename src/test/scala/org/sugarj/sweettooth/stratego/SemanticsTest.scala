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

  def assertTrm(expected: Trm)(actual: Trm) = assertResult(expected)(actual)

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
      assertTrm(m + n)(eval(Call('plus), Trm.App('_, List(m, n)), DEFS))
  }

  test ("app") {
    assertTrm(0)(eval(Call('app, List(Build('Zero@@())), List()), 1, DEFS))
  }



}
