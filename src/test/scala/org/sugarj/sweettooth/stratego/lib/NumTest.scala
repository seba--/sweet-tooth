package org.sugarj.sweettooth.stratego.lib

import org.scalatest._
import org.sugarj.sweettooth.stratego.Semantics._
import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.lib.Num._
import org.sugarj.sweettooth.stratego.{Semantics, Syntax}

import scala.language.implicitConversions

/**
 * Created by seba on 30/07/14.
 */
class NumTest extends FunSuite {

  implicit def mkNatTrm(n: Int) = eval(mkNat(n), Trm.App('Foo), DEFS)

  def assertTrm(expected: Trm)(actual: Trm) = assertResult(expected)(actual)

  test("zero") {
    assertTrm(0)(eval(Call('zero_0_0), Trm.App('Foo), DEFS))
  }

  test("succ") {
    for (i <- 1 to 20)
      assertTrm(i + 1)(eval(Call('succ_0_0), i, DEFS))
  }

  test(s"plus") {
    for {m <- 1 to 5;
         n <- 1 to 5}
      assertTrm(m + n)(eval(Call('plus_0_0), Trm.App('_, m, n), DEFS))
  }
}
