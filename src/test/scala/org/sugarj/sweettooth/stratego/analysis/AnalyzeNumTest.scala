package org.sugarj.sweettooth.stratego.analysis

import org.scalatest._
import org.sugarj.sweettooth.stratego.Semantics._
import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.analysis.base.{StoreTrait, BasicStack}
import org.sugarj.sweettooth.stratego.analysis.domain.PowersetDomain
import org.sugarj.sweettooth.stratego.analysis.v1.v1Analysis
import org.sugarj.sweettooth.stratego.lib.Num._

import scala.language.implicitConversions

/**
 * Created by seba on 30/07/14.
 */
class AnalyzeNumTest extends FunSuite {

  type V = PowersetDomain.T
  type D = PowersetDomain.D.type
  val dom = PowersetDomain.D

  object analysis extends
    v1Analysis[V, D] with
    BasicStack[V, D] with
    StoreTrait[V, D] {
    val dom = AnalyzeNumTest.this.dom
  }

  def lift(t: Trm) = dom.lift(t)

  implicit def mkNatTrm(n: Int) = eval(mkNat(n), Trm.App('Foo), DEFS)

  def assertDomT(expected: V)(actual: V) = assertResult(expected)(actual)

  test("zero") {
    assertDomT(lift(0))(analysis.analyze(Call('zero), lift(Trm.App('Foo)), DEFS))
  }

  test("zero top") {
    assertDomT(lift(0))(analysis.analyze(Call('zero), dom.top, DEFS))
  }

  test("succ") {
    for (i <- 1 to 20)
      assertDomT(lift(i + 1))(analysis.analyze(Call('succ), lift(i), DEFS))
  }

  test("succ prefix") {
    var expected = dom.top
    var actual = dom.top

    for (i <- 1 to 20) {
      expected = dom.liftApp('Succ, List(expected))
      actual = analysis.analyze(Call('succ), actual, DEFS)
      assertDomT(expected)(actual)
    }
  }

  test(s"plus") {
    for {m <- 1 to 5;
         n <- 1 to 5}
      assertDomT(lift(m + n))(analysis.analyze(Call('plus), lift(Trm.App('_, m, n)), DEFS))
  }

  test(s"plus top top") {
    assertDomT(dom.top)(analysis.analyze(Call('plus), dom.liftApp('_, List(dom.top, dom.top)), DEFS))
  }

  test(s"plus zero top") {
    assertDomT(dom.top)(analysis.analyze(Call('plus), dom.liftApp('_, List(lift(0), dom.top)), DEFS))
  }

  test(s"plus one top") {
    val atLeastOne = dom.liftApp('Succ, List(dom.top))
    assertDomT(atLeastOne)(analysis.analyze(Call('plus), dom.liftApp('_, List(lift(1), dom.top)), DEFS))
  }

  test(s"plus >=one top") {
    val atLeastOne = dom.liftApp('Succ, List(dom.top))
    assertDomT(atLeastOne)(analysis.analyze(Call('plus), dom.liftApp('_, List(atLeastOne, dom.top)), DEFS))
  }

  test(s"plus top one") {
    val atLeastOne = dom.liftApp('Succ, List(dom.top))
    assertDomT(atLeastOne)(analysis.analyze(Call('plus), dom.liftApp('_, List(dom.top, lift(1))), DEFS))
  }

  test(s"plus top >=one") {
    val atLeastOne = dom.liftApp('Succ, List(dom.top))
    assertDomT(atLeastOne)(analysis.analyze(Call('plus), dom.liftApp('_, List(dom.top, atLeastOne)), DEFS))
  }

  test(s"plus >=one >=one") {
    val atLeastOne = dom.liftApp('Succ, List(dom.top))
    val atLeastTwo = dom.liftApp('Succ, List(dom.liftApp('Succ, List(dom.top))))
    assertDomT(atLeastTwo)(analysis.analyze(Call('plus), dom.liftApp('_, List(atLeastOne, atLeastOne)), DEFS))
  }
  test(s"plus two top") {
    val atLeastTwo = dom.liftApp('Succ, List(dom.liftApp('Succ, List(dom.top))))
    assertDomT(atLeastTwo)(analysis.analyze(Call('plus), dom.liftApp('_, List(lift(2), dom.top)), DEFS))
  }
}
