package org.sugarj.sweettooth.stratego.analysis.v1_d2

import org.scalatest._
import org.sugarj.sweettooth.stratego.Semantics._
import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.analysis.base.{BasicStack, StoreTrait}
import org.sugarj.sweettooth.stratego.analysis.domain.d2_PowersetFlagDomain
import org.sugarj.sweettooth.stratego.analysis.v1.v1Analysis
import org.sugarj.sweettooth.stratego.lib.Num._

import scala.language.implicitConversions

/**
 * Created by seba on 30/07/14.
 */
class AnalyzeNumTest extends FunSuite {
  val prefix = getClass.getPackage.getName.substring(getClass.getPackage.getName.lastIndexOf('.') + 1)

  type V = d2_PowersetFlagDomain.T
  type D = d2_PowersetFlagDomain.D.type
  val dom = d2_PowersetFlagDomain.D

  object analysis extends
    v1Analysis[V, D] with
    BasicStack[V, D] with
    StoreTrait[V, D] {
    val dom = AnalyzeNumTest.this.dom
  }

  def lift(t: Trm) = dom.lift(t)

  implicit def mkNatTrm(n: Int) = eval(mkNat(n), Trm.App('Foo), DEFS)

  def assertDomT(expected: V)(actual: V) = assertResult(expected)(actual)

  test(s"$prefix: zero") {
    assertDomT(lift(0))(analysis.analyze(Call('zero), lift(Trm.App('Foo)), DEFS))
  }

  test(s"$prefix: zero top") {
    assertDomT(lift(0))(analysis.analyze(Call('zero), dom.top, DEFS))
  }

  test(s"$prefix: succ") {
    for (i <- 1 to 20)
      assertDomT(lift(i + 1))(analysis.analyze(Call('succ), lift(i), DEFS))
  }

  test(s"$prefix: succ prefix") {
    var expected = dom.top
    var actual = dom.top

    for (i <- 1 to 20) {
      expected = dom.liftApp('Succ, List(expected))
      actual = analysis.analyze(Call('succ), actual, DEFS)
      assertDomT(expected)(actual)
    }
  }

  test(s"$prefix: plus") {
    for {m <- 1 to 5;
         n <- 1 to 5}
      assertDomT(lift(m + n))(analysis.analyze(Call('plus), lift(Trm.App('_, m, n)), DEFS))
  }

  test(s"$prefix: plus top top") {
    val maybeOne = dom.join(dom.top, dom.liftApp('Succ, List(dom.top)))
    assertDomT(maybeOne)(analysis.analyze(Call('plus), dom.liftApp('_, List(dom.top, dom.top)), DEFS))
  }

  test(s"$prefix: plus zero top") {
    assertDomT(dom.top)(analysis.analyze(Call('plus), dom.liftApp('_, List(lift(0), dom.top)), DEFS))
  }

  test(s"$prefix: plus one top") {
    val atLeastOne = dom.liftApp('Succ, List(dom.top))
    assertDomT(atLeastOne)(analysis.analyze(Call('plus), dom.liftApp('_, List(lift(1), dom.top)), DEFS))
  }

  test(s"$prefix: plus >=one top") {
    val atLeastOne = dom.liftApp('Succ, List(dom.top))
    val atLeastOneOrTwo = dom.liftApp('Succ, List(dom.join(dom.top, dom.liftApp('Succ, List(dom.top)))))
    assertDomT(atLeastOneOrTwo)(analysis.analyze(Call('plus), dom.liftApp('_, List(atLeastOne, dom.top)), DEFS))
  }

  test(s"$prefix: plus top one") {
    val atLeastOrExactlyOne = dom.liftApp('Succ, List(dom.join(dom.top, dom.liftApp('Zero, List()))))
    assertDomT(atLeastOrExactlyOne)(analysis.analyze(Call('plus), dom.liftApp('_, List(dom.top, lift(1))), DEFS))
  }

  test(s"$prefix: plus top >=one") {
    val atLeastOne = dom.liftApp('Succ, List(dom.top))
    assertDomT(atLeastOne)(analysis.analyze(Call('plus), dom.liftApp('_, List(dom.top, atLeastOne)), DEFS))
  }

  test(s"$prefix: plus >=one >=one") {
    val atLeastOne = dom.liftApp('Succ, List(dom.top))
    val atLeastTwo = dom.liftApp('Succ, List(dom.liftApp('Succ, List(dom.top))))
    assertDomT(atLeastTwo)(analysis.analyze(Call('plus), dom.liftApp('_, List(atLeastOne, atLeastOne)), DEFS))
  }
  test(s"$prefix: plus two top") {
    val atLeastTwo = dom.liftApp('Succ, List(dom.liftApp('Succ, List(dom.top))))
    assertDomT(atLeastTwo)(analysis.analyze(Call('plus), dom.liftApp('_, List(lift(2), dom.top)), DEFS))
  }
}
