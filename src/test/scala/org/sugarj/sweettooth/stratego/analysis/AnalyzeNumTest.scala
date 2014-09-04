package org.sugarj.sweettooth.stratego.analysis

import org.scalatest._
import org.sugarj.sweettooth.stratego.Semantics._
import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.lib.Num._

import scala.language.implicitConversions

/**
 * Created by seba on 30/07/14.
 */
class AnalyzeNumTest extends FunSuite {

  val analysis = new Analyze(PowersetDomain)
  import analysis.dom
  def lift(t: Trm) = dom.lift(t)

  implicit def mkNatTrm(n: Int) = eval(mkNat(n), Trm.App('Foo), DEFS)

  def assertDomT(expected: dom.T)(actual: dom.T) = assertResult(expected)(actual)

//  test("zero") {
//    assertDomT(lift(0))(analysis.analyze(Call('zero), lift(Trm.App('Foo)), DEFS))
//  }
//
//  test("zero top") {
//    assertDomT(lift(0))(analysis.analyze(Call('zero), dom.top, DEFS))
//  }
//
//  test("succ") {
//    for (i <- 1 to 20)
//      assertDomT(lift(i + 1))(analysis.analyze(Call('succ), lift(i), DEFS))
//  }
//
//  test("succ prefix") {
//    var expected = dom.top
//    var actual = dom.top
//
//    for (i <- 1 to 20) {
//      expected = dom.liftApp('Succ, List(expected))
//      actual = analysis.analyze(Call('succ), actual, DEFS)
//      assertDomT(expected)(actual)
//    }
//  }
//
//  test(s"plus") {
//    for {m <- 1 to 5;
//         n <- 1 to 5}
//      assertDomT(lift(m + n))(analysis.analyze(Call('plus), lift(Trm.App('_, m, n)), DEFS))
//  }

  test(s"plus top top") {
    assertDomT(dom.top)(analysis.analyze(Call('plus), dom.liftApp('_, List(dom.top, dom.top)), DEFS))
  }
}
