package org.sugarj.sweettooth.stratego.analysis

import org.sugarj.sweettooth.stratego.Semantics._
import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.analysis.base.Analysis
import org.sugarj.sweettooth.stratego.analysis.domain.Domain
import org.sugarj.sweettooth.stratego.lib.Num._

import scala.language.implicitConversions

/**
* Created by seba on 30/07/14.
*/
abstract class AnalyzeNumSuite extends AnalysisSuite {
  val prefix = this.getClass.getPackage.getName.substring(getClass.getPackage.getName.lastIndexOf('.') + 1)

  type V
  type D <: Domain[V]
  val dom: D

  val analysis: Analysis[V,D]

  def lift(t: Trm) = dom.lift(t)

  implicit def mkNatTrm(n: Int) = eval(mkNat(n), Trm.App('Foo), DEFS)

  val zero: V
  test(s"$prefix: zero") {
    assertDomT(zero)(analysis.analyze(Call('zero_0_0), lift(Trm.App('Foo)), DEFS))
  }

  val zero_top: V
  test(s"$prefix: zero top") {
    assertDomT(zero_top)(analysis.analyze(Call('zero_0_0), dom.top, DEFS))
  }

  def succ(i: Int): V
  test(s"$prefix: succ") {
    for (i <- 1 to 20)
      assertDomT(succ(i))(analysis.analyze(Call('succ_0_0), lift(i), DEFS))
  }

  def succ_prefix(i: Int): V
  test(s"$prefix: succ prefix") {
    var actual = dom.top

    for (i <- 1 to 20) {
      actual = analysis.analyze(Call('succ_0_0), actual, DEFS)
      assertDomT(succ_prefix(i))(actual)
    }
  }

  def plus(m: Int, n: Int): V
  test(s"$prefix: plus") {
    for {m <- 1 to 5;
         n <- 1 to 5}
      assertDomT(plus(m, n))(analysis.analyze(Call('plus_0_0), lift(Trm.App('_, m, n)), DEFS))
  }

  val plus_top_top: V
  test(s"$prefix: plus top top") {
    assertDomT(plus_top_top)(analysis.analyze(Call('plus_0_0), dom.liftApp('_, List(dom.top, dom.top)), DEFS))
  }

  val plus_zero_top: V
  test(s"$prefix: plus zero top") {
    assertDomT(plus_zero_top)(analysis.analyze(Call('plus_0_0), dom.liftApp('_, List(lift(0), dom.top)), DEFS))
  }

  val plus_one_top: V
  test(s"$prefix: plus one top") {
    assertDomT(plus_one_top)(analysis.analyze(Call('plus_0_0), dom.liftApp('_, List(lift(1), dom.top)), DEFS))
  }

  val plus_oneMore_top: V
  test(s"$prefix: plus >=one top") {
    val atLeastOne = dom.liftApp('Succ, List(dom.top))
    assertDomT(plus_oneMore_top)(analysis.analyze(Call('plus_0_0), dom.liftApp('_, List(atLeastOne, dom.top)), DEFS))
  }

  val plus_top_one: V
  test(s"$prefix: plus top one") {
    assertDomT(plus_top_one)(analysis.analyze(Call('plus_0_0), dom.liftApp('_, List(dom.top, lift(1))), DEFS))
  }

  val plus_top_oneMore: V
  test(s"$prefix: plus top >=one") {
    val atLeastOne = dom.liftApp('Succ, List(dom.top))
    assertDomT(plus_top_oneMore)(analysis.analyze(Call('plus_0_0), dom.liftApp('_, List(dom.top, atLeastOne)), DEFS))
  }

  val plus_oneMore_oneMore: V
  test(s"$prefix: plus >=one >=one") {
    val atLeastOne = dom.liftApp('Succ, List(dom.top))
    assertDomT(plus_oneMore_oneMore)(analysis.analyze(Call('plus_0_0), dom.liftApp('_, List(atLeastOne, atLeastOne)), DEFS))
  }

  val plus_two_top: V
  test(s"$prefix: plus two top") {
    assertDomT(plus_two_top)(analysis.analyze(Call('plus_0_0), dom.liftApp('_, List(lift(2), dom.top)), DEFS))
  }
}
