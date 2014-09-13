package org.sugarj.sweettooth.stratego.analysis

import org.sugarj.sweettooth.stratego.Semantics._
import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.lib.Num
import org.sugarj.sweettooth.stratego.lib.Num._

import scala.language.implicitConversions

/**
* Created by seba on 30/07/14.
*/
abstract class AnalyzeNumSuite extends AnalysisSuite {
  val lib = Num

  implicit def mkNatTrm(n: Int) = eval(mkNat(n), Trm.App('Foo), DEFS)

  val zero: V
  test_analysis("zero")(Call('zero_0_0), lift(Trm.App('Foo)))(zero)

  val zero_top: V
  test_analysis("zero top")(Call('zero_0_0), dom.top)(zero_top)

  def succ(i: Int): V
  for (i <- 1 to 20)
    test_analysis(s"succ $i")(Call('succ_0_0), lift(i))(succ(i))

  def succ_prefix(i: Int): V
  for (i <- 1 to 20) {
    lazy val input = (1 to (i-1)).foldLeft(dom.top)((t,_) => dom.liftApp('Succ, t))
    test_analysis(s"succ prefix $i")(Call('succ_0_0), input)(succ_prefix(i))
  }

  def plus(m: Int, n: Int): V
  for {m <- 1 to 5;
       n <- 1 to 5}
    test_analysis(s"plus $m $n")(Call('plus_0_0), lift(Trm.App('_, m, n)))(plus(m, n))

  val plus_top_top: V
  test_analysis("plus top top")(Call('plus_0_0), dom.liftApp('_, List(dom.top, dom.top)))(plus_top_top)

  val plus_zero_top: V
  test_analysis("plus zero top")(Call('plus_0_0), dom.liftApp('_, List(lift(0), dom.top)))(plus_zero_top)

  val plus_one_top: V
  test_analysis("plus one top")(Call('plus_0_0), dom.liftApp('_, List(lift(1), dom.top)))(plus_one_top)

  val plus_oneMore_top: V
  lazy val atLeastOne = dom.liftApp('Succ, List(dom.top))
  test_analysis("plus >=one top")(Call('plus_0_0), dom.liftApp('_, List(atLeastOne, dom.top)))(plus_oneMore_top)

  val plus_top_one: V
  test_analysis("plus top one")(Call('plus_0_0), dom.liftApp('_, List(dom.top, lift(1))))(plus_top_one)

  val plus_top_oneMore: V
  test_analysis("plus top >=one")(Call('plus_0_0), dom.liftApp('_, List(dom.top, atLeastOne)))(plus_top_oneMore)

  val plus_oneMore_oneMore: V
  test_analysis("plus >=one >=one")(Call('plus_0_0), dom.liftApp('_, List(atLeastOne, atLeastOne)))(plus_oneMore_oneMore)

  val plus_two_top: V
  test_analysis("plus two top")(Call('plus_0_0), dom.liftApp('_, List(lift(2), dom.top)))(plus_two_top)
}
