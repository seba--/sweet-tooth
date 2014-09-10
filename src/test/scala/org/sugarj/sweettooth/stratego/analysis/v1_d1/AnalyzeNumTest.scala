package org.sugarj.sweettooth.stratego.analysis.v1_d1

import org.scalatest._
import org.sugarj.sweettooth.stratego.Semantics._
import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.analysis.AnalyzeNumSuite
import org.sugarj.sweettooth.stratego.analysis.base.{BasicStack, StoreTrait}
import org.sugarj.sweettooth.stratego.analysis.domain.d1_PowersetDomain
import org.sugarj.sweettooth.stratego.analysis.v1.v1Analysis
import org.sugarj.sweettooth.stratego.lib.Num._

import scala.language.implicitConversions

/**
 * Created by seba on 30/07/14.
 */
class AnalyzeNumTest extends AnalyzeNumSuite with Config {

  val zero = lift(0)
  val zero_top = lift(0)
  def succ(i: Int) = lift(i+1)
  def succ_prefix(i: Int) = {
    var expected = dom.top
    for (i <- 1 to i)
      expected = dom.liftApp('Succ, List(expected))
    expected
  }
  def plus(m: Int, n: Int) = lift(m + n)
  val plus_top_top = dom.top
  val plus_zero_top = dom.top
  val plus_one_top = dom.liftApp('Succ, List(dom.top))
  val plus_oneMore_top = dom.liftApp('Succ, List(dom.top))
  val plus_top_one = dom.liftApp('Succ, List(dom.top))
  val plus_top_oneMore = dom.liftApp('Succ, List(dom.top))
  val plus_oneMore_oneMore = dom.liftApp('Succ, List(dom.liftApp('Succ, List(dom.top))))
  val plus_two_top = dom.liftApp('Succ, List(dom.liftApp('Succ, List(dom.top))))
}
