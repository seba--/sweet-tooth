package org.sugarj.sweettooth.stratego.analysis.v4_d1

import org.sugarj.sweettooth.stratego.analysis.AnalyzeNumSuite

import scala.language.implicitConversions

/**
 * Created by seba on 30/07/14.
 */
class AnalyzeNumTest extends AnalyzeNumSuite with Config {

  val NAT = {
    val box = dom.makeBox(dom.top)
    val nat = dom.liftApp('Zero) || dom.liftApp('Succ, box)
    box.target = nat
    box.markStable()
    nat
  }
  override lazy val atLeastOne = dom.liftApp('Succ, NAT)

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
  val plus_top_top = dom.mliftApp('Succ, dom.top)
  val plus_zero_top = dom.top
  val plus_one_top = dom.liftApp('Succ, List(dom.top))
  val plus_oneMore_top = dom.liftApp('Succ, List(dom.mliftApp('Succ, dom.top)))
  val plus_top_one = dom.liftApp('Succ, NAT)
  val plus_top_oneMore = dom.liftApp('Succ, NAT)
  val plus_oneMore_oneMore = dom.liftApp('Succ, dom.liftApp('Succ, NAT))
  val plus_two_top = dom.liftApp('Succ, List(dom.liftApp('Succ, List(dom.top))))
}
