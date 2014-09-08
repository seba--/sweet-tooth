package org.sugarj.sweettooth.stratego.analysis

import org.scalatest._
import org.sugarj.sweettooth.stratego.Semantics._
import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.lib.List._
import org.sugarj.sweettooth.stratego.lib.Num

import scala.language.implicitConversions

/**
 * Created by seba on 30/07/14.
 */
class AnalyzeListTest extends FunSuite {

  val analysis = new Analyze(PowersetDomain)
  import analysis.dom
  def lift(t: Trm) = dom.lift(t)

  implicit def mkListTrm(l: List[Trm]) = eval(mkList(l), Trm.App('Foo, scala.List()), DEFS)
  def mkListOfLength(n: Int): scala.List[Trm] =
    if (n == 0)
      scala.List()
    else
      Trm.App(Symbol(s"Foo$n"))::mkListOfLength(n-1)

  def assertDomT(expected: dom.T)(actual: dom.T) = assertResult(expected)(actual)

  test("nil") {
    assertDomT(lift(Trm.App('Nil)))(analysis.analyze(Call('nil), lift(Trm.App('Foo)), DEFS))
  }

  test("cons1") {
    assertDomT(
      lift(Trm.App('Cons, Trm.App('Foo), Trm.App('Nil))))(
      analysis.analyze(Call('cons), lift(Trm.App('_, Trm.App('Foo), Trm.App('Nil))), DEFS))
  }

  test("cons") {
    for (i <- 1 to 20) {
      val l = mkListOfLength(i)
      assertDomT(lift(Trm.App('Foo)::l))(analysis.analyze(Call('cons), lift(Trm.App('_, Trm.App('Foo), l)), DEFS))
    }
  }

  test(s"map") {
    for (i <- 1 to 20) {
      val l = mkListOfLength(i)
      val s = Build('Zero@@())
      assertDomT(
        lift(l.map(_ => Trm.App('Zero))))(
        analysis.analyze(Call('map, scala.List(s), scala.List()), lift(l), DEFS ++ Num.DEFS))
    }
  }
}
