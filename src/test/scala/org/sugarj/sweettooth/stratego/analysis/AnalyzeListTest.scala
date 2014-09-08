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
      Trm.App(Symbol(s"Elem_${n-1}"))::mkListOfLength(n-1)

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
      assertDomT(
        lift(Trm.App('Foo)::l))(
        analysis.analyze(Call('cons), lift(Trm.App('_, Trm.App('Foo), l)), DEFS))
    }
  }

  test("cons top nil") {
    assertDomT(
      dom.liftApp('Cons, dom.top, dom.liftApp('Nil)))(
      analysis.analyze(Call('cons), dom.liftApp('_, dom.top, dom.liftApp('Nil)), DEFS))
  }

  test("cons Zero top") {
    assertDomT(
      dom.liftApp('Cons, dom.liftApp('Nil), dom.top))(
      analysis.analyze(Call('cons), dom.liftApp('_, dom.liftApp('Nil), dom.top), DEFS))
  }

  test("pair to list") {
    val pairToList = Seqs(
      ??('_@@('x, 'y)),
      !!('_@@('y, 'Nil@@())),
      Call('cons),
      ??('xs),
      !!('_@@('x, 'xs)),
      Call('cons))

    assertDomT(
      dom.liftApp('Cons, dom.top, dom.liftApp('Cons, dom.top, dom.liftApp('Nil))))(
      analysis.analyze(pairToList, dom.liftApp('_, dom.top, dom.top), DEFS))
    assertDomT(
      dom.liftApp('Cons, dom.top, dom.liftApp('Cons, dom.liftApp('Zero), dom.liftApp('Nil))))(
      analysis.analyze(pairToList, dom.liftApp('_, dom.top, dom.liftApp('Zero)), DEFS))
    assertDomT(
      dom.liftApp('Cons, dom.liftApp('Zero), dom.liftApp('Cons, dom.top, dom.liftApp('Nil))))(
      analysis.analyze(pairToList, dom.liftApp('_, dom.liftApp('Zero), dom.top), DEFS))
    assertDomT(
      dom.liftApp('Cons, dom.liftApp('Zero), dom.liftApp('Cons, dom.liftApp('One), dom.liftApp('Nil))))(
      analysis.analyze(pairToList, dom.liftApp('_, dom.liftApp('Zero), dom.liftApp('One)), DEFS))
  }

  test(s"map") {
    for (i <- 1 to 20) {
      val l = mkListOfLength(i)
      val s = Seq(Match('x), Build('_@@('Zero@@(), 'x)))
      assertDomT(
        lift(l.map(elem => Trm.App('_, Trm.App('Zero), Trm.App(Symbol(s"Elem_${l.length - l.indexOf(elem) - 1}"))))))(
        analysis.analyze(Call('map, scala.List(s), scala.List()), lift(l), DEFS ++ Num.DEFS))
    }
  }
}
