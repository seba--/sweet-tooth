package org.sugarj.sweettooth.stratego.analysis

import org.sugarj.sweettooth.stratego.Semantics._
import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.analysis.base.Analysis
import org.sugarj.sweettooth.stratego.analysis.domain.{Domain, d1_PowersetDomain}
import org.sugarj.sweettooth.stratego.analysis.v1.v1Analysis
import org.sugarj.sweettooth.stratego.lib.List._
import org.sugarj.sweettooth.stratego.lib.Num

import scala.language.implicitConversions

/**
* Created by seba on 30/07/14.
*/
abstract class AnalyzeListSuite extends AnalysisSuite {
  val prefix = this.getClass.getPackage.getName.substring(getClass.getPackage.getName.lastIndexOf('.') + 1)

  type V
  type D <: Domain[V]
  val dom: D

  val analysis: Analysis[V,D]

  def lift(t: Trm) = dom.lift(t)

  implicit def mkListTrm(l: List[Trm]) = eval(mkList(l), Trm.App('Foo, scala.List()), DEFS)
  def mkListOfLength(n: Int): scala.List[Trm] =
    if (n == 0)
      scala.List()
    else
      Trm.App(Symbol(s"Elem_${n-1}"))::mkListOfLength(n-1)

  val nil: V
  test(s"$prefix: nil") {
    assertDomT(nil)(analysis.analyze(Call('nil), lift(Trm.App('Foo)), DEFS))
  }

  val cons1: V
  test(s"$prefix: cons1") {
    assertDomT(cons1)(
      analysis.analyze(Call('cons), lift(Trm.App('_, Trm.App('Foo), Trm.App('Nil))), DEFS))
  }

  def cons(l: List[Trm]): V
  test(s"$prefix: cons") {
    for (i <- 1 to 20) {
      val l = mkListOfLength(i)
      assertDomT(cons(l))(
        analysis.analyze(Call('cons), lift(Trm.App('_, Trm.App('Foo), l)), DEFS))
    }
  }

  val cons_top_Nil: V
  test(s"$prefix: cons top nil") {
    assertDomT(cons_top_Nil)(
      analysis.analyze(Call('cons), dom.liftApp('_, dom.top, dom.liftApp('Nil)), DEFS))
  }

  val cons_Zero_top: V
  test(s"$prefix: cons Zero top") {
    assertDomT(cons_Zero_top)(
      analysis.analyze(Call('cons), dom.liftApp('_, dom.liftApp('Nil), dom.top), DEFS))
  }

  val pair_to_list_top_top: V
  val pair_to_list_top_Zero: V
  val pair_to_list_Zero_top: V
  val pair_to_list_Zero_One: V

  test(s"$prefix: pair to list") {
    val pairToList = Seqs(
      ??('_@@('x, 'y)),
      !!('_@@('y, 'Nil@@())),
      Call('cons),
      ??('xs),
      !!('_@@('x, 'xs)),
      Call('cons))

    assertDomT(pair_to_list_top_top)(
      analysis.analyze(pairToList, dom.liftApp('_, dom.top, dom.top), DEFS))
    assertDomT(pair_to_list_top_Zero)(
      analysis.analyze(pairToList, dom.liftApp('_, dom.top, dom.liftApp('Zero)), DEFS))
    assertDomT(pair_to_list_Zero_top)(
      analysis.analyze(pairToList, dom.liftApp('_, dom.liftApp('Zero), dom.top), DEFS))
    assertDomT(pair_to_list_Zero_One)(
      analysis.analyze(pairToList, dom.liftApp('_, dom.liftApp('Zero), dom.liftApp('One)), DEFS))
  }

  def map(l: List[Trm]): V
  test(s"$prefix: map") {
    for (i <- 0 to 20) {
      val l = mkListOfLength(i)
      val s = Scoped('x, Seq(Match('x), Build('_@@('Zero@@(), 'x))))
      assertDomT(map(l))(
        analysis.analyze(Call('map, scala.List(s), scala.List()), lift(l), DEFS ++ Num.DEFS))
    }
  }

  val map_top: V
  test(s"$prefix: map top") {
    val s = Scoped('x, Seq(Match('x), Build('Zero@@())))
    val result = analysis.analyze(Call('map, scala.List(s), scala.List()), dom.top, DEFS ++ Num.DEFS)
    assertDomT(map_top)(result)
  }
}
