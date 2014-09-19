package org.sugarj.sweettooth.stratego.analysis

import org.sugarj.sweettooth.stratego
import org.sugarj.sweettooth.stratego.Semantics._
import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.lib.List._

import scala.language.implicitConversions

/**
* Created by seba on 30/07/14.
*/
abstract class AnalyzeListSuite extends AnalysisSuite {
  val baseLib = stratego.lib.List

  implicit def mkListTrm(l: List[Trm]) = eval(mkList(l), Trm.App('Foo, scala.List()), DEFS)
  def mkListOfLength(n: Int): scala.List[Trm] =
    if (n == 0)
      scala.List()
    else
      Trm.App(Symbol(s"Elem_${n-1}"))::mkListOfLength(n-1)

  val nil: V
  test_analysis("nil")(Call('nil_0_0), lift(Trm.App('Foo)))(nil)

  val cons1: V
  test_analysis("cons1")(Call('cons_0_0), lift(Trm.App('_, Trm.App('Foo), Trm.App('Nil))))(cons1)

  def cons(l: List[Trm]): V
  for (i <- 1 to 20) {
    val l = mkListOfLength(i)
    test_analysis(s"cons $i")(Call('cons_0_0), lift(Trm.App('_, Trm.App('Foo), l)))(cons(l))
  }

  val cons_top_Nil: V
  test_analysis("cons top nil")(Call('cons_0_0), dom.liftApp('_, dom.top, dom.liftApp('Nil)))(cons_top_Nil)

  val cons_Zero_top: V
  test_analysis("cons Zero top")(Call('cons_0_0), dom.liftApp('_, dom.liftApp('Nil), dom.top))(cons_Zero_top)

  val pair_to_list_top_top: V
  val pair_to_list_top_Zero: V
  val pair_to_list_Zero_top: V
  val pair_to_list_Zero_One: V

  val pairToList = Seqs(
    ??('_@@('x, 'y)),
    !!('_@@('y, 'Nil@@())),
    Call('cons_0_0),
    ??('xs),
    !!('_@@('x, 'xs)),
    Call('cons_0_0))
  test_analysis("pair to list top top")(pairToList, dom.liftApp('_, dom.top, dom.top))(pair_to_list_top_top)
  test_analysis("pair to list top zero")(pairToList, dom.liftApp('_, dom.top, dom.liftApp('Zero)))(pair_to_list_top_Zero)
  test_analysis("pair to list zero top")(pairToList, dom.liftApp('_, dom.liftApp('Zero), dom.top))(pair_to_list_Zero_top)
  test_analysis("pair to list zero one")(pairToList, dom.liftApp('_, dom.liftApp('Zero), dom.liftApp('One)))(pair_to_list_Zero_One)

  def map(l: List[Trm]): V
  for (i <- 0 to 20) {
    val s = Scoped('x, Seq(Match('x), Build('_ @@('Zero @@(), 'x))))
    val l = mkListOfLength(i)
    test_analysis(s"map $i")(Call('map_1_0, scala.List(s), scala.List()), lift(l))(map(l))
  }

  val map_top: V
  val s = Scoped('x, Seq(Match('x), Build('Zero@@())))
  test_analysis("map top")(Call('map_1_0, scala.List(s), scala.List()), dom.top)(map_top)
}
