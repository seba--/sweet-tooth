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

  val conc_top: V
  test_strat("conc", "top")(dom.top)(conc_top)

  val conc_FooBar_top: V
  test_strat("conc", "[Foo,Bar] top")(dom.liftApp('_, lift(List(Trm.App('Foo), Trm.App('Bar))), dom.top))(conc_FooBar_top)

  val conc_top_FooBar: V
  test_strat("conc", "top [Foo,Bar]")(dom.liftApp('_, dom.top, lift(List(Trm.App('Foo), Trm.App('Bar)))))(conc_top_FooBar)

  lazy val conc_topBaz_FooBar: V = dom.liftApp('Cons, dom.top, lift(List(Trm.App('Baz), Trm.App('Foo), Trm.App('Bar))))
  test_strat("conc", "[top, Baz] [Foo,Bar]")(dom.liftApp('_, dom.liftApp('Cons, dom.top, lift(List(Trm.App('Baz)))), lift(List(Trm.App('Foo), Trm.App('Bar)))))(conc_topBaz_FooBar)

  lazy val conc_topBaztop_FooBar: V = dom.liftApp('Cons, dom.top, dom.liftApp('Cons, dom.liftApp('Baz), dom.liftApp('Cons, dom.mliftApp('Foo), dom.mliftApp('Cons, dom.liftApp('Bar), dom.liftApp('Nil)))))
  test_strat("conc", "[top, Baz | top] [Foo,Bar]")(dom.liftApp('_, dom.liftApp('Cons, dom.top, dom.liftApp('Cons, dom.liftApp('Baz), dom.top)), lift(List(Trm.App('Foo), Trm.App('Bar)))))(conc_topBaztop_FooBar)

  lazy val atend_top_FooBar : V = dom.join(dom.liftApp('Cons, dom.liftApp('Foo), dom.liftApp('Cons, dom.liftApp('Bar), dom.liftApp('Nil))), dom.liftApp('Cons, dom.top, dom.top))
  val arg = Pat.App('Cons, Pat.App('Foo), Pat.App('Cons, Pat.App('Bar), Pat.App('Nil)))
  test_analysis("<at-end(|[Foo,Bar])> top")(Call('at_end_1_0, List(Build(arg)), List()), dom.top)(atend_top_FooBar)

  lazy val atend_top_FooBarBaz : V = dom.join(dom.liftApp('Cons, dom.liftApp('Foo), dom.liftApp('Cons, dom.liftApp('Bar), dom.liftApp('Cons, dom.liftApp('Baz), dom.liftApp('Nil)))), dom.liftApp('Cons, dom.top, dom.top))
  val arg2 = Pat.App('Cons, Pat.App('Foo), Pat.App('Cons, Pat.App('Bar), Pat.App('Cons, Pat.App('Baz), Pat.App('Nil))))
  test_analysis("<at-end(|[Foo,Bar,Baz])> top")(Call('at_end_1_0, List(Build(arg2)), List()), dom.top)(atend_top_FooBarBaz)

}
