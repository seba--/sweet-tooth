package org.sugarj.sweettooth.stratego.analysis.v1_d2

import org.sugarj.sweettooth.stratego.Syntax.Cons
import org.sugarj.sweettooth.stratego.analysis.AnalyzeRegexToJavaStringSuite

import scala.language.implicitConversions

/**
* Created by seba on 30/07/14.
*/
class AnalyzeRegexToJavaTest extends AnalyzeRegexToJavaStringSuite with Config {

  def loop(f: V => V, v: V, n: Int) = {
    var res = v
    (1 to n) foreach (_ => res = f(res))
    res
  }

  def string(v: V) = dom.liftApp('String, v)
  def unstring(t: V): V = dom.matchAppPat(Cons('String, 1), t).map(_.head).reduce(dom.join)

  val bracket_c = lift("[c]")
  def closing_bracket_rec(v: V) = dom.liftApp('Cons, dom.join(dom.liftLit(']'), dom.top), v)
  val closing_bracket = string(dom.liftApp('Cons, dom.liftLit(']'), dom.liftApp('Nil)))

  def bracket_top_rec(v: V) =
    string(
      dom.liftApp('Cons,
        dom.liftLit('['),
        a_at_end(
          v,
          dom.liftApp('Cons, dom.liftLit(']'), dom.liftApp('Nil)))))
  lazy val bracket_top = bracket_top_rec(dom.top)

  val ce2str_lit_top = dom.top

  def ce2str_range_top_rec(v1: V, v2: V) =
    bracket_top_rec(
      a_at_end(
        unstring(v1),
        dom.liftApp('Cons,
          dom.liftLit('-'),
          unstring(v2))))
  lazy val ce2str_range_top = ce2str_range_top_rec(dom.top, dom.top)

  def ce2str_negation_top_rec(v: V) = bracket_top_rec(dom.liftApp('Cons, dom.liftLit('^'), unstring(v)))
  lazy val ce2str_negation_top = {
    val skip = (v:V) => ce2str_negation_top_rec(v)
    ce2str_negation_top_rec(ce2str_top_rec_skip(skip)(dom.top))
  }

  def ce2str_union_top_rec(v1: V, v2: V) = bracket_top_rec(a_at_end(unstring(v1), unstring(v2)))
  lazy val ce2str_union_top = {
    val skip = (v:V) => ce2str_union_top_rec(v, v)
    val rec = ce2str_top_rec_skip(skip)(dom.top)
    ce2str_union_top_rec(rec, rec)
  }

  def ce2str_intersection_top_rec(v1: V, v2: V): V =
    bracket_top_rec(
      a_at_end(unstring(v1),
        dom.liftApp('Cons, dom.liftLit('&'),
          dom.liftApp('Cons, dom.liftLit('&'),
            unstring(v2)))))
  lazy val ce2str_intersection_top = {
    val skip = (v:V) => ce2str_intersection_top_rec(v, v)
    val rec = ce2str_top_rec_skip(skip)(dom.top)
    ce2str_intersection_top_rec(rec, rec)
  }

  val ce2str_predefined_dot_top = string(dom.liftApp('Cons, dom.liftLit('.'), dom.liftApp('Nil)))
  def ce2str_predefined_other_top_rec(v: V) = string(dom.liftApp('Cons, dom.liftLit('\\'), v))
  val ce2str_predefined_other_top = ce2str_predefined_other_top_rec(dom.top)


  def ce2str_top_rec(v: V) = ce2str_top_rec_skips(List())(v)
  def ce2str_top_rec_skip(skip: V=>V)(v: V) = ce2str_top_rec_skips(List(skip))(v)
  def ce2str_top_rec_skips(skip: List[V=>V])(v: V): V = List(
    ce2str_lit_top,
    ce2str_range_top_rec(v, v),
    ce2str_negation_top_rec(v),
    ce2str_union_top_rec(v, v),
    ce2str_intersection_top_rec(v, v),
    ce2str_predefined_dot_top,
    ce2str_predefined_other_top_rec(v)
  ).diff(skip.map(_(v))).reduce(dom.join)

  val ce2str_top = ce2str_top_rec(dom.top)

  val regex_top =
    dom.liftApp('Lit,
      dom.liftApp('String,
        dom.liftApp('Cons,
          dom.liftApp('Chars,
            dom.liftApp('String, dom.top)),
          dom.liftApp('Nil))))
}
