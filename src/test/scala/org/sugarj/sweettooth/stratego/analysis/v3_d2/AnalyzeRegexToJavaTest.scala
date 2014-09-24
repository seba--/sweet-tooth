package org.sugarj.sweettooth.stratego.analysis.v3_d2

import org.sugarj.sweettooth.stratego.analysis.AnalyzeRegexToJavaStringSuite

import scala.language.implicitConversions

/**
* Created by seba on 30/07/14.
*/
class AnalyzeRegexToJavaTest extends AnalyzeRegexToJavaStringSuite with Config {

  override def a_at_end(current: V, end: V, stack:List[(V,V)]=List()): V = {
    if (dom.compare(current, dom.liftApp('_Nil)) ||
      dom.compare(current, dom.liftApp('_Cons, dom.top, dom.top)))
      super.a_at_end(current, end, stack)
    else
      dom.liftApp('_Conc, current, end)
  }

  val bracket_c = lift("[c]")
  def closing_bracket_rec(v: V) = dom.liftApp('_Cons, dom.join(dom.liftLit(']'), dom.top), v)
  val closing_bracket = string(dom.liftApp('_Cons, dom.liftLit(']'), dom.liftApp('_Nil)))

  def bracket_top_rec(v: V) =
    string(
      dom.liftApp('_Cons,
        dom.liftLit('['),
        a_at_end(
          v,
          dom.liftApp('_Cons, dom.liftLit(']'), dom.liftApp('_Nil)))))
  lazy val bracket_top = bracket_top_rec(dom.top)

  val ce2str_lit_top = dom.top

  def ce2str_range_top_rec(v1: V, v2: V) =
    bracket_top_rec(
      a_at_end(
        unstring(v1),
        dom.liftApp('_Cons,
          dom.liftLit('-'),
          unstring(v2))))
  lazy val ce2str_range_top = ce2str_range_top_rec(dom.top, dom.top)

  def ce2str_negation_top_rec(v: V) = bracket_top_rec(dom.liftApp('_Cons, dom.liftLit('^'), unstring(v)))
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
        dom.liftApp('_Cons, dom.liftLit('&'),
          dom.liftApp('_Cons, dom.liftLit('&'),
            unstring(v2)))))
  lazy val ce2str_intersection_top = {
    val skip = (v:V) => ce2str_intersection_top_rec(v, v)
    val rec = ce2str_top_rec_skip(skip)(dom.top)
    ce2str_intersection_top_rec(rec, rec)
  }

  val ce2str_predefined_dot_top = string(dom.liftApp('_Cons, dom.liftLit('.'), dom.liftApp('_Nil)))
  def ce2str_predefined_other_top_rec(v: V) = string(dom.liftApp('_Cons, dom.liftLit('\\'), v))
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


  val r2str_lit1_top = dom.top

  val r2str_lit2_top =
    dom.liftApp('_String,
      dom.join(
        dom.liftApp('_Nil),
        dom.liftApp('_Cons, dom.mliftLit('\\'), dom.mliftApp('_Cons, dom.liftLit('\\'), dom.top))))

  val r2str_lit_top = dom.join(r2str_lit1_top, r2str_lit2_top)

  val r2str_ccexp_top = string(dom.liftApp('_Cons, dom.liftLit('['), a_at_end(unstring(ce2str_top), unstring(lift("]")))))

  def r2str_option_top_rec(v: V) = string(dom.liftApp('_Cons, dom.liftLit('?'), unstring(v)))
  lazy val r2str_option_top = {
    val rec = r2str_top_rec_skip(r2str_option_top_rec)(dom.top)
    r2str_option_top_rec(rec)
  }

  def r2str_many_top_rec(v: V) = string(a_at_end(unstring(v), unstring(lift("*"))))
  lazy val r2str_many_top = {
    val rec = r2str_top_rec_skip(r2str_many_top_rec)(dom.top)
    r2str_many_top_rec(rec)
  }

  def r2str_many1_top_rec(v: V) = string(a_at_end(unstring(v), unstring(lift("+"))))
  lazy val r2str_many1_top = {
    val rec = r2str_top_rec_skip(r2str_many1_top_rec)(dom.top)
    r2str_many1_top_rec(rec)
  }

  def r2str_seq_top_rec(v1: V, v2: V) = string(a_at_end(unstring(v1), unstring(v2)))
  lazy val r2str_seq_top = {
    val skip = (v:V) => r2str_seq_top_rec(v, v)
    val rec = r2str_top_rec_skip(skip)(dom.top)
    r2str_seq_top_rec(rec, rec)
  }

  def r2str_alt_top_rec(v1: V, v2: V) = string(a_at_end(unstring(v1), dom.liftApp('_Cons, dom.liftLit('|'), unstring(v2))))
  lazy val r2str_alt_top = {
    val skip = (v:V) => r2str_alt_top_rec(v, v)
    val rec = r2str_top_rec_skip(skip)(dom.top)
    r2str_alt_top_rec(rec, rec)
  }

  def r2str_group_top_rec(v: V) = string(dom.liftApp('_Cons, dom.liftLit('('), a_at_end(unstring(v), unstring(lift(")")))))
  lazy val r2str_group_top = {
    val rec = r2str_top_rec_skip(r2str_group_top_rec)(dom.top)
    r2str_group_top_rec(rec)
  }

  val r2str_predef_top = string(dom.liftApp('_Cons, dom.liftLit('\\'), dom.top))

  def r2str_top_rec(v: V) = r2str_top_rec_skips(List())(v)
  def r2str_top_rec_skip(skip: V=>V)(v: V) = r2str_top_rec_skips(List(skip))(v)
  def r2str_top_rec_skips(skip: List[V=>V])(v: V): V = List(
    r2str_lit_top,
    r2str_ccexp_top,
    r2str_option_top_rec(v),
    r2str_many_top_rec(v),
    r2str_many1_top_rec(v),
    r2str_seq_top_rec(v, v),
    r2str_alt_top_rec(v, v),
    r2str_group_top_rec(v),
    r2str_predef_top
  ).diff(skip.map(_(v))).reduce(dom.join)
  val r2str_top = r2str_top_rec(dom.top)



  val regexAsString_top =
    dom.liftApp('Lit,
      dom.liftApp('String,
        dom.liftApp('_Cons,
          dom.liftApp('Chars, r2str_top),
          dom.liftApp('_Nil))))


//  {'Lit->(
//    {'String->(
//      {'_Cons->(
//        {'Chars->(
//          ?{'_String->(
//            {'_Conc->(
//              ?,
//              ?{'_Cons->(
//                {'*', '+', '|'},
//                ?{'_Nil->()})}),
//             '_Cons->(
//               ?{'(', '?', '[', '\'},
//               ?{'_Conc->(
//                   ?{'_Cons->(
//                     {'.', '[', '\'},
//                     ?{'_Conc->(
//                       {'_Conc->(
//                         ?,
//                         ?{'_Cons->(
//                           {'&', '-'},
//                           ?{'_Cons->({'&'}, ?)})})},
//                       {'_Cons->({']'}, {'_Nil->()})}),
//                       '_Cons->(
//                         {'^'},
//                         {'_Conc->(
//                           ?,
//                           {'_Cons->({']'}, {'_Nil->()})})}),
//                       '_Nil->()})},
//                   {'_Cons->({')', ']'}, {'_Nil->()})}),
//                 '_Cons->({'\'}, ?)}),
//             '_Nil->()})})},
//        {'_Nil->()})})})}
}
