package org.sugarj.sweettooth.stratego.analysis.v1_d2

import org.sugarj.sweettooth.stratego.Syntax.Cons
import org.sugarj.sweettooth.stratego.analysis.AnalyzeRegexToJavaStringSuite

import scala.language.implicitConversions

/**
* Created by seba on 30/07/14.
*/
class AnalyzeRegexToJavaTest extends AnalyzeRegexToJavaStringSuite with Config {

  val bracket_c = lift("[c]")
  val closing_bracket = lift("]")

  def stripString(t: V): V =
    dom.matchAppPat(Cons('String, 1), t).map(_.head).reduce(dom.join)

  val bracket_top = // [??*
    dom.liftApp('String,
      dom.liftApp('Cons,
        dom.liftLit('['),
        dom.join(
          stripString(closing_bracket),
          dom.liftApp('Cons, dom.top, dom.top))))

  val aList = dom.join(
    dom.liftApp('Nil),
    dom.liftApp('Cons, dom.top, dom.top)
  )

//  val ce2str_range_top =
//    dom.join(
//      dom.liftApp('String, dom.liftApp('Cons, dom.top, dom.top)),
//      dom.liftApp('String, dom.liftApp('Cons, dom.liftLit('-'), dom.top)))

  val ce2str_lit_top = dom.top
  val ce2str_range_top =
    dom.liftApp('String,
      dom.liftApp('Cons, dom.liftLit('['),
        dom.liftApp('Cons, dom.join(dom.top, dom.liftLit('-')),
          dom.join(
            stripString(closing_bracket),
            dom.liftApp('Cons, dom.top, dom.top)))))

//  1{Cons(4{"]", "[", ".", "\\"}, 2{Nil(), Cons(6{"]", ".", "-", "[", "&", "\\"}, 2{Nil(), Cons(6{"]", ".", "-", "[", "&", "\\"}, 2{Nil(), Cons(6{"]", ".", "-", "[", "&", "\\"}, 2{Nil(), Cons(6{"]", ".", "-", "[", "&", "\\"}, 2{Nil(), Cons(6{"]", ".", "-", "[", "&", "\\"}, 2{Nil(), Cons(6{"]", ".", "-", "[", "&", "\\"}, 2{Nil(), Cons(6{"]", ".", "-", "[", "&", "\\"}, 2{Nil(), Cons(6{"]", ".", "-", "[", "&", "\\"}, 2{Nil(), Cons(6{"]", ".", "-", "[", "&", "\\"}, 2{Nil(), Cons(6{"]", ".", "-", "[", "&", "\\"}, 2{Nil(), Cons(6{"]", ".", "-", "[", "&", "\\"}, 2{Nil(), Cons(6{"]", ".", "-", "[", "&", "\\"}, 2{Nil(), Cons(6{"]", ".", "-", "[", "&", "\\"}, 2{Nil(), Cons(6{"]", ".", "-", "[", "&", "\\"}, 2{Nil(), Cons(6{"]", ".", "-", "[", "&", "\\"}, 2{Nil(), Cons(3{"]", "&", "-"}, 2{Nil(), Cons(2{"]", "&"}, 2{Nil(), Cons(2{"]", "&"}, 2{Nil(), Cons(2{"]", "&"}, 2{Nil(), Cons(2{"]", "&"}, 2{Nil(), Cons(1{"]"}, 2{Nil(), Cons(1{"]"}, 2{Nil(), Cons(1{"]"}, 2{Nil(), Cons(1{"]"}, 2{Nil(), Cons(1{"]"}, 1{Nil()})})})})})})})})})})})})})})})})})})})})})})})})})})})}

  lazy val ce2str_negation_top_negated_part = List(
    dom.liftApp('Cons, dom.top, dom.top),
    stripString(ce2str_range_top),
    stripString(closing_bracket),
    stripString(ce2str_predefined_dot_top),
    stripString(ce2str_predefined_other_top)
  ).reduce(dom.join)

  lazy val ce2str_negation_top =
    dom.liftApp('String,
      dom.liftApp('Cons, dom.liftLit('['),
        dom.liftApp('Cons, dom.liftLit('^'),
          ce2str_negation_top_negated_part)))
  val ce2str_union_top =
    dom.liftApp('String,
      dom.liftApp('Cons, dom.liftLit('['),
        dom.liftApp('Cons, dom.top, dom.top)))
  val ce2str_intersection_top =
    dom.liftApp('String,
      dom.liftApp('Cons, dom.liftLit('['),
        dom.liftApp('Cons, dom.top,
          dom.liftApp('Cons, dom.top, dom.top))))
  val ce2str_predefined_dot_top =
    dom.liftApp('String,
      dom.liftApp('Cons, dom.liftLit('.'), dom.liftApp('Nil)))
  val ce2str_predefined_other_top =
    dom.liftApp('String,
      dom.liftApp('Cons, dom.liftLit('\\'), dom.top))

  val regex_top =
    dom.liftApp('Lit,
      dom.liftApp('String,
        dom.liftApp('Cons,
          dom.liftApp('Chars,
            dom.liftApp('String, dom.top)),
          dom.liftApp('Nil))))
}
