//package org.sugarj.sweettooth.stratego.analysis.v2_d1
//
//import org.sugarj.sweettooth.stratego.analysis.AnalyzeRegexToJavaStringSuite
//
//import scala.language.implicitConversions
//
///**
//* Created by seba on 30/07/14.
//*/
//class AnalyzeRegexToJavaTest extends AnalyzeRegexToJavaStringSuite with Config {
//
//  val bracket_top = // [??*
//    dom.liftApp('String,
//      dom.liftApp('Cons, dom.liftLit('['),
//        dom.liftApp('Cons, dom.top, dom.top)))
//
//  val bracket_c = lift("[c]")
//
//  val ce2str_lit_top = dom.top
//  val ce2str_range_top =
//    dom.liftApp('String,
//      dom.liftApp('Cons, dom.liftLit('['),
//        dom.liftApp('Cons, dom.top,
//          dom.liftApp('Cons, dom.top, dom.top))))
//  val ce2str_negation_top = Equals(
//    dom.liftApp('String,
//      dom.liftApp('Cons, dom.liftLit('['),
//        dom.liftApp('Cons, dom.liftLit('^'),
//          dom.liftApp('Cons, dom.top, dom.top)))))
//  val ce2str_union_top =
//    dom.liftApp('String,
//      dom.liftApp('Cons, dom.liftLit('['),
//        dom.liftApp('Cons, dom.top, dom.top)))
//  val ce2str_intersection_top = Equals(
//    dom.liftApp('String,
//      dom.liftApp('Cons, dom.liftLit('['),
//        dom.liftApp('Cons, dom.top,
//          dom.liftApp('Cons, dom.top, dom.top)))))
//  val ce2str_predefined_dot_top =
//    dom.liftApp('String,
//      dom.liftApp('Cons, dom.liftLit('.'), dom.liftApp('Nil)))
//  val ce2str_predefined_other_top =
//    dom.liftApp('String,
//      dom.liftApp('Cons, dom.liftLit('\\'), dom.top))
//
//  // Lit(String([Chars(<r2str> e)]))
//  val regex_top =
//    dom.liftApp('Lit,
//      dom.liftApp('String,
//        dom.liftApp('Cons,
//          dom.liftApp('Chars, dom.top),
//          dom.liftApp('Nil))))
//}
