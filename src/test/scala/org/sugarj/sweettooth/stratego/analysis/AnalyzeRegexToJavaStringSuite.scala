package org.sugarj.sweettooth.stratego.analysis

import org.sugarj.sweettooth.stratego.Semantics._
import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.lib
import org.sugarj.sweettooth.stratego.load.Load

import scala.language.implicitConversions

/**
* Created by seba on 30/07/14.
*/
object AnalyzeRegexToJavaStringSuite {
  val baseLib = lib.Generic + lib.Num + lib.List + lib.String
  val regexLib = Load.load(s"${AnalysisSuite.RESOURCES}/regex/RegexAsString.str", List(AnalysisSuite.RESOURCES), baseLib)
}

abstract class AnalyzeRegexToJavaStringSuite extends AnalysisSuite {

  val baseLib = AnalyzeRegexToJavaStringSuite.regexLib
  
  implicit def mkStringTrm(s: String) = eval(lib.String.buildString(s), Trm.App('Foo), baseLib.DEFS)

  val bracket_top: V
  test_strat("bracket", "top")(dom.top)(bracket_top)

  val bracket_c: V
  test_strat("bracket", "c")(lift("c"))(bracket_c)

  val ce2str_lit_top: V
  test_strat("ce2str-lit", "top")(dom.top)(ce2str_lit_top)
  val ce2str_range_top: V
  test_strat("ce2str-range", "top")(dom.top)(ce2str_range_top)
  val ce2str_negation_top: V
  test_strat("ce2str-negation", "top")(dom.top)(ce2str_negation_top)

  val ce2str_union_top: V
  test_strat("ce2str-union", "top")(dom.top)(ce2str_union_top)
  val ce2str_intersection_top: V
  test_strat("ce2str-intersection", "top")(dom.top)(ce2str_intersection_top)

  val ce2str_predefined_dot_top: V
  test_strat("ce2str-predefined-dot", "top")(dom.top)(ce2str_predefined_dot_top)
  val ce2str_predefined_other_top: V
  test_strat("ce2str-predefined-other", "top")(dom.top)(ce2str_predefined_other_top)

  val ce2str_top: V // = List(ce2str_lit_top, ce2str_range_top, ce2str_negation_top, ce2str_union_top, ce2str_intersection_top, ce2str_predefined_dot_top, ce2str_predefined_other_top)
  test_strat("ce2str", "top")(dom.top)(ce2str_top)


//  (r2str-lit1 <+ r2str-lit2) <+
//    r2str-many <+
//    r2str-many1 <+
//    r2str-seq <+
//    r2str-alt <+
//    r2str-group <+
//    r2str-predef

  val r2str_lit1_top: V
  test_strat("r2str-lit1", "top")(dom.top)(r2str_lit1_top)
  val r2str_lit2_top: V
  test_strat("r2str-lit2", "top")(dom.top)(r2str_lit2_top)
  val r2str_lit_top: V
  test_strat("r2str-lit", "top")(dom.top)(r2str_lit_top)


  val r2str_ccexp_top: V
  test_strat("r2str-ccexp", "top")(dom.top)(r2str_ccexp_top)

  val r2str_option_top: V
  test_strat("r2str-option", "top")(dom.top)(r2str_option_top)

  val r2str_many_top: V
  test_strat("r2str-many", "top")(dom.top)(r2str_many_top)

  val r2str_many1_top: V
  test_strat("r2str-many1", "top")(dom.top)(r2str_many1_top)

  val r2str_seq_top: V
  test_strat("r2str-seq", "top")(dom.top)(r2str_seq_top)

  val r2str_alt_top: V
  test_strat("r2str-alt", "top")(dom.top)(r2str_alt_top)

  val r2str_group_top: V
  test_strat("r2str-group", "top")(dom.top)(r2str_group_top)

  val r2str_predef_top: V
  test_strat("r2str-predef", "top")(dom.top)(r2str_predef_top)

  val r2str_top: V
  test_strat("r2str", "top")(dom.top)(r2str_top)


  val regexAsString_top: V
  test_strat("regexAsString", "top")(dom.top)(regexAsString_top)


  def string(v: V) = dom.liftApp('_String, v)
  def unstring(t: V): V = dom.matchAppPat(Cons('_String, 1), t).map(_.head).reduce(dom.join)

  def a_at_end(current: V, end: V, stack:List[(V,V)]=List()): V = {
    if (stack.contains((current, end)))
      return dom.top

    var res = dom.bottom
    if (!dom.matchAppPat(Cons('_Nil, 0), current).isEmpty)
      res = dom.join(res, end)

    var args = List(dom.bottom, dom.bottom)
    for (argList <- dom.matchAppPat(Cons('_Cons, 2), current))
      args = args.zip(argList).map(p => dom.join(p._1, p._2))
    val hd = args(0)
    val tl = args(1)

    val cons = dom.liftApp('_Cons, hd, a_at_end(tl, end, (current,end)::stack))
    val res2 = dom.join(res, cons)
    res2
  }
}
