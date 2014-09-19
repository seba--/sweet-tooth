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
//  test_strat("ce2str-union", "top")(dom.top)(ce2str_union_top)
  val ce2str_intersection_top: V
//  test_strat("ce2str-intersection", "top")(dom.top)(ce2str_intersection_top)
  val ce2str_predefined_dot_top: V
//  test_strat("ce2str-predefined-dot", "top")(dom.top)(ce2str_predefined_dot_top)
  val ce2str_predefined_other_top: V
//  test_strat("ce2str-predefined-other", "top")(dom.top)(ce2str_predefined_other_top)

  lazy val ce2str_top = List(ce2str_lit_top, ce2str_range_top, ce2str_negation_top, ce2str_union_top, ce2str_intersection_top, ce2str_predefined_dot_top, ce2str_predefined_other_top)
//  test_strat("ce2str", "top")(dom.top)(ce2str_top.reduce(dom.join))

  val regex_top: V
//  test_strat("main", "top")(dom.top)(regex_top)
}
