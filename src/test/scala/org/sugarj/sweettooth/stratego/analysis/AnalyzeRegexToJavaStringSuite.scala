package org.sugarj.sweettooth.stratego.analysis

import org.sugarj.sweettooth.stratego.Semantics._
import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.lib
import org.sugarj.sweettooth.stratego.lib.Num._
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

  val ce2str_range_top: V
    test_strat("ce2str-range", "top")(dom.top)(ce2str_range_top)

  val ce2str_top: V
  test_strat("ce2str", "top")(dom.top)(ce2str_top)

  val regex_top: V
//  test_strat("main", "top")(dom.top)(regex_top)
}
