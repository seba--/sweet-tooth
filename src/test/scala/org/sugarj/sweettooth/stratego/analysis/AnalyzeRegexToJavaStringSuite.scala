package org.sugarj.sweettooth.stratego.analysis

import org.sugarj.sweettooth.stratego.Semantics.Fail
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

  val lib = AnalyzeRegexToJavaStringSuite.regexLib

  val regex_top: V
  test_strat("main", "top")(dom.top)(regex_top)
}
