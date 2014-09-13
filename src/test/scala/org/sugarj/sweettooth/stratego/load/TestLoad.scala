package org.sugarj.sweettooth.stratego.load

import org.scalatest._

import scala.language.implicitConversions

/**
 * Created by seba on 30/07/14.
 */
class TestLoad extends FunSuite {

  import org.sugarj.sweettooth.stratego.analysis.AnalyzeRegexToJavaStringSuite.regexLib

  test ("load RegexAsString") {
    assert(regexLib.DEFS.isDefinedAt('regexAsString_0_0))
    assert(regexLib.DEFS.isDefinedAt('r2str_0_0))
    assert(regexLib.DEFS.isDefinedAt('ce2str_0_0))
    assert(regexLib.DEFS.isDefinedAt('bracket_0_0))
    assert(regexLib.DEFS.isDefinedAt('conc_strings_0_0))
    assert(regexLib.DEFS.isDefinedAt('string_replace_0_2))
    assert(regexLib.DEFS.isDefinedAt('elem_0_0))
  }

}
