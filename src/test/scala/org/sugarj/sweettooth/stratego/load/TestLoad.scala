package org.sugarj.sweettooth.stratego.load

import org.scalatest._

import scala.language.implicitConversions

/**
 * Created by seba on 30/07/14.
 */
class TestLoad extends FunSuite {

  val SUGAR_BIN = "/Users/seba/projects/sugar-lang/case-studies/regex/bin"
  val SUGARJ_STD = "/Users/seba/projects/sugar-lang/stdlib/src"
  val SUGARJ_JAVA = "/Users/seba/projects/sugar-lang/languages/java/bin/"

  val cp = List(SUGAR_BIN, SUGARJ_STD, SUGARJ_JAVA)

  test ("RegexAsString") {
    val lib = Load.load(s"$SUGAR_BIN/regex/RegexAsString.str", cp)
    assert(lib.DEFS.isDefinedAt('regexAsString_0_0))
    assert(lib.DEFS.isDefinedAt('r2str_0_0))
    assert(lib.DEFS.isDefinedAt('ce2str_0_0))
    assert(lib.DEFS.isDefinedAt('bracket_0_0))
    assert(lib.DEFS.isDefinedAt('conc_strings_0_0))
    assert(lib.DEFS.isDefinedAt('string_replace_0_2))
    assert(lib.DEFS.isDefinedAt('elem_0_0))
  }


}
