package org.sugarj.sweettooth.stratego.analysis

import org.scalatest.FunSuite
import org.sugarj.sweettooth.stratego.Semantics.Fail
import org.sugarj.sweettooth.stratego.Syntax.{Exp, Call, Trm}
import org.sugarj.sweettooth.stratego.analysis.base.Analysis
import org.sugarj.sweettooth.stratego.analysis.domain.Domain
import org.sugarj.sweettooth.stratego.lib.Library

import org.sugarj.sweettooth.stratego.lib

/**
 * Created by seba on 10/09/14.
 */
object AnalysisSuite {
  val RESOURCES = "src/test/resources"
}

abstract class AnalysisSuite extends FunSuite {
  val prefix = this.getClass.getPackage.getName.substring(getClass.getPackage.getName.lastIndexOf('.') + 1)

  type V
  type D <: Domain[V]
  val dom: D

  val analysis: Analysis[V, D]
  val baseLib: Library

  def lift(t: Trm) = dom.lift(t)

  def test_analysis(name: String)(e: Exp, input: =>V)(expected: =>V) =
    test(s"$prefix: $name") {
      try {
        val res = analysis.analyze(e, input, baseLib.DEFS)
        assertResult(expected)(res)
      } catch {
        case Fail(s, msg) => assert(false, s"Execution failed:\n  Message: $msg\n  Strategy: $s\n  Expected: $expected")
      }
    }

  def test_strat(strat: String, name: String)(input: =>V)(expected: =>V) =
    test(s"$prefix: <$strat> ($name)") {
      try {
        val res = analysis.analyze(Call(Symbol(s"${strat.replace('-','_')}_0_0")), input, baseLib.DEFS)
        assertResult(expected)(res)
      } catch {
        case Fail(s, msg) => assert(false, s"Execution failed:\n  Message: $msg\n  Strategy: $s\n  Expected: $expected")
      }
    }
}
