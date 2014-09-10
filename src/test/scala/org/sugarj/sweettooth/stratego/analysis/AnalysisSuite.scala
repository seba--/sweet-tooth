package org.sugarj.sweettooth.stratego.analysis

import org.scalatest.FunSuite
import org.sugarj.sweettooth.stratego.Semantics.Fail

/**
 * Created by seba on 10/09/14.
 */
class AnalysisSuite extends FunSuite {
  def assertDomT[V1,V2](expected: V1)(actual: =>V2) {
    try {
      val res = actual
      assertResult(expected)(res)
    } catch {
      case Fail(s, msg) => assert(false, s"Execution failed:\n  Message: $msg\n  Strategy: $s\n  Expected: $expected")
    }
  }
}
