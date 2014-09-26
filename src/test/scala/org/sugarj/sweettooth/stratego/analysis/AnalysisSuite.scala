package org.sugarj.sweettooth.stratego.analysis

import org.scalatest.FunSuite
import org.sugarj.sweettooth.stratego.Semantics.Fail
import org.sugarj.sweettooth.stratego.Syntax.{Call, Exp, Trm}
import org.sugarj.sweettooth.stratego.analysis.base.Analysis
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}
import org.sugarj.sweettooth.stratego.lib.Library
import scala.language.implicitConversions

/**
 * Created by seba on 10/09/14.
 */
object AnalysisSuite {
  val RESOURCES = "src/test/resources"
}

abstract class AnalysisSuite extends FunSuite {
  val prefix = this.getClass.getPackage.getName.substring(getClass.getPackage.getName.lastIndexOf('.') + 1)

  type D <: Domain
  val dom: D

  val analysis: Analysis[D]
  val baseLib: Library

  def lift(t: Trm) = dom.lift(t)

  abstract class Spec
  case class Equals(v: Val) extends Spec
  case class BoundBy(lower: Val, upper: Val) extends Spec
  implicit def equalsSpec(v: Val): Spec = Equals(v)

  def test_analysis(name: String)(e: Exp, input: =>Val)(expected: =>Spec): Unit =
    test(s"$prefix: $name") {
      try {
        val res = analysis.analyze(e, input, baseLib.DEFS)
        val expectedV = expected match {case Equals(v)=>v;case BoundBy(v,_)=>v}
        lazy val hint = "\n\n" +
          s"exp = $expectedV\n\n" +
          s"res = $res\n\n"
        s"    res <= expected: ${dom.compare(res, expectedV)}\n" +
          s"    expected <= res: ${dom.compare(expectedV, res)}\n"
        //                        +
        //                        s"    res && expected: ${dom.meet(res, expected)}\n" +
        //                        s"    res -- expected: ${dom.diff(res, expected)}\n" +
        //                        s"    expected -- res: ${dom.diff(expected, res)}"
        expected match {
          case Equals(v) => assertResult(v, hint)(res)
          case BoundBy(low, up) =>
            assert(dom.compare(low, res), s"Result does not satisfy lower bound$hint")
            assert(dom.compare(res, up), s"Result does not satisfy upper bound$hint")
        }
      } catch {
        case Fail(s, msg) => assert(false, s"Execution failed:\n  Message: $msg\n  Strategy: $s\n  Expected: $expected")
      }
    }

  def test_strat(strat: String, name: String)(input: =>Val)(expected: =>Spec) = {
    val normStrat = s"${strat.replace('-','_')}_0_0"
    test_analysis(s"<$strat> ($name)")(Call(Symbol(normStrat)), input)(expected)
  }
//
//  test(s"$prefix: <$strat> ($name)") {
//      try {
//        val normStrat = s"${strat.replace('-','_')}_0_0"
//        val res = analysis.analyze(Call(Symbol(normStrat)), input, baseLib.DEFS)
//        val expectedV = expected match {case Equals(v)=>v;case BoundBy(v,_)=>v}
//        lazy val hint = "\n\n" +
//                        s"exp = $expectedV\n\n" +
//                        s"res = $res\n\n"
//                        s"    res <= expected: ${dom.compare(res, expectedV)}\n" +
//                        s"    expected <= res: ${dom.compare(expectedV, res)}\n"
////                        +
////                        s"    res && expected: ${dom.meet(res, expected)}\n" +
////                        s"    res -- expected: ${dom.diff(res, expected)}\n" +
////                        s"    expected -- res: ${dom.diff(expected, res)}"
//        expected match {
//          case Equals(v) => assertResult(v, hint)(res)
//          case BoundBy(low, up) =>
//            assert(dom.compare(low, res), s"Result does not satisfy lower bound$hint")
//            assert(dom.compare(res, up), s"Result does not satisfy upper bound$hint")
//        }
//      } catch {
//        case Fail(s, msg) => assert(false, s"Execution failed:\n  Message: $msg\n  Strategy: $s\n  Expected: $expected")
//      }
//    }
}
