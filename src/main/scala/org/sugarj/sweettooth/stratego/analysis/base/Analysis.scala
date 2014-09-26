package org.sugarj.sweettooth.stratego.analysis.base

import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}
import org.sugarj.sweettooth.stratego.Semantics.Fail

/**
 * Created by seba on 09/09/14.
 */
trait Analysis[D <: Domain] extends
  AnalyzeSVar[D] with
  AnalyzeBuild[D] with
  AnalyzeMatch[D] with
  AnalyzeSeq[D] with
  AnalyzeIf[D] with
  AnalyzeCall[D] with
  AnalyzeScoped[D] {

  def analyze(e: Exp, current: Val, defs: Defs): Val = {
    this.defs = defs
    try { analyze(e, current, emptyStore, emptyStack)._1 }
    catch { case _: Fail => dom.bottom }
  }

  def analyze(e: Exp, current: Val, store: Store, stack: Stack): (Val, Store) = e match {
    case SVar(s) => analyzeSVar(s, current, store, stack)
    case Match(p) => analyzeMatch(p, current, store, stack)
    case Build(p) => analyzeBuild(p, current, store, stack)
    case Seq(e1, e2) => analyzeSeq(e1, e2, current, store, stack)
    case If(cnd, thn, els) => analyzeIf(cnd, thn, els, current, store, stack)
    case Call(f, sargs, targs) => analyzeCall(f, sargs, targs, current, store, stack)
    case Scoped(x, e) => analyzeScoped(x, e, current, store, stack)
  }
}
