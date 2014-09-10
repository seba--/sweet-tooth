package org.sugarj.sweettooth.stratego.analysis.base

import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.analysis.domain.Domain
import org.sugarj.sweettooth.stratego.Semantics.Fail

/**
 * Created by seba on 09/09/14.
 */
trait Analysis[V, D <: Domain[V]] extends
  AnalyzeSVar[V, D] with
  AnalyzeBuild[V, D] with
  AnalyzeMatch[V, D] with
  AnalyzeSeq[V, D] with
  AnalyzeIf[V, D] with
  AnalyzeCall[V, D] with
  AnalyzeScoped[V, D] {

  def analyze(e: Exp, current: V, defs: Defs): V = {
    this.defs = defs
    analyze(e, current, emptyStore, emptyStack)._1
  }

  def analyze(e: Exp, current: V, store: Store, stack: Stack): (V, Store) = e match {
    case SVar(s) => analyzeSVar(s, current, store, stack)
    case Match(p) => analyzeMatch(p, current, store, stack)
    case Build(p) => analyzeBuild(p, current, store, stack)
    case Seq(e1, e2) => analyzeSeq(e1, e2, current, store, stack)
    case If(cnd, thn, els) => analyzeIf(cnd, thn, els, current, store, stack)
    case Call(f, sargs, targs) => analyzeCall(f, sargs, targs, current, store, stack)
    case Scoped(x, e) => analyzeScoped(x, e, current, store, stack)
  }
}
