package org.sugarj.sweettooth.stratego.analysis.v1

import org.sugarj.sweettooth.stratego.Syntax.Exp
import org.sugarj.sweettooth.stratego.analysis.base.AnalyzeSeq
import org.sugarj.sweettooth.stratego.analysis.domain.Domain

/**
  * Created by seba on 09/09/14.
  */
trait v1AnalyzeSeq[V, D <: Domain[V]] extends AnalyzeSeq[V,D] {
  def analyzeSeq(e1: Exp, e2: Exp, current: V, store: Store, stack: Stack): (V, Store) = {
    val (t1, store1) = analyze(e1, current, store, stack)
    analyze(e2, t1, store1, stack)
  }
}
