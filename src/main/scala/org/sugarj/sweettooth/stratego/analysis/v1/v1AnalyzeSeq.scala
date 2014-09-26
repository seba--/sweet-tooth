package org.sugarj.sweettooth.stratego.analysis.v1

import org.sugarj.sweettooth.stratego.Syntax.Exp
import org.sugarj.sweettooth.stratego.analysis.base.AnalyzeSeq
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

/**
  * Created by seba on 09/09/14.
  */
trait v1AnalyzeSeq[D <: Domain] extends AnalyzeSeq[D] {
  def analyzeSeq(e1: Exp, e2: Exp, current: Val, store: Store, stack: Stack): (Val, Store) = {
    val (t1, store1) = analyze(e1, current, store, stack)
    analyze(e2, t1, store1, stack)
  }
}
