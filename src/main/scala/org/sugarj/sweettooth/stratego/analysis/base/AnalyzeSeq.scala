package org.sugarj.sweettooth.stratego.analysis.base

import org.sugarj.sweettooth.stratego.Syntax.{Exp, Pat}
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

/**
  * Created by seba on 09/09/14.
  */
trait AnalyzeSeq[D <: Domain] extends AnalyzeBase[D] {
  def analyzeSeq(e1: Exp, e2: Exp, current: Val, store: Store, stack: Stack): (Val, Store)
}
