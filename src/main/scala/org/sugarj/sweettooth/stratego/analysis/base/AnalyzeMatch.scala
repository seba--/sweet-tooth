package org.sugarj.sweettooth.stratego.analysis.base

import org.sugarj.sweettooth.stratego.Syntax.Pat
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

/**
  * Created by seba on 09/09/14.
  */
trait AnalyzeMatch[V <: Val[V], D <: Domain[V]] extends AnalyzeBase[V, D] {
  def analyzeMatch(matchPat: Pat, current: V, store: Store, stack: Stack): (V, Store)
}
