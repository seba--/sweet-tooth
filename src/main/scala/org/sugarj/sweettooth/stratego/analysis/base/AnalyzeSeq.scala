package org.sugarj.sweettooth.stratego.analysis.base

import org.sugarj.sweettooth.stratego.Syntax.{Exp, Pat}
import org.sugarj.sweettooth.stratego.analysis.domain.Domain

/**
  * Created by seba on 09/09/14.
  */
trait AnalyzeSeq[V, D <: Domain[V]] extends AnalyzeBase[V,D] {
  def analyzeSeq(e1: Exp, e2: Exp, current: V, store: Store, stack: Stack): (V, Store)
}
