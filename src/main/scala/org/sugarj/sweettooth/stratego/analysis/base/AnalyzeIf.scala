package org.sugarj.sweettooth.stratego.analysis.base

import org.sugarj.sweettooth.stratego.Syntax.{Exp, Pat}
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

/**
  * Created by seba on 09/09/14.
  */
trait AnalyzeIf[V <: Val[V], D <: Domain[V]] extends AnalyzeBase[V, D] {
  def analyzeIf(cnd: Exp, thn: Exp, els: Exp, current: V, store: Store, stack: Stack): (V, Store)
}
