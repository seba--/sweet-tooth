package org.sugarj.sweettooth.stratego.analysis.base

import org.sugarj.sweettooth.stratego.Syntax.Pat
import org.sugarj.sweettooth.stratego.analysis.domain.Domain

/**
  * Created by seba on 09/09/14.
  */
trait AnalyzeSVar[V, D <: Domain[V]] extends AnalyzeBase[V,D] {
  def analyzeSVar(s: Symbol, current: V, store: Store, stack: Stack): (V, Store)
}
