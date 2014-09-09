package org.sugarj.sweettooth.stratego.analysis.base

import org.sugarj.sweettooth.stratego.Syntax.{Exp, Pat}
import org.sugarj.sweettooth.stratego.analysis.domain.Domain

/**
  * Created by seba on 09/09/14.
  */
trait AnalyzeCall[V, D <: Domain[V]] extends AnalyzeBase[V,D] {
  def analyzeCall(f: Symbol, sargs: List[Exp], targs: List[Pat], current: V, store: Store, stack: Stack): (V, Store)
}
