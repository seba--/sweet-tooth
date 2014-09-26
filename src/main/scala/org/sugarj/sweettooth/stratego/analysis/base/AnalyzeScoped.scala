package org.sugarj.sweettooth.stratego.analysis.base

import org.sugarj.sweettooth.stratego.Syntax.{Exp, Pat}
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

/**
  * Created by seba on 09/09/14.
  */
trait AnalyzeScoped[D <: Domain] extends AnalyzeBase[D] {
  def analyzeScoped(x: Symbol, e: Exp, current: Val, store: Store, stack: Stack): (Val, Store)
}
