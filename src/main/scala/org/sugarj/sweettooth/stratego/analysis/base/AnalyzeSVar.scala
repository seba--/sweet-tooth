package org.sugarj.sweettooth.stratego.analysis.base

import org.sugarj.sweettooth.stratego.Syntax.Pat
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

/**
  * Created by seba on 09/09/14.
  */
trait AnalyzeSVar[D <: Domain] extends AnalyzeBase[D] {
  def analyzeSVar(s: Symbol, current: Val, store: Store, stack: Stack): (Val, Store)
}
