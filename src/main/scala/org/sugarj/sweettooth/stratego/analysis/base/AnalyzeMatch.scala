package org.sugarj.sweettooth.stratego.analysis.base

import org.sugarj.sweettooth.stratego.Syntax.Pat
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

/**
  * Created by seba on 09/09/14.
  */
trait AnalyzeMatch[D <: Domain] extends AnalyzeBase[D] {
  def analyzeMatch(matchPat: Pat, current: Val, store: Store, stack: Stack): (Val, Store)
}
