package org.sugarj.sweettooth.stratego.analysis.base

import org.sugarj.sweettooth.stratego.Syntax.{Exp, Pat}
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

/**
  * Created by seba on 09/09/14.
  */
trait AnalyzeCall[D <: Domain] extends AnalyzeBase[D] {
  def analyzeCall(f: Symbol, sargs: List[Exp], targs: List[Pat], current: Val, store: Store, stack: Stack): (Val, Store)
}
