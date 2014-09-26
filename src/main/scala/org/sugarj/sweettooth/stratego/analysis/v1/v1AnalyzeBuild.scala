package org.sugarj.sweettooth.stratego.analysis.v1

import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.analysis.base._
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

/**
  * Created by seba on 09/09/14.
  */
trait v1AnalyzeBuild[D <: Domain] extends AnalyzeBuild[D] {
  def analyzeBuild(p: Pat, current: Val, store: Store, stack: Stack): (Val, Store) = {
    val norm = normalize(p, store)
    (norm, store)
  }
}
