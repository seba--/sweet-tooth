package org.sugarj.sweettooth.stratego.analysis.v1

import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.analysis.base._
import org.sugarj.sweettooth.stratego.analysis.domain.Domain

/**
  * Created by seba on 09/09/14.
  */
trait v1AnalyzeBuild[V, D <: Domain[V]] extends AnalyzeBuild[V,D] with v1AnalyzeBase[V,D] {
  def analyzeBuild(p: Pat, current: V, store: Store, stack: Stack): (V, Store) = {
    val norm = normalize(p, store)
    (norm, store)
  }
}
