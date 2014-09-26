package org.sugarj.sweettooth.stratego.analysis.v1

import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.analysis.base.Analysis
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

/**
 * Created by seba on 09/09/14.
 */
trait v1Analysis[V <: Val[V], D <: Domain[V]] extends
  Analysis[V,D] with
  v1AnalyzeSVar[V,D] with
  v1AnalyzeBuild[V,D] with
  v1AnalyzeMatch[V,D] with
  v1AnalyzeSeq[V,D] with
  v1AnalyzeIf[V,D] with
  v1AnalyzeCall[V,D] with
  v1AnalyzeScoped[V,D] {

}
