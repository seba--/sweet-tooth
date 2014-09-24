package org.sugarj.sweettooth.stratego.analysis.v3_struct_lists

import org.sugarj.sweettooth.stratego.analysis.base.Analysis
import org.sugarj.sweettooth.stratego.analysis.domain.{d3_ConcDomain, Domain}
import org.sugarj.sweettooth.stratego.analysis.v1._
import org.sugarj.sweettooth.stratego.analysis.v2_refine_match.v2AnalyzeMatch

/**
 * Created by seba on 09/09/14.
 */
trait v3Analysis[V, D <: d3_ConcDomain[V]] extends
  Analysis[V, D] with
  v1AnalyzeSVar[V, D] with
  v1AnalyzeBuild[V, D] with
  v2AnalyzeMatch[V, D] with
  v1AnalyzeSeq[V, D] with
  v1AnalyzeIf[V, D] with
  v3AnalyzeCall[V, D] with
  v1AnalyzeScoped[V, D] {

}
