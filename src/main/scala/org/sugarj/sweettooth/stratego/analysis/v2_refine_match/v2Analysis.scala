package org.sugarj.sweettooth.stratego.analysis.v2_refine_match

import org.sugarj.sweettooth.stratego.analysis.base.Analysis
import org.sugarj.sweettooth.stratego.analysis.domain.Domain
import org.sugarj.sweettooth.stratego.analysis.v1._

/**
 * Created by seba on 09/09/14.
 */
trait v2Analysis[V, D <: Domain[V]] extends
  Analysis[V, D] with
  v1AnalyzeSVar[V, D] with
  v1AnalyzeBuild[V, D] with
  v2AnalyzeMatch[V, D] with
  v1AnalyzeSeq[V, D] with
  v1AnalyzeIf[V, D] with
  v1AnalyzeCall[V, D] {

}
