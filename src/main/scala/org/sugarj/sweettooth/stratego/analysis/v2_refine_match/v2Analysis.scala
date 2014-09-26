package org.sugarj.sweettooth.stratego.analysis.v2_refine_match

import org.sugarj.sweettooth.stratego.analysis.base.Analysis
import org.sugarj.sweettooth.stratego.analysis.domain.Domain
import org.sugarj.sweettooth.stratego.analysis.v1._

/**
 * Created by seba on 09/09/14.
 */
trait v2Analysis[D <: Domain] extends
  Analysis[D] with
  v1AnalyzeSVar[D] with
  v1AnalyzeBuild[D] with
  v2AnalyzeMatch[D] with
  v1AnalyzeSeq[D] with
  v1AnalyzeIf[D] with
  v1AnalyzeCall[D] with
  v1AnalyzeScoped[D] {

}
