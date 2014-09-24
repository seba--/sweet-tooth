package org.sugarj.sweettooth.stratego.analysis.v2_d1

import org.sugarj.sweettooth.stratego.analysis.base.{BasicStack, StoreTrait}
import org.sugarj.sweettooth.stratego.analysis.domain.d1_PowersetDomain
import org.sugarj.sweettooth.stratego.analysis.v2_refine_match.v2Analysis

/**
 * Created by seba on 10/09/14.
 */
trait Config {
  object dom extends d1_PowersetDomain.D
  type V = d1_PowersetDomain.T
  type D = dom.type

  object analysis extends
  v2Analysis[V, D] with
  BasicStack[V, D] with
  StoreTrait[V, D] {
    val dom = Config.this.dom
  }
}
