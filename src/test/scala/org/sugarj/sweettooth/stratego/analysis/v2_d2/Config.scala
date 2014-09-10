package org.sugarj.sweettooth.stratego.analysis.v2_d2

import org.sugarj.sweettooth.stratego.analysis.base.{StoreTrait, BasicStack}
import org.sugarj.sweettooth.stratego.analysis.domain.d2_PowersetFlagDomain
import org.sugarj.sweettooth.stratego.analysis.v2_refine_match.v2Analysis

/**
 * Created by seba on 10/09/14.
 */
trait Config {
  type V = d2_PowersetFlagDomain.T
  type D = d2_PowersetFlagDomain.D.type
  val dom = d2_PowersetFlagDomain.D

  object analysis extends
  v2Analysis[V, D] with
  BasicStack[V, D] with
  StoreTrait[V, D] {
    val dom = Config.this.dom
  }
}
