package org.sugarj.sweettooth.stratego.analysis.v1_d2

import org.sugarj.sweettooth.stratego.analysis.base.{BasicStack, StoreTrait}
import org.sugarj.sweettooth.stratego.analysis.domain.d2_PowersetFlagDomain
import org.sugarj.sweettooth.stratego.analysis.v1.v1Analysis

/**
 * Created by seba on 10/09/14.
 */
trait Config {
  type V = d2_PowersetFlagDomain.T
  type D = d2_PowersetFlagDomain.D.type
  val dom = d2_PowersetFlagDomain.D

  object analysis extends
  v1Analysis[V, D] with
  BasicStack[V, D] with
  StoreTrait[V, D] {
    val dom = Config.this.dom
  }

}
