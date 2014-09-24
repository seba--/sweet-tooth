package org.sugarj.sweettooth.stratego.analysis.v1_d1

import org.sugarj.sweettooth.stratego.analysis.base.{BasicStack, StoreTrait}
import org.sugarj.sweettooth.stratego.analysis.domain.d1_PowersetDomain
import org.sugarj.sweettooth.stratego.analysis.v1.v1Analysis

/**
 * Created by seba on 10/09/14.
 */
trait Config {
  object dom extends d1_PowersetDomain.D
  type D = dom.type
  type V = d1_PowersetDomain.T

  object analysis extends
  v1Analysis[V, D] with
  BasicStack[V, D] with
  StoreTrait[V, D] {
    val dom = Config.this.dom
  }
}
