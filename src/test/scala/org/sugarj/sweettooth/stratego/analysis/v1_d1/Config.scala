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

  object analysis extends
  v1Analysis[D] with
  BasicStack[D] with
  StoreTrait[D] {
    val dom = Config.this.dom
  }
}
