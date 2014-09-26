package org.sugarj.sweettooth.stratego.analysis.v1_d2

import org.sugarj.sweettooth.stratego.analysis.base.{BasicStack, StoreTrait}
import org.sugarj.sweettooth.stratego.analysis.domain.d2_PowersetFlagDomain
import org.sugarj.sweettooth.stratego.analysis.v1.v1Analysis

/**
 * Created by seba on 10/09/14.
 */
trait Config {
  object dom extends d2_PowersetFlagDomain.D
  type D = dom.type

  object analysis extends
  v1Analysis[D] with
  BasicStack[D] with
  StoreTrait[D] {
    val dom = Config.this.dom
  }

}
