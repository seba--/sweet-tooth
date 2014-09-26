package org.sugarj.sweettooth.stratego.analysis.v3_d2

import org.sugarj.sweettooth.stratego.analysis.base.{BasicStack, StoreTrait}
import org.sugarj.sweettooth.stratego.analysis.domain.d2_PowersetFlagDomain
import org.sugarj.sweettooth.stratego.analysis.v3_struct_lists.{ConcatenableDomain, v3Analysis}

/**
 * Created by seba on 10/09/14.
 */
trait Config {
  object dom extends d2_PowersetFlagDomain.D with ConcatenableDomain
  type D = dom.type

  object analysis extends
  v3Analysis[D] with
  BasicStack[D] with
  StoreTrait[D] {
    val dom = Config.this.dom
  }
}
