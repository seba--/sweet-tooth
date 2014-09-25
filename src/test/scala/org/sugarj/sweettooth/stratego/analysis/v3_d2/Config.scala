package org.sugarj.sweettooth.stratego.analysis.v3_d2

import org.sugarj.sweettooth.stratego.analysis.base.{BasicStack, StoreTrait}
import org.sugarj.sweettooth.stratego.analysis.domain.d2_PowersetFlagDomain
import org.sugarj.sweettooth.stratego.analysis.v3_struct_lists.{ConcatenableDomain, v3Analysis}

/**
 * Created by seba on 10/09/14.
 */
trait Config {
  type V = d2_PowersetFlagDomain.T
  object dom extends d2_PowersetFlagDomain.D with ConcatenableDomain[V]
  type D = dom.type

  object analysis extends
  v3Analysis[V, D] with
  BasicStack[V, D] with
  StoreTrait[V, D] {
    val dom = Config.this.dom
  }
}
