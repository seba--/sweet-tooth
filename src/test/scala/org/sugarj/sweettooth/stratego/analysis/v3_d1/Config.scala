package org.sugarj.sweettooth.stratego.analysis.v3_d1

import org.sugarj.sweettooth.stratego.analysis.base.{BasicStack, StoreTrait}
import org.sugarj.sweettooth.stratego.analysis.domain.d1_PowersetDomain
import org.sugarj.sweettooth.stratego.analysis.v3_struct_lists.{ConcatenableDomain, v3Analysis}

/**
 * Created by seba on 10/09/14.
 */
trait Config {
  type V = d1_PowersetDomain.T
  object dom extends d1_PowersetDomain.D with ConcatenableDomain[V]
  type D = dom.type

  object analysis extends
  v3Analysis[V, D] with
  BasicStack[V, D] with
  StoreTrait[V, D] {
    val dom = Config.this.dom
  }
}
