package org.sugarj.sweettooth.stratego.analysis.v3_d1

import org.sugarj.sweettooth.stratego.analysis.base.{BasicStack, StoreTrait}
import org.sugarj.sweettooth.stratego.analysis.domain.d1_PowersetDomain
import org.sugarj.sweettooth.stratego.analysis.v3_struct_lists.{ConcatenableDomain, v3Analysis}

/**
 * Created by seba on 10/09/14.
 */
trait Config {
  object dom extends d1_PowersetDomain.D with ConcatenableDomain
  type D = dom.type

  object analysis extends
  v3Analysis[D] with
  BasicStack[D] with
  StoreTrait[D] {
    val dom = Config.this.dom
  }
}
