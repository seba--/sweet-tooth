package org.sugarj.sweettooth.stratego.analysis.v1_d2

import org.sugarj.sweettooth.stratego.Syntax.Cons
import org.sugarj.sweettooth.stratego.Syntax.Trm.Lit
import org.sugarj.sweettooth.stratego.analysis.base.{BasicStack, StoreTrait}
import org.sugarj.sweettooth.stratego.analysis.domain.{d2_PowersetTopTraceDomainFactory}
import org.sugarj.sweettooth.stratego.analysis.v1.v1Analysis

/**
 * Created by seba on 10/09/14.
 */
trait Config {
  object factory extends d2_PowersetTopTraceDomainFactory {
    type Vx = V
    object domain extends D
    object factory extends Factory {
      def makeMFin(lits: Set[Lit[_]], apps: Map[Cons, List[V]], inf: Boolean) = new MFin(lits, apps, inf)
    }
  }
  val dom = factory.domain
  type V = factory.V
  type D = factory.D


  object analysis extends
  v1Analysis[V, D] with
  BasicStack[V, D] with
  StoreTrait[V, D] {
    val dom = Config.this.dom
  }

}
