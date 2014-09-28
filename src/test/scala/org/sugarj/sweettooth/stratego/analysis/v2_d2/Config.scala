package org.sugarj.sweettooth.stratego.analysis.v2_d2

import org.sugarj.sweettooth.stratego.Syntax.Cons
import org.sugarj.sweettooth.stratego.Syntax.Trm.Lit
import org.sugarj.sweettooth.stratego.analysis.base.{BasicStack, StoreTrait}
import org.sugarj.sweettooth.stratego.analysis.domain.d2_PowersetTopTraceDomainFactory
import org.sugarj.sweettooth.stratego.analysis.v2_refine_match.v2Analysis

/**
 * Created by seba on 10/09/14.
 */
trait Config {
  object factory extends d2_PowersetTopTraceDomainFactory {
    type Vx = V
    object domain extends D {
      def makeMFin(lits: Set[Lit[_]], apps: Map[Cons, List[V]], inf: Boolean) = new MFin(lits, apps, inf)
    }
  }
  val dom = factory.domain
  type V = factory.V
  type D = factory.D

  object analysis extends
  v2Analysis[V, D] with
  BasicStack[V, D] with
  StoreTrait[V, D] {
    val dom = Config.this.dom
  }
}
