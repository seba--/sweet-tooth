package org.sugarj.sweettooth.stratego.analysis.v2_d1

import org.sugarj.sweettooth.stratego.Syntax.Cons
import org.sugarj.sweettooth.stratego.Syntax.Trm.Lit
import org.sugarj.sweettooth.stratego.analysis.base.{BasicStack, StoreTrait}
import org.sugarj.sweettooth.stratego.analysis.domain.d1_PowersetDomainFactory
import org.sugarj.sweettooth.stratego.analysis.v2_refine_match.v2Analysis

/**
 * Created by seba on 10/09/14.
 */
trait Config {
  object factory extends d1_PowersetDomainFactory {
    type Vx = V
    object domain extends D
    object factory extends Factory {
      def makeInf = new Inf()
      def makeFin(lits: Set[Lit[_]], apps: Map[Cons, List[V]]) = new Fin(lits, apps)
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
