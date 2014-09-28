package org.sugarj.sweettooth.stratego.analysis.v3_d1

import org.sugarj.sweettooth.stratego.Syntax.Cons
import org.sugarj.sweettooth.stratego.Syntax.Trm.Lit
import org.sugarj.sweettooth.stratego.analysis.base.{BasicStack, StoreTrait}
import org.sugarj.sweettooth.stratego.analysis.domain.d1_PowersetDomainFactory
import org.sugarj.sweettooth.stratego.analysis.v3_struct_lists.{ConcatenableVal, v3Analysis}

/**
* Created by seba on 10/09/14.
*/
trait Config {
  object factory extends d1_PowersetDomainFactory {
    trait Vx extends V with ConcatenableVal[Vx]
    class ConcInf extends Inf with Vx
    class ConcFin(lits: Set[Lit[_]], apps: Map[Cons, List[Vx]]) extends Fin(lits, apps) with Vx

    object domain extends D
    object factory extends Factory {
      def makeInf = new ConcInf()
      def makeFin(lits: Set[Lit[_]], apps: Map[Cons, List[Vx]]) = new ConcFin(lits, apps)
    }
  }
  val dom = factory.domain
  type V = factory.Vx
  type D = factory.D

  object analysis extends
  v3Analysis[V, D] with
  BasicStack[V, D] with
  StoreTrait[V, D] {
    val dom = Config.this.dom
  }
}
