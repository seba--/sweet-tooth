package org.sugarj.sweettooth.stratego.analysis.v4_d1

import org.sugarj.sweettooth.stratego.Syntax.Cons
import org.sugarj.sweettooth.stratego.Syntax.Trm.Lit
import org.sugarj.sweettooth.stratego.analysis.base.{BasicStack, StoreTrait}
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, d1_PowersetDomainFactory}
import org.sugarj.sweettooth.stratego.analysis.v2_refine_match.v2Analysis
import org.sugarj.sweettooth.stratego.analysis.v3_struct_lists.{ConcatenableVal, v3Analysis}
import org.sugarj.sweettooth.stratego.analysis.v4._

/**
* Created by seba on 10/09/14.
*/
trait Config {
  object factory extends d1_PowersetDomainFactory {
    trait Vx extends Val[Vx] with BoxableVal[Vx] // trait Vx extends V with BoxVal[Vx]
    class ConcInf extends Inf with Vx with NoBox[Vx]
    class ConcFin(lits: Set[Lit[_]], apps: Map[Cons, List[Vx]]) extends Fin(lits, apps) with Vx with NoBox[Vx]
    class ConcBoxVal extends BoxVal[Vx] with Vx

    trait D extends super.D with BoxDomain[Vx] {
      def makeBox(v: Vx) = {
        val b = new ConcBoxVal
        b.target = v
        b
      }
    }

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
  v2Analysis[V, D] with
  GraphStack[V, D] with
  StoreTrait[V, D] {
    val dom = Config.this.dom
  }
}
