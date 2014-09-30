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
    trait Vx extends Val[Vx] with ConcatenableVal[Vx] {
      val dom = domain
    }
    class ConcInf extends Inf with Vx with NoBox[Vx]
    class ConcFin(lits: Set[Lit[_]], apps: Map[Cons, List[Vx]]) extends Fin(lits, apps) with Vx with NoBox[Vx]
    class ConcBox extends Box[Vx] with Vx
    class ConcMMeet(b: MutableVal[Vx], ref: Vx) extends MMeet[Vx](b, ref) with Vx
    class ConcMJoin(b: MutableVal[Vx], ref: Vx) extends MJoin[Vx](b, ref) with Vx

    object domain extends D with BoxDomain[Vx] {
      def makeInf = new ConcInf()
      def makeFin(lits: Set[Lit[_]], apps: Map[Cons, List[Vx]]) = new ConcFin(lits, apps)
      def makeBox(v: Vx) = {
        val b = new ConcBox
        b.target = v
        b
      }
      def makeMMeet(b: MutableVal[Vx], ref: Vx) = new ConcMMeet(b, ref)
      def makeMJoin(b: MutableVal[Vx], ref: Vx) = new ConcMJoin(b, ref)
    }
  }
  val dom = factory.domain
  type V = factory.Vx
  type D = dom.type

  object analysis extends
  v3Analysis[V, D] with
  GraphStack[V, D] with
  StoreTrait[V, D] {
    val dom = Config.this.dom
  }
}
