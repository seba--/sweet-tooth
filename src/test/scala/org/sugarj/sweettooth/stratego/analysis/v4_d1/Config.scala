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
    trait MVx extends Vx with ConcatenableVal[Vx] with MutableVal[Vx] {
      abstract override def matchCons(cons: Cons) = dom.makeMMatches(this, cons)
    }
    class ConcInf extends Inf with Vx with NoBox[Vx]
    class ConcFin(lits: Set[Lit[_]], apps: Map[Cons, List[Vx]]) extends Fin(lits, apps) with Vx with NoBox[Vx]
    class ConcBox extends Box[Vx] with MVx
    class ConcMMeet(_b: MutableVal[Vx], _ref: Vx) extends MMeet[Vx] with MVx {
      val b = _b
      val ref = _ref
    }
    class ConcMJoin(_b: MutableVal[Vx], _ref: Vx) extends MJoin[Vx] with MVx {
      val b = _b
      val ref = _ref
    }
    class ConcMMatch(_b: MutableVal[Vx], _cons: Cons, _index: Int) extends MMatch[Vx] with MVx {
      val b = _b
      val cons = _cons
      val index = _index
    }

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
      def makeMMatch(b: MutableVal[Vx], cons: Cons, index: Int) = new ConcMMatch(b, cons, index)
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
