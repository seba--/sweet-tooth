package org.sugarj.sweettooth.stratego.analysis.v4_d2

import org.sugarj.sweettooth.stratego.Syntax.Cons
import org.sugarj.sweettooth.stratego.Syntax.Trm.Lit
import org.sugarj.sweettooth.stratego.analysis.base.{BasicStack, StoreTrait}
import org.sugarj.sweettooth.stratego.analysis.domain.{d2_PowersetTopTraceDomainFactory, Val}
import org.sugarj.sweettooth.stratego.analysis.v3_struct_lists.{ConcatenableVal, v3Analysis}
import org.sugarj.sweettooth.stratego.analysis.v4._

/**
* Created by seba on 10/09/14.
*/
trait Config {
  object factory extends d2_PowersetTopTraceDomainFactory {
    trait Vx extends Val[Vx] with BoxableVal[Vx] with ConcatenableVal[Vx] {
      val dom = domain
    }
    class ConcMFin(lits: Set[Lit[_]], apps: Map[Cons, List[Vx]], inf: Boolean) extends MFin(lits, apps, inf) with Vx with NoBox[Vx]
    class ConcBoxVal extends BoxVal[Vx] with Vx

    object domain extends D with BoxDomain[Vx] {
      def makeMFin(lits: Set[Lit[_]], apps: Map[Cons, List[Vx]], inf: Boolean) = new ConcMFin(lits, apps, inf)
      def makeBox(v: Vx) = {
        val b = new ConcBoxVal
        b.target = v
        b
      }
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
