package org.sugarj.sweettooth.stratego.analysis.v3_d2

import org.sugarj.sweettooth.stratego.Syntax.Cons
import org.sugarj.sweettooth.stratego.Syntax.Trm.Lit
import org.sugarj.sweettooth.stratego.analysis.base.{BasicStack, StoreTrait}
import org.sugarj.sweettooth.stratego.analysis.domain.d2_PowersetTopTraceDomainFactory
import org.sugarj.sweettooth.stratego.analysis.v3_struct_lists.{ConcatenableVal, v3Analysis}

/**
 * Created by seba on 10/09/14.
 */
trait Config {
  object factory extends d2_PowersetTopTraceDomainFactory {
    trait Vx extends V with ConcatenableVal[Vx]{
      val dom = domain
    }
    class ConcMFin(lits: Set[Lit[_]], apps: Map[Cons, List[Vx]], inf: Boolean) extends MFin(lits, apps, inf) with Vx

    object domain extends D {
      def makeMFin(lits: Set[Lit[_]], apps: Map[Cons, List[Vx]], inf: Boolean) = new ConcMFin(lits, apps, inf)
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
