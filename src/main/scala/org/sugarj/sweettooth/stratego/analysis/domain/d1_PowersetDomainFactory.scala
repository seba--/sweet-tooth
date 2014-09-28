package org.sugarj.sweettooth.stratego.analysis.domain

import org.sugarj.sweettooth.stratego.Syntax.{Trm, Pat, Cons, litLT}

trait d1_PowersetDomainFactory {
  import Trm.Lit

  type Vx <: Val[Vx]
  val domain: Domain[Vx]
  trait Factory {
    def makeInf: Vx
    def makeFin(lits: Set[Lit[_]], apps: Map[Cons, List[Vx]]): Vx
  }
  val factory: Factory


  abstract class V extends Val[Vx] {
    val dom = domain
//    def matchCons(cons: Cons): Set[List[V]] = Set()
  }
  class Inf extends V {
    def ||(v: Vx) = factory.makeInf
    def &&(v: Vx) = v
    def <=(lessPrecise: Vx) = lessPrecise == this
    def >=(morePrecise: Vx) = true
    def matchCons(cons: Cons) = Set(for (i <- (1 to cons.ar).toList) yield dom.top)
    override def toString = "?"
    override def hashCode = 0
    override def equals(a: Any) = a.isInstanceOf[Inf]
  }
  object Inf {
    def apply() = new Inf()
    def unapply(v: Vx): Boolean = v.isInstanceOf[Inf]
  }
  class Fin(val lits: Set[Lit[_]], val apps: Map[Cons, List[Vx]]) extends V {
    def ||(v: Vx) = v match {
      case Inf() => v
      case Fin(lits2, apps2) => factory.makeFin(lits ++ lits2, mergeUnion(apps, apps2))
    }

    def &&(v: Vx) = v match {
      case Inf() => factory.makeInf
      case Fin(lits2, apps2) => factory.makeFin(lits intersect lits2, mergeIntersect(apps, apps2))
    }

    def <=(lessPrecise: Vx) = lessPrecise match {
      case Inf() => true
      case Fin(lits2, apps2) =>
        def findInS2(t1: (Cons, List[Vx])): Boolean = apps2.get(t1._1) match {
          case None => false
          case Some(args2) => t1._2.zip(args2).forall{p => (p._1 <= p._2)}
        }
        lits.forall(lits2.contains(_)) && apps.forall(findInS2)
    }

    def >=(morePrecise: Vx) = morePrecise <= factory.makeFin(lits, apps)
    def matchCons(cons: Cons) = apps.get(cons) match {
      case None => Set()
      case Some(xs) => Set(xs)
    }

    def isEmpty = lits.isEmpty && apps.isEmpty
    def size = lits.size + apps.size

    override def toString = {
      val slits = lits.toList.sortWith(litLT)
      var litString = slits.toString
      if (lits.isEmpty)
        litString = "Set("
      else
        litString = litString.substring(0, litString.length - 1)

      val appString = StringBuilder.newBuilder
      apps.foreach(p => appString append (p._1.name + "(" + listString(p._2) + "), "))
      if (!apps.isEmpty)
        appString.delete(appString.length - 2, appString.length)
      litString + appString + ")"
    }

    def listString(xs: List[Vx]): String = xs match {
      case Nil => ""
      case x::Nil => x.toString
      case x::xs => x.toString + ", " + listString(xs)
    }

    override def hashCode = lits.hashCode * 31 + apps.hashCode

    override def equals(a: Any) =
      if (a.isInstanceOf[Fin]) {
        val f = a.asInstanceOf[Fin]
        f.lits == lits && f.apps == apps
      }
      else
        false

  }
  object Fin {
    def apply(lits: Set[Lit[_]], apps: Map[Cons, List[Vx]]) = factory.makeFin(lits, apps)
    def unapply(v: Vx): Option[(Set[Lit[_]], Map[Cons, List[Vx]])] =
      if (v.isInstanceOf[Fin]) {
        val w = v.asInstanceOf[Fin]
        Some((w.lits, w.apps))
      }
      else
        None
  }

  trait D extends Domain[Vx] {
    def bottom: Vx = factory.makeFin(Set(), Map())
    def top: Vx = factory.makeInf

    def liftLit[T](v: T): Vx = factory.makeFin(Set(Lit(v)), Map())
    def liftApp(cons: Cons, xs: List[Vx]): Vx = factory.makeFin(Set(), Map(cons -> xs))
  }
}