package org.sugarj.sweettooth.stratego.analysis.domain

import org.sugarj.sweettooth.stratego.Syntax.{Trm, Pat, Cons, litLT}

trait d1_PowersetDomain {
  import Trm.Lit

  val domain: Domain[V]

  abstract class V extends Val[V] {
    val dom = domain
  }
  case object Inf extends V {
    def ||(v: V) = Inf
    def &&(v: V) = v
    def <=(lessPrecise: V) = lessPrecise == Inf
    def >=(morePrecise: V) = true
    def matchCons(cons: Cons) = Set(for (i <- (1 to cons.ar).toList) yield dom.top)
  }
  case class Fin(lits: Set[Lit[_]], apps: Map[Cons, List[V]]) extends V {
    def ||(v: V) = v match {
      case Inf => Inf
      case Fin(lits2, apps2) => Fin(lits ++ lits2, mergeUnion(apps, apps2))
    }

    def &&(v: V) = v match {
      case Inf => this
      case Fin(lits2, apps2) => Fin(lits intersect lits2, mergeIntersect(apps, apps2))
    }

    def <=(lessPrecise: V) = lessPrecise match {
      case Inf => true
      case Fin(lits2, apps2) =>
        def findInS2(t1: (Cons, List[V])): Boolean = apps2.get(t1._1) match {
          case None => false
          case Some(args2) => t1._2.zip(args2).forall{p => (p._1 <= p._2)}
        }
        lits.forall(lits2.contains(_)) && apps.forall(findInS2)
    }

    def >=(morePrecise: V) = morePrecise <= this
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
        appString.delete(appString.length - 3, appString.length - 1)
      litString + appString + ")"
    }

    def listString(xs: List[V]): String = xs match {
      case Nil => ""
      case x::Nil => x.toString
      case x::xs => x.toString + ", " + listString(xs)
    }
  }

  trait D extends Domain[V] {
    def bottom: V = Fin(Set(), Map())
    def top: V = Inf

    def liftLit[V](v: V) = Fin(Set(Lit(v)), Map())
    def liftApp(cons: Cons, xs: List[V]): V = Fin(Set(), Map(cons -> xs))
  }
}