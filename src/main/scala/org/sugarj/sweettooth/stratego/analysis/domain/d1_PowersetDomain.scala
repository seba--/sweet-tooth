package org.sugarj.sweettooth.stratego.analysis.domain

import org.sugarj.sweettooth.stratego.Syntax.{Pat,Cons}

object d1_PowersetDomain {
  type T = Option[TS] // None represents the infinite set, Some represents finite sets

  case class TS(lits: Set[Lit[_]], apps: Map[Cons, List[T]]) {
    def isEmpty = lits.isEmpty && apps.isEmpty
    def size = lits.size + apps.size

    override def toString = {
      var litString = lits.toString
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

    def listString(xs: List[T]): String = xs match {
      case Nil => ""
      case x::Nil => x.toString
      case x::xs => x.toString + ", " + listString(xs)
    }
  }

  abstract class Trm
  case class Lit[V](v: V) extends Trm

  object D extends Domain[T] {
    def bottom: T = Some(TS(Set(), Map()))
    def top: T = None

    def compare(morePrecise: T, lessPrecise: T): Boolean = (morePrecise, lessPrecise) match {
      case (_,None) => true
      case (None,_) => false
      case (Some(s1), Some(s2)) =>

        def findInS2(t1: (Cons, List[T])): Boolean = s2.apps.get(t1._1) match {
          case None => false
          case Some(args2) => t1._2.zip(args2).forall{p => compare(p._1, p._2)}
        }

        s1.lits.forall(s2.lits.contains(_)) &&
        s1.apps.forall(findInS2)
    }

    def join(t1: T, t2: T): T = (t1,t2) match {
      case (None,_) => None
      case (_,None) => None
      case (Some(s1), Some(s2)) => Some(TS(s1.lits ++ s2.lits, mergeUnion(s1.apps, s2.apps)))
    }

    def meet(t1: T, t2: T): T = (t1,t2) match {
      case (None,_) => t2
      case (_,None) => t1
      case (Some(s1), Some(s2)) => Some(TS(s1.lits.intersect(s2.lits), mergeIntersect(s1.apps, s2.apps)))
    }

    def diff(t1: T, t2: T): T = (t1,t2) match {
      case (None,_) => None
      case (_,None) => t1
      case (Some(s1), Some(s2)) => Some(TS(s1.lits -- s2.lits, mergeDiff(s1.apps, s2.apps)))
    }

    def matchAppPat(cons: Cons, t: T): Set[List[T]] = t match {
      case None => Set(for (i <- (1 to cons.ar).toList) yield top)
      case Some(TS(_,apps)) => apps.get(cons) match {
        case None => Set()
        case Some(xs) => Set(xs)
      }
    }

    def liftLit[V](v: V) = Some(TS(Set(Lit(v)), Map()))
    def liftApp(cons: Cons, xs: List[T]) = Some(TS(Set(), Map(cons -> xs)))

    def explode(t: T, depth: Int): List[Pat] = t match {
      case None => List(Pat.Var('?))
      case Some(ts) =>
        val litExplode = ts.lits.toList map {case Lit(l) => Pat.Lit(l)}
        val appExplode = ts.apps flatMap(p => explodeApp(p._1, p._2, depth))
        litExplode ++ appExplode
    }

    def explodeApp(cons: Cons, args: List[T], depth: Int): List[Pat] =
      if (depth <= 1)
        List(Pat.Var('_ooo))
      else {
        val ls = args map (explode(_, depth-1))
        crossProduct(ls) map (args => Pat.App(cons, args))
      }

    def crossProduct[T](tss: List[List[T]]): List[List[T]] =
      if (tss.isEmpty)
        List(List())
      else if (tss.tail.isEmpty)
        tss.head map (List(_))
      else {
        val rest = crossProduct(tss.tail)
        for (prod <- rest;
             ts <- tss.head)
        yield ts :: prod
      }
  }
}