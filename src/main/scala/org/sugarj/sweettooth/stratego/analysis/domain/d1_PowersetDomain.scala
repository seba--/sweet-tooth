package org.sugarj.sweettooth.stratego.analysis.domain

import org.sugarj.sweettooth.stratego.Syntax.{Trm, Pat, Cons, litLT}

object d1_PowersetDomain {
  import Trm.Lit

  case object Inf extends Val
  case class Fin(lits: Set[Lit[_]], apps: Map[Cons, List[Val]]) extends Val {
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

    def listString(xs: List[Val]): String = xs match {
      case Nil => ""
      case x::Nil => x.toString
      case x::xs => x.toString + ", " + listString(xs)
    }
  }

  trait D extends Domain {
    def bottom: Val = Fin(Set(), Map())
    def top: Val = Inf

    def compare(morePrecise: Val, lessPrecise: Val): Boolean = (morePrecise, lessPrecise) match {
      case (_,Inf) => true
      case (Inf,_) => false
      case (Fin(lits1, apps1), Fin(lits2, apps2)) =>

        def findInS2(t1: (Cons, List[Val])): Boolean = apps2.get(t1._1) match {
          case None => false
          case Some(args2) => t1._2.zip(args2).forall{p => compare(p._1, p._2)}
        }

        lits1.forall(lits2.contains(_)) && apps1.forall(findInS2)
    }

    def join(t1: Val, t2: Val): Val = (t1,t2) match {
      case (Inf,_) => Inf
      case (_,Inf) => Inf
      case (Fin(lits1, apps1), Fin(lits2, apps2)) => Fin(lits1 ++ lits2, mergeUnion(apps1, apps2))
    }

    def meet(t1: Val, t2: Val): Val = (t1,t2) match {
      case (Inf,_) => t2
      case (_,Inf) => t1
      case (Fin(lits1, apps1), Fin(lits2, apps2)) => Fin(lits1.intersect(lits2), mergeIntersect(apps1, apps2))
    }

//    def diff(t1: Val, t2: Val): Val = (t1,t2) match {
//      case (None,_) => None
//      case (_,None) => t1
//      case (Some(s1), Some(s2)) => Some(TS(s1.lits -- s2.lits, mergeDiff(s1.apps, s2.apps)))
//    }

    def matchAppPat(cons: Cons, t: Val): Set[List[Val]] = t match {
      case Inf => Set(for (i <- (1 to cons.ar).toList) yield top)
      case Fin(_,apps) => apps.get(cons) match {
        case None => Set()
        case Some(xs) => Set(xs)
      }
    }

    def liftLit[V](v: V) = Fin(Set(Lit(v)), Map())
    def liftApp(cons: Cons, xs: List[Val]): Val = Fin(Set(), Map(cons -> xs))

//    def explode(t: Val, depth: Int): List[Pat] = t match {
//      case None => List(Pat.Var('?))
//      case Some(ts) =>
//        val litExplode = ts.lits.toList map {case Lit(l) => Pat.Lit(l)}
//        val appExplode = ts.apps flatMap(p => explodeApp(p._1, p._2, depth))
//        litExplode ++ appExplode
//    }
//
//    def explodeApp(cons: Cons, args: List[T], depth: Int): List[Pat] =
//      if (depth <= 1)
//        List(Pat.Var('_ooo))
//      else {
//        val ls = args map (explode(_, depth-1))
//        crossProduct(ls) map (args => Pat.App(cons, args))
//      }
//
//    def crossProduct[T](tss: List[List[T]]): List[List[T]] =
//      if (tss.isEmpty)
//        List(List())
//      else if (tss.tail.isEmpty)
//        tss.head map (List(_))
//      else {
//        val rest = crossProduct(tss.tail)
//        for (prod <- rest;
//             ts <- tss.head)
//        yield ts :: prod
//      }
  }
}