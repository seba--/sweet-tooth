//package org.sugarj.sweettooth.stratego.analysis.v4
//
//import org.sugarj.sweettooth.stratego.Syntax._
//import org.sugarj.sweettooth.stratego.Syntax.Trm.Lit
//import org.sugarj.sweettooth.stratego.analysis.domain.Domain
//
//object GraphableDomain {
//
//  abstract class T
//  case object Top extends T
//  case class Box(var target: T) extends T
//  case class Fin(lits: Set[Lit[_]], apps: Map[Cons, List[T]]) extends T {
//    def isEmpty = lits.isEmpty && apps.isEmpty
//    def size = lits.size + apps.size
//
//    override def toString = {
//      val slits = lits.toList.sortWith(litLT)
//      var litString = slits.toString
//      if (lits.isEmpty)
//        litString = "Set("
//      else
//        litString = litString.substring(0, litString.length - 1)
//
//      val appString = StringBuilder.newBuilder
//      apps.foreach(p => appString append (p._1.name + "(" + listString(p._2) + "), "))
//      if (!apps.isEmpty)
//        appString.delete(appString.length - 3, appString.length - 1)
//      litString + appString + ")"
//    }
//
//    def listString(xs: List[T]): String = xs match {
//      case Nil => ""
//      case x::Nil => x.toString
//      case x::xs => x.toString + ", " + listString(xs)
//    }
//  }
//  object Resolved {
//    def unapply(t: T): Option[(Set[Lit[_]], Map[Cons, List[T]])] = t match {
//      case Top => None
//      case Box(t2) => unapply(t2)
//      case Fin(lits, apps) => Some((lits, apps))
//    }
//  }
//
//  trait D extends Domain {
//    def bottom: T = Fin(Set(), Map())
//    def top: T = Top
//
//    def compare(morePrecise: T, lessPrecise: T): Boolean = (morePrecise, lessPrecise) match {
//      case (_,Top) => true
//      case (Top,_) => false
//      case (Resolved(lits1, apps1), Resolved(lits2, apps2)) =>
//        def findInS2(t1: (Cons, List[T])): Boolean = apps2.get(t1._1) match {
//          case None => false
//          case Some(args2) => t1._2.zip(args2).forall{p => compare(p._1, p._2)}
//        }
//
//        lits1.forall(lits2.contains(_)) && apps1.forall(findInS2)
//    }
//
//    def join(t1: T, t2: T): T = (t1,t2) match {
//      case (Top,_) => Top
//      case (_,Top) => Top
//      case (Resolved(lits1, apps1), Resolved(lits2, apps2)) =>
//        Fin(lits1 ++ lits2, mergeUnion(apps1, apps2))
//    }
//
//    def meet(t1: T, t2: T): T = (t1,t2) match {
//      case (Top,_) => Top
//      case (_,Top) => Top
//      case (Resolved(lits1, apps1), Resolved(lits2, apps2)) =>
//        Fin(lits1 intersect lits2, mergeIntersect(apps1, apps2))
//    }
//    def matchAppPat(cons: Cons, t: T): Set[List[T]] = t match {
//      case Top => Set(for (i <- (1 to cons.ar).toList) yield top)
//      case Resolved(_,apps) => apps.get(cons) match {
//        case None => Set()
//        case Some(xs) => Set(xs)
//      }
//    }
//
//    def liftLit[V](v: V) = Fin(Set(Lit(v)), Map())
//    def liftApp(cons: Cons, xs: List[T]): T = Fin(Set(), Map(cons -> xs))
//
// }
//}