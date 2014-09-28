package org.sugarj.sweettooth.stratego.analysis.v4

import org.sugarj.sweettooth.stratego.Syntax.Cons
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

//trait d3_BoxDomainFactory {

//  type Vx <: Val[Vx]
//  trait Factory {
//    def makeBox(v: Vx): Vx
//  }
//  val factory: Factory

trait BoxableVal[V <: Val[V]] extends Val[V] {
  def isBoxable: Boolean
  def target: V
  def target_=(v: V)
}

trait NoBox[V <: Val[V]] extends BoxableVal[V] {
  def isBoxable = false
  def target = throw new UnsupportedOperationException
  def target_=(v: V) = throw new UnsupportedOperationException
}

abstract class BoxVal[V <: Val[V]] extends BoxableVal[V] {
  var target: V = _
  def isBoxable = true

  def isBottom = target.isBottom
  def isTop = target.isTop

  def ||(v: V) = target || v
  def &&(v: V) = target && v

  def <=(lessPrecise: V) = target <= lessPrecise
  def >=(morePrecise: V) = target >= morePrecise

  def matchCons(cons: Cons) = target.matchCons(cons)
}

trait BoxDomain[V <: Val[V]] extends Domain[V] {
  def makeBox(v: V): V
}


//  def compare(morePrecise: T, lessPrecise: T): Boolean = (morePrecise, lessPrecise) match {
//    case (_,Top) => true
//    case (Top,_) => false
//    case (Resolved(lits1, apps1), Resolved(lits2, apps2)) =>
//      def findInS2(t1: (Cons, List[T])): Boolean = apps2.get(t1._1) match {
//        case None => false
//        case Some(args2) => t1._2.zip(args2).forall{p => compare(p._1, p._2)}
//      }
//
//      lits1.forall(lits2.contains(_)) && apps1.forall(findInS2)
//  }
//
//  def join(t1: T, t2: T): T = (t1,t2) match {
//    case (Top,_) => Top
//    case (_,Top) => Top
//    case (Resolved(lits1, apps1), Resolved(lits2, apps2)) =>
//      Fin(lits1 ++ lits2, mergeUnion(apps1, apps2))
//  }
//
//  def meet(t1: T, t2: T): T = (t1,t2) match {
//    case (Top,_) => Top
//    case (_,Top) => Top
//    case (Resolved(lits1, apps1), Resolved(lits2, apps2)) =>
//      Fin(lits1 intersect lits2, mergeIntersect(apps1, apps2))
//  }
//  def matchAppPat(cons: Cons, t: T): Set[List[T]] = t match {
//    case Top => Set(for (i <- (1 to cons.ar).toList) yield top)
//    case Resolved(_,apps) => apps.get(cons) match {
//      case None => Set()
//      case Some(xs) => Set(xs)
//    }
//  }
//
//  def liftLit[V](v: V) = Fin(Set(Lit(v)), Map())
//  def liftApp(cons: Cons, xs: List[T]): T = Fin(Set(), Map(cons -> xs))
