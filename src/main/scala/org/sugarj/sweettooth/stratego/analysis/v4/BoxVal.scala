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

  abstract override def ||(v: V) = v match {
    case BoxVal(target) => this || target
    case _ => super.||(v)
  }
  abstract override def &&(v: V) = v match {
    case BoxVal(target) => this && target
    case _ => super.&&(v)
  }

  abstract override def <=(lessPrecise: V) = lessPrecise match {
    case BoxVal(target) => this <= target
    case _ => super.<=(lessPrecise)
  }
  abstract override def >=(morePrecise: V) = morePrecise match {
    case BoxVal(target) => this <= target
    case _ => super.>=(morePrecise)
  }
}

abstract class BoxVal[V <: Val[V]] extends BoxableVal[V] {
  var target: V = _
  val num = BoxVal.nextBoxNum
  def isBoxable = true

  def isBottom = target.isBottom
  def isTop = target.isTop

  def ||(v: V) = if (this eq v) v else target || v
  def &&(v: V) = if (this eq v) v else target && v

  def <=(lessPrecise: V) = this == lessPrecise || target <= lessPrecise
  def >=(morePrecise: V) = this == morePrecise || target >= morePrecise

  def matchCons(cons: Cons) = target.matchCons(cons)

  var syncToString = false
  override def toString =
    if (syncToString)
      s"Box->#$num"
    else {
      syncToString = true
      val res = s"Box#$num($target)"
      syncToString = false
      res
    }

  var syncHashCode = false
  override def hashCode =
    if (syncHashCode)
      104033
    else {
      syncHashCode = true
      val res = target.hashCode
      syncHashCode = false
      res
    }

  var syncEquals = Set[(Int, Int)]()
  override def equals(a: Any) = a match {
    case bv: BoxVal[_] =>
      if (this.num == bv.num)
        true
      else if (syncEquals.contains((this.num, bv.num)))
        true
      else {
        syncEquals += this.num -> bv.num
        syncEquals += bv.num -> this.num
        this.target == bv.target
      }
    case _ => {
      val h = -Math.abs(a.hashCode + 1)
      if (syncEquals.contains((this.num, h)))
        true
      else {
        syncEquals += this.num -> h
        syncEquals += h -> this.num
        this.target == a
      }
    }
  }

}
object BoxVal {
  def unapply[V <: Val[V]](v: Val[V]): Option[V] =
    if (v.isInstanceOf[BoxVal[_]])
      Some(v.asInstanceOf[BoxVal[V]].target)
    else
      None

  private var count = 0
  def nextBoxNum = synchronized {
    val num = count
    count += 1
    num
  }
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
