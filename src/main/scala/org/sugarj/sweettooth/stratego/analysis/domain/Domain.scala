package org.sugarj.sweettooth.stratego.analysis.domain

import org.sugarj.sweettooth.stratego.Syntax
import org.sugarj.sweettooth.stratego.Syntax.Cons

trait Val[V <: Val[V]] {
  val dom: Domain[V]

  /* join this value with another value */
  def ||(t2: V): V
  /* meet this value with another value */
  def &&(t2: V): V

  def ||(t2: V, t3: V, ts: V*): V =
    this || (t2::t3::List(ts:_*) reduce(_||_))
  def &&(t2: V, t3: V, ts: V*): V =
    this && (t2::t3::List(ts:_*) reduce(_&&_))

  def <=(lessPrecise: V): Boolean
  def >=(morePrecise: V): Boolean
  def <(lessPrecise: V) = !(this >= lessPrecise)
  def >(morePrecise: V) = !(this <= morePrecise)

  def matchCons(cons: Cons): Set[List[V]]

  def mergeUnion(s1: Map[Cons, List[V]], s2: Map[Cons, List[V]]) = {
    var apps = s1
    for ((c, ys) <- s2)
      s1.get(c) match {
        case None => apps += c -> ys
        case Some(xs) => apps += c -> (xs.zip(ys).map(p => p._1 || p._2))
      }
    apps
  }

  def mergeIntersect(m1: Map[Cons, List[V]], m2: Map[Cons, List[V]]) = {
    var apps = Map[Cons, List[V]]()
    for ((c, ys) <- m2)
      m1.get(c) match {
        case None => // no meet
        case Some(xs) =>
          val met = (xs.zip(ys).map(p => p._1 && p._2))
          if (met.isEmpty || met.exists(x => x != dom.bottom))
            apps += c -> met
      }
    apps
  }
}

trait Domain[V <: Val[V]] {
  def top: V
  def bottom: V

  def liftLit[T](v: T): V
  def mliftLit[T](v: T): V = top || liftLit(v)
  def liftApp(cons: Cons, xs: List[V]): V
  def liftApp(cons: Symbol, xs: List[V]): V = liftApp(Cons(cons, xs.size), xs)
  def liftApp(cons: Cons, xs: V*): V = liftApp(cons, List(xs:_*))
  def liftApp(cons: Symbol, xs: V*): V = liftApp(Cons(cons, xs.size), List(xs:_*))
  def mliftApp(cons: Symbol, xs: V*): V = top || liftApp(Cons(cons, xs.size), List(xs:_*))

  def lift(t: Syntax.Trm): V = t match {
    case Syntax.Trm.Lit(v) => liftLit(v)
    case Syntax.Trm.App(cons, xs) => liftApp(cons, xs map (lift(_)))
  }

//  case class Explodable(t: T, explode: T => List[Syntax.Pat]) {
//    lazy V ex = explode(t)
//    override def toString = "\n Explode: " + ex.toString
//  }
//  object Explodable {
//    def apply(t: T): Explodable = Explodable(t, explode(_, 3))
//  }
//  def explode(t: T, depth: Int): List[Syntax.Pat]

//  def mergeDiff(m1: Map[Cons, List[T]], m2: Map[Cons, List[T]]): Map[Cons, List[T]] = {
//    var apps = Map[Cons, List[T]]()
//    for ((c, xs) <- m1)
//      m2.get(c) match {
//        case None => apps += c -> xs
//        case Some(ys) =>
//          val d = (xs.zip(ys).map(p => diff(p._1,p._2)))
//          val nonBottomTop = d.count(x => x!=bottom && x!=top)
//          if (nonBottomTop == 1) {
//            val diff = d.zip(xs).map(p => if (p._1 != bottom) p._1 else p._2)
//            apps += c -> diff
//          }
//          else if (nonBottomTop > 1)
//            apps += c -> xs
//          else
//            apps.size
//      }
//    apps
//  }
}
