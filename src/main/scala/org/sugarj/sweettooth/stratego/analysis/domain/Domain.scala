package org.sugarj.sweettooth.stratego.analysis.domain

import org.sugarj.sweettooth.stratego.Syntax
import org.sugarj.sweettooth.stratego.Syntax.Cons

trait Domain[T] {
  def top: T
  def bottom: T
  def compare(morePrecise: T, lessPrecise: T): Boolean
  def join(t1: T, t2: T): T
  def meet(t1: T, t2: T): T
  def diff(t1: T, t2: T): T

  def matchAppPat(cons: Cons, t: T): Set[List[T]]

  def liftLit[V](v: V): T
  def liftApp(cons: Cons, xs: List[T]): T
  def liftApp(cons: Symbol, xs: List[T]):T = liftApp(Cons(cons, xs.size), xs)
  def liftApp(cons: Cons, xs: T*): T = liftApp(cons, List(xs:_*))
  def liftApp(cons: Symbol, xs: T*): T = liftApp(Cons(cons, xs.size), List(xs:_*))

  def lift(t: Syntax.Trm): T = t match {
    case Syntax.Trm.Lit(v) => liftLit(v)
    case Syntax.Trm.App(cons, xs) => liftApp(cons, xs map (lift(_)))
  }

  case class Explodable(t: T, explode: T => List[Syntax.Pat]) {
    lazy val ex = explode(t)
    override def toString = "\n Explode: " + ex.toString
  }
  object Explodable {
    def apply(t: T): Explodable = Explodable(t, explode(_, 3))
  }
  def explode(t: T, depth: Int): List[Syntax.Pat]


  def mergeUnion(s1: Map[Cons, List[T]], s2: Map[Cons, List[T]]) = {
    var apps = s1
    for ((c, ys) <- s2)
      s1.get(c) match {
        case None => apps += c -> ys
        case Some(xs) => apps += c -> (xs.zip(ys).map(p => join(p._1, p._2)))
      }
    apps
  }

  def mergeIntersect(m1: Map[Cons, List[T]], m2: Map[Cons, List[T]]) = {
    var apps = Map[Cons, List[T]]()
    for ((c, ys) <- m2)
      m1.get(c) match {
        case None => // no meet
        case Some(xs) =>
          val met = (xs.zip(ys).map(p => meet(p._1,p._2)))
          if (met.isEmpty || met.exists(x => x != bottom))
            apps += c -> met
      }
    apps
  }

  def mergeDiff(m1: Map[Cons, List[T]], m2: Map[Cons, List[T]]): Map[Cons, List[T]] = {
    var apps = Map[Cons, List[T]]()
    for ((c, xs) <- m1)
      m2.get(c) match {
        case None => apps += c -> xs
        case Some(ys) =>
          val d = (xs.zip(ys).map(p => diff(p._1,p._2)))
          val nonbottom = d.count(x => x != bottom)
          if (!d.isEmpty && nonbottom == 1)
            apps += c -> d.zip(xs).map(p => if (p._1 == bottom) p._2 else p._1)
          else if (!d.isEmpty && nonbottom > 1)
            apps += c -> xs
      }
    apps
  }
}
