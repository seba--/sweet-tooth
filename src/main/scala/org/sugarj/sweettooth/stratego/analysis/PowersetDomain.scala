package org.sugarj.sweettooth.stratego.analysis

import org.sugarj.sweettooth.stratego.Syntax

object PowersetDomain extends Domain {
  type T = Option[Set[Trm]] // None represents the infinite set, Some represents finite sets
  def bottom = Some(Set())
  def top = None

  def compare(morePrecise: T, lessPrecise: T): Boolean = (morePrecise, lessPrecise) match {
    case (_,None) => true
    case (None,_) => false
    case (Some(s1), Some(s2)) => s1 subsetOf s2
  }

  def join(t1: T, t2: T): T = (t1,t2) match {
    case (None,_) => None
    case (_,None) => None
    case (Some(s1), Some(s2)) => Some(merge(s1, s2))
  }

  def merge(s1: Set[Trm], s2: Set[Trm]) = {
    var lits = Set[Trm]()
    var apps = Map[Symbol, List[T]]()

    def it(s: Set[Trm]) = s.map(t => t match {
      case l: Lit[_] => lits += l
      case App(cons, xs) =>
        apps.get(cons) match {
          case None => apps += cons -> xs
          case Some(ys) =>
            val args = ys zip xs
            val joined = args.map(p => join(p._1,p._2))
            apps += cons -> joined
        }
    })

    it(s1)
    it(s2)
    lits ++ apps.map(p => App(p._1,p._2))
  }

  def meet(t1: T, t2: T): T = (t1,t2) match {
    case (None,_) => t2
    case (_,None) => t1
    case (Some(s1), Some(s2)) => Some(s1 intersect s2)
  }

  def matchAppPat(cons: Symbol, arity: Int, t: T): Set[List[T]] = t match {
    case None => Set(for (i <- (1 to arity).toList) yield None)
    case Some(s) => for (App(`cons`, xs) <- s if xs.size == arity) yield xs
  }

  def liftLit[V](v: V) = Some(Set(Lit(v)))
  def liftApp(cons: Symbol, xs: List[T]) = Some(Set(App(cons, xs)))
  
  abstract class Trm
  case class Lit[T](v: T) extends Trm
  case class App(cons: Symbol, xs: List[T]) extends Trm {
    override def toString = cons.name + "(" + listString(xs) + ")"

    def listString(xs: List[T]): String = xs match {
      case Nil => ""
      case x::Nil => x.toString
      case x::xs => x.toString + ", " + listString(xs)
    }
  }

  def App(cons: Symbol): App = App(cons, List())
  def App(cons: Symbol, x: T, xs: T*): App = App(cons, x::List(xs:_*))
}