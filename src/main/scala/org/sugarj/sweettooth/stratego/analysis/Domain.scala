package org.sugarj.sweettooth.stratego.analysis

import org.sugarj.sweettooth.stratego.Syntax

trait Domain {
  type T
  def top: T
  def bottom: T
  def compare(morePrecise: T, lessPrecise: T): Boolean
  def join(t1: T, t2: T): T
  def meet(t1: T, t2: T): T

  def matchAppPat(cons: Symbol, arity: Int, t: T): Set[List[T]]

  def liftLit[V](v: V): T
  def liftApp(cons: Symbol, xs: List[T]): T
  def liftApp(cons: Symbol, xs: T*): T

  def lift(t: Syntax.Trm): T = t match {
    case Syntax.Trm.Lit(v) => liftLit(v)
    case Syntax.Trm.App(cons, xs) => liftApp(cons, xs map (lift(_)))
  }
}