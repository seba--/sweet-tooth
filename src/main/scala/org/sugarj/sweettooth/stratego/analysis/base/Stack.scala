package org.sugarj.sweettooth.stratego.analysis.base

import org.sugarj.sweettooth.stratego.Syntax.{Pat, Exp}
import org.sugarj.sweettooth.stratego.analysis.domain.Domain

/**
  * Created by seba on 09/09/14.
  */
trait StackTrait[V, D <: Domain[V]] extends StoreTrait[V,D] {
  def emptyStack: Stack

  trait Stack {
    def terminate(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, V], current: V, store: Store): Option[V]
    def push(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, V], current: V, store: Store): Stack
  }
}

trait BasicStack[V, D <: Domain[V]] extends StackTrait[V,D] {
  type Call = (Symbol, Map[Symbol, Closure], Map[Symbol, V])
  type Current = V

  def emptyStack = Stack(List())

  case class Stack(st: List[(Call, Current)]) extends super.Stack {
    def terminate(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, V], current: V, store: Store): Option[V] =
      if (st.contains(((f, sargs, targs), current)))
        Some(dom.top)
      else
        None

    def push(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, V], current: V, store: Store): Stack =
      Stack(((f, sargs, targs), current) :: st)
  }
}

trait GraphStack[V, D <: Domain[V]] extends StackTrait[V,D] {
  type Call = (Symbol, Map[Symbol, Closure], Map[Symbol, V])
  type Current = V

  def emptyStack = Stack(List())

  case class Stack(st: List[(Call, Current)]) extends super.Stack {
    def terminate(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, V], current: V, store: Store): Option[V] = {
      val index = st.indexOf(((f, sargs, targs), current))
      if (index >= 0)
        Some(dom.top)
      else
        None
    }


    def push(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, V], current: V, store: Store): Stack =
      Stack(((f, sargs, targs), current) :: st)
  }
}
