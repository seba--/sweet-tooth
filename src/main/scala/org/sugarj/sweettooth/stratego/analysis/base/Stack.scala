package org.sugarj.sweettooth.stratego.analysis.base

import org.sugarj.sweettooth.stratego.Syntax.{Pat, Exp}
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

/**
  * Created by seba on 09/09/14.
  */
trait StackTrait[V <: Val[V], D <: Domain[V]] extends StoreTrait[V, D] {
  def emptyStack: Stack

  trait Stack {
    def terminate(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, V], current: V, store: Store): Option[V]
    def push(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, V], current: V, store: Store)
    def popSuccess(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, V], current: V, store: Store, result: V): V
    def popFail(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, V], current: V, store: Store)
  }
}

trait BasicStack[V <: Val[V], D <: Domain[V]] extends StackTrait[V, D] {
  type Call = (Symbol, Map[Symbol, Closure], Map[Symbol, V])
  type Current = V

  def emptyStack = new Stack()

  class Stack() extends super.Stack {
    var st: List[(Call, Current)] = List()

    def terminate(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, V], current: V, store: Store): Option[V] =
      if (st.contains(((f, sargs, targs), current)))
        Some(dom.top)
      else
        None

    def push(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, V], current: V, store: Store) {
      st = ((f, sargs, targs), current) :: st
    }

    def popSuccess(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, V], current: V, store: Store, result: V) = {
      assert(st.head == ((f, sargs, targs), current))
      st = st.tail
      result
    }

    def popFail(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, V], current: V, store: Store) {
      assert(st.head == ((f, sargs, targs), current))
      st = st.tail
    }
  }
}
