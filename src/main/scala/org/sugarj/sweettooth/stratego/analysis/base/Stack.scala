package org.sugarj.sweettooth.stratego.analysis.base

import org.sugarj.sweettooth.stratego.Syntax.{Pat, Exp}
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

/**
  * Created by seba on 09/09/14.
  */
trait StackTrait[D <: Domain] extends StoreTrait[D] {
  def emptyStack: Stack

  trait Stack {
    def terminate(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, Val], current: Val, store: Store): Option[Val]
    def push(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, Val], current: Val, store: Store)
    def popSuccess(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, Val], current: Val, store: Store, result: Val): Val
    def popFail(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, Val], current: Val, store: Store)
  }
}

trait BasicStack[D <: Domain] extends StackTrait[D] {
  type Call = (Symbol, Map[Symbol, Closure], Map[Symbol, Val])
  type Current = Val

  def emptyStack = new Stack()

  class Stack() extends super.Stack {
    var st: List[(Call, Current)] = List()

    def terminate(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, Val], current: Val, store: Store): Option[Val] =
      if (st.contains(((f, sargs, targs), current)))
        Some(dom.top)
      else
        None

    def push(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, Val], current: Val, store: Store) {
      st = ((f, sargs, targs), current) :: st
    }

    def popSuccess(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, Val], current: Val, store: Store, result: Val) = {
      assert(st.head == ((f, sargs, targs), current))
      st = st.tail
      result
    }

    def popFail(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, Val], current: Val, store: Store) {
      assert(st.head == ((f, sargs, targs), current))
      st = st.tail
    }
  }
}
