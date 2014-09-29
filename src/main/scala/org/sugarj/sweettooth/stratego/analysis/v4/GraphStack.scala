package org.sugarj.sweettooth.stratego.analysis.v4

import org.sugarj.sweettooth.stratego.analysis.base.StackTrait
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

/**
* Created by seba on 25/09/14.
*/
trait GraphStack[V <: BoxableVal[V], D <: BoxDomain[V]] extends StackTrait[V, D] {
  type Call = (Symbol, Map[Symbol, Closure], Map[Symbol, V])

  def emptyStack = new Stack()

  class Stack extends super.Stack {
    var st: List[(Call, V)] = List()
    var boxes: Map[(Call, V), V] = Map()

    def terminate(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, V], current: V, store: Store): Option[V] = {
      val call = ((f, sargs, targs), current)
      if (st.contains(call)) {
        boxes.get(call) match {
          case None =>
            val b = dom.makeBox(dom.top)
            boxes += call -> b
            Some(b)
          case b@Some(_) =>
            Some(b.get)
        }
      }
      else
        None
    }

    def push(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, V], current: V, store: Store) {
      st = ((f, sargs, targs), current) :: st
    }

    def popSuccess(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, V], current: V, store: Store, result: V) = {
      val call = ((f, sargs, targs), current)
      assert(st.head == call)
      st = st.tail

      boxes.get(call) match {
        case None => result
        case Some(b) =>
          b.target = result
          b
      }
    }

    def popFail(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, V], current: V, store: Store) = {
      val call = ((f, sargs, targs), current)
      assert(st.head == call)
      st = st.tail
    }
  }
}
