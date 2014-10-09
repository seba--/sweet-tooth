package org.sugarj.sweettooth.stratego.analysis.v4

import org.sugarj.sweettooth.stratego.analysis.base.StackTrait
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

/**
* Created by seba on 25/09/14.
*/
trait GraphStack[V <: Val[V], D <: BoxDomain[V]] extends StackTrait[V, D] {
  val STACK_LOG = true

  type Call = (Symbol, Map[Symbol, Closure], Map[Symbol, V])

  def emptyStack = new Stack()

  class Stack extends super.Stack {
    var st: List[(Call, V)] = List()
    var boxes: Map[(Call, V), V with BoxedVal[V]] = Map()

    def terminate(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, V], current: V, store: Store): Option[V] = {
      val call = ((f, sargs, targs), current)
      if (boxes.contains(call)) {
        Some(boxes(call))
      }
      else if (st.contains(call)) {
        val b = dom.makeBox(dom.top)
        boxes += call -> b
        Some(b)
      }
      else {
        if (STACK_LOG) {
          var indent = ""
          (1 to st.size) foreach (_ => indent += "  ")
          println(s"$indent=>$f($current)")
        }
        None
      }
    }

    def push(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, V], current: V, store: Store) {
      st = ((f, sargs, targs), current) :: st
    }

    def popSuccess(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, V], current: V, store: Store, result: V) = {
      val call = ((f, sargs, targs), current)
      assert(st.head == call)
      st = st.tail

      if (STACK_LOG) {
        var indent = ""
        (1 to st.size) foreach (_ => indent += "  ")
        println(s"$indent<=$f($result)")
      }

      boxes.get(call) match {
        case None => result
        case Some(b) =>
          if (b.isStable) {
            val c = (result <= b.current)
            assert(result <= b.current)
            b
          }
          else if (b.target >= result) {
            b.target = result
            b
          }
          else
            throw new IllegalStateException(s"Boxed value is more precise than result of recursive call\n    box = ${b.target}\n    res = $result")
      }
    }

    def popFail(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, V], current: V, store: Store) = {
      val call = ((f, sargs, targs), current)
      assert(st.head == call)
      st = st.tail
    }
  }
}
