//package org.sugarj.sweettooth.stratego.analysis.v4
//
//import org.sugarj.sweettooth.stratego.analysis.base.StackTrait
//import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}
//
///**
//* Created by seba on 25/09/14.
//*/
//trait GraphStack[D <: BoxDomain] extends StackTrait[D] {
//  type Call = (Symbol, Map[Symbol, Closure], Map[Symbol, Val])
//
//  def emptyStack = new Stack()
//
//  import dom.Box
//
//  class Stack extends super.Stack {
//    var st: List[(Call, Val)] = List()
//    var boxes: Map[(Call, Val), Box] = Map()
//
//    def terminate(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, Val], current: Val, store: Store): Option[Val] = {
//      val call = ((f, sargs, targs), current)
//      if (st.contains(call)) {
//        boxes.get(call) match {
//          case None =>
//            val b = Box(dom.top)
//            boxes += call -> b
//            Some(b)
//          case b@Some(_) => b
//        }
//      }
//      else
//        None
//    }
//
//    def push(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, Val], current: Val, store: Store) {
//      st = ((f, sargs, targs), current) :: st
//    }
//
//    def popSuccess(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, Val], current: Val, store: Store, result: Val) = {
//      val call = ((f, sargs, targs), current)
//      assert(st.head == call)
//      st = st.tail
//
//      boxes.get(call) match {
//        case None => result
//        case Some(b) =>
//          b.target = result
//          b
//      }
//    }
//
//    def popFail(f: Symbol, sargs: Map[Symbol, Closure], targs: Map[Symbol, Val], current: Val, store: Store) = {
//      val call = ((f, sargs, targs), current)
//      assert(st.head == call)
//      st = st.tail
//    }
//  }
//}
