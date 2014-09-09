package org.sugarj.sweettooth.stratego.analysis.base

import org.sugarj.sweettooth.stratego.Syntax.Exp
import org.sugarj.sweettooth.stratego.analysis.domain.Domain

/**
 * Created by seba on 09/09/14.
 */
trait StoreTrait[V, D <: Domain[V]] {
  val dom: D
  val emptyStore = Store(Map(), Map())

  case class Store(store: Map[Symbol, V], sstore: Map[Symbol, Closure]) {
    def lookup(s: Symbol) = store.get(s)
    def slookup(s: Symbol) = sstore.get(s)

    def +(p1: Symbol, p2: V) = Store(store + (p1 -> p2), sstore)
    def +(p1: Symbol, p2: Closure) = Store(store, sstore + (p1 -> p2))
    def +(p1: Symbol, p2: Exp) = Store(store, sstore + (p1 -> Closure(p2, this)))

    def join(other: Store): Store = {
      //      if (sstore != other.sstore)
      //        throw new IllegalArgumentException(s"Cannot join Store objects with different sstore\n    ${this.sstore}\n    ${other.sstore}")

      var store = this.store
      for ((x, t) <- other.store)
        store.get(x) match {
          case None => store += (x -> t)
          case Some(t0) => store += (x -> dom.join(t0, t))
        }

      var sstore = this.sstore
      for ((x, cl) <- other.sstore)
        sstore.get(x) match {
          case None => sstore += (x -> cl)
          case Some(cl0) =>
            if (cl.e == cl0.e)
              cl0.store.join(cl.store)
            else
              throw new IllegalArgumentException(s"Cannot join Stores conflicting on strategy definition of $x:\n  ${cl0.e} versus\n ${cl.e}")
        }

      Store(store, sstore)
    }
  }

  case class Closure(e: Exp, store: Store)
}
