package org.sugarj.sweettooth.stratego.analysis.base

import org.sugarj.sweettooth.stratego.Semantics.Fail
import org.sugarj.sweettooth.stratego.Syntax.{Build, Pat, Defs, Exp}
import org.sugarj.sweettooth.stratego.analysis.domain.Domain

/**
 * Created by seba on 09/09/14.
 */
trait AnalyzeBase[V, D <: Domain[V]] extends StoreTrait[V, D] with StackTrait[V, D] {

  val dom: D
  var defs: Defs = Map()

  def analyze(e: Exp, current: V, store: Store, stack: Stack): (V, Store) @throws[Fail]

  def fail(current: Exp, msg: String = "") = throw Fail(current, msg)

  def normalize(p: Pat, store: Store): V = p match {
    case Pat.Lit(v) => dom.liftLit(v)
    case Pat.Var(x) =>
      store.lookup(x) match {
        case Some(t) => t
        case None => fail(Build(p), s"Unbound variable $x. Existing bindings ${store.store}")
      }
    case Pat.App(cons, xs) => dom.liftApp(cons, xs map (normalize(_, store)))
  }
}
