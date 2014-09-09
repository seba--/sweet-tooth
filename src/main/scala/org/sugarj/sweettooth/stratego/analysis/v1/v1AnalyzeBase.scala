package org.sugarj.sweettooth.stratego.analysis.v1

import org.sugarj.sweettooth.stratego.Syntax.{Build, Pat}
import org.sugarj.sweettooth.stratego.analysis.base.AnalyzeBase
import org.sugarj.sweettooth.stratego.analysis.domain.Domain

/**
 * Created by seba on 09/09/14.
 */
trait v1AnalyzeBase[V, D <: Domain[V]] extends AnalyzeBase[V, D] {

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
