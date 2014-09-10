package org.sugarj.sweettooth.stratego.analysis.v1

import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.analysis.base._
import org.sugarj.sweettooth.stratego.analysis.domain.Domain

/**
  * Created by seba on 09/09/14.
  */
trait v1AnalyzeSVar[V, D <: Domain[V]] extends AnalyzeSVar[V,D] with v1AnalyzeBase[V,D] {
  def analyzeSVar(s: Symbol, current: V, store: Store, stack: Stack): (V, Store) =
    store.slookup(s) match {
      case Some(Closure(fe, clStore)) =>
        val (t, fstore) = analyze(fe, current, clStore.store, stack)
        clStore.store = fstore
        (t, store)
      case None => fail(SVar(s), s"Undefined strategy variable $s")
    }
}
