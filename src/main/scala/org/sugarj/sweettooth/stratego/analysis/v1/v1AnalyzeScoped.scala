package org.sugarj.sweettooth.stratego.analysis.v1

import org.sugarj.sweettooth.stratego.Syntax.Exp
import org.sugarj.sweettooth.stratego.analysis.base.{AnalyzeScoped, AnalyzeSeq}
import org.sugarj.sweettooth.stratego.analysis.domain.Domain

/**
  * Created by seba on 09/09/14.
  */
trait v1AnalyzeScoped[V, D <: Domain[V]] extends AnalyzeScoped[V,D] {
  def analyzeScoped(x: Symbol, e: Exp, current: V, store: Store, stack: Stack): (V, Store) = {
    val orig = store.lookup(x)
    val (res, subStore) = analyze(e, current, Store(store.store - x, store.sstore), stack)
    orig match {
      case None => (res, subStore)
      case Some(t) => (res, Store(subStore.store + (x -> t), subStore.sstore))
    }
  }
}
