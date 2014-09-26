package org.sugarj.sweettooth.stratego.analysis.v1

import org.sugarj.sweettooth.stratego.Syntax.Exp
import org.sugarj.sweettooth.stratego.analysis.base.{AnalyzeScoped, AnalyzeSeq}
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

/**
  * Created by seba on 09/09/14.
  */
trait v1AnalyzeScoped[D <: Domain] extends AnalyzeScoped[D] {
  def analyzeScoped(x: Symbol, e: Exp, current: Val, store: Store, stack: Stack): (Val, Store) = {
    val orig = store.lookup(x)
    val (res, subStore) = analyze(e, current, Store(store.store - x, store.sstore), stack)
    orig match {
      case None => (res, Store(subStore.store - x, subStore.sstore))
      case Some(t) => (res, Store(subStore.store + (x -> t), subStore.sstore))
    }
  }
}
