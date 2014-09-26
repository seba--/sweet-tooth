package org.sugarj.sweettooth.stratego.analysis.v1

import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.analysis.base._
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

/**
  * Created by seba on 09/09/14.
  */
trait v1AnalyzeSVar[D <: Domain] extends AnalyzeSVar[D] {
  def analyzeSVar(s: Symbol, current: Val, store: Store, stack: Stack): (Val, Store) =
    store.slookup(s) match {
      case Some(Closure(fe, clStore)) =>
        val (t, fstore) = analyze(fe, current, clStore.store, stack)
        clStore.store = fstore
        (t, store)
      case None => fail(SVar(s), s"Undefined strategy variable $s")
    }
}
