package org.sugarj.sweettooth.stratego.analysis.v1

import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.analysis.base._
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}
import org.sugarj.sweettooth.stratego.Semantics.Fail
/**
  * Created by seba on 09/09/14.
  */
trait v1AnalyzeMatch[V <: Val[V], D <: Domain[V]] extends AnalyzeMatch[V,D] {
  def analyzeMatch(p: Pat, current: V, store: Store, stack: Stack): (V, Store) = {
    val mStore = matchPat(p, current, store)
    (current, mStore)
  }

  def matchPat(p: Pat, t: V, store: Store): Store = p match {
    case Pat.Lit(v) =>
      if (dom.liftLit(v) <= t) // v maybe matches t
        store
      else fail(Match(p), s"Could not match pattern $p against term $t")
    case Pat.Var(x) => store.lookup(x) match {
      case Some(t1) =>
        if ((t1 && t) > dom.bottom)
          store
        else fail(Match(p), s"Could not match pattern $p against term $t, expected $t1")
      case None => store + (x, t)
    }
    case Pat.App(cons, xs) =>
      val argLists = t.matchCons(cons)

      var st = store
      xs.zip(argLists) map { p =>
        try {
          st = st join matchPat(p._1, p._2, st)
        } catch {
          case Fail(_,_) => fail(Match(p._1), s"Mismatching pattern. Expected ${p._1}, was ${p._2}")
        }
      }
      st
  }
}
