package org.sugarj.sweettooth.stratego.analysis.v1

import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.analysis.base._
import org.sugarj.sweettooth.stratego.analysis.domain.Domain

/**
  * Created by seba on 09/09/14.
  */
trait v1AnalyzeMatch[V, D <: Domain[V]] extends AnalyzeMatch[V,D] {
  def analyzeMatch(p: Pat, current: V, store: Store, stack: Stack): (V, Store) = {
    val mStore = matchPat(p, current, store)
    (current, mStore)
  }

  def matchPat(p: Pat, t: V, store: Store): Store = p match {
    case Pat.Lit(v) =>
      if (dom.compare(dom.liftLit(v), t)) // v maybe matches t
        store
      else fail(Match(p), s"Could not match pattern $p against term $t")
    case Pat.Var(x) => store.lookup(x) match {
      case Some(t1) =>
        if (dom.meet(t1, t) != dom.bottom)
          store
        else fail(Match(p), s"Could not match pattern $p against term $t, expected $t1")
      case None => store + (x, t)
    }
    case Pat.App(cons, xs) =>
      val argLists = dom.matchAppPat(cons, xs.size, t)
      if (argLists.isEmpty)
        fail(Match(p), s"Mismatching pattern. Expected $p, was $t")

      val newStores = argLists flatMap (ys =>
        try {
          Some(xs.zip(ys).foldLeft[Store](store)((st, pt) => matchPat(pt._1, pt._2, st)))
        } catch {
          case Fail(e,msg) =>
            //            println(s"Elimated argument list ys; $msg\n    $e")
            None
        }
        )

      if (newStores.isEmpty)
        fail(Match(p), s"Mismatching pattern. Expected $p, was $t")

      newStores.foldLeft(store)((st, newStore) => st join newStore)
  }
}
