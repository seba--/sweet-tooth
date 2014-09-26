package org.sugarj.sweettooth.stratego.analysis.v1

import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.analysis.base._
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}
import org.sugarj.sweettooth.stratego.Semantics.Fail
/**
  * Created by seba on 09/09/14.
  */
trait v1AnalyzeMatch[D <: Domain] extends AnalyzeMatch[D] {
  def analyzeMatch(p: Pat, current: Val, store: Store, stack: Stack): (Val, Store) = {
    val mStore = matchPat(p, current, store)
    (current, mStore)
  }

  def matchPat(p: Pat, t: Val, store: Store): Store = p match {
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
      val argLists = dom.matchAppPat(cons, t)
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
