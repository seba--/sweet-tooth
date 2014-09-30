package org.sugarj.sweettooth.stratego.analysis.v2_refine_match

import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.analysis.base._
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}
import org.sugarj.sweettooth.stratego.Semantics.Fail
/**
  * Created by seba on 09/09/14.
  */
trait v2AnalyzeMatch[V <: Val[V], D <: Domain[V]] extends AnalyzeMatch[V,D] {
  def analyzeMatch(p: Pat, current: V, store: Store, stack: Stack): (V, Store) = {
    val (refined, mStore) = matchPat(p, current, store)

//    assert (dom.compare(refined, current), s"$refined is not more precise than $current")
//    if (!dom.compare(current, refined))
//      println(s"Refined\n  $current to\n  $refined")

    (current && refined, mStore)
  }

  def matchPat(p: Pat, t: V, store: Store): (V, Store) = p match {
    case Pat.Lit(v) =>
      val lit = dom.liftLit(v)
      if (lit <= t) // v maybe matches t
        ((lit && t), store)
      else fail(Match(p), s"Could not match pattern $p against term $t")
    case Pat.Var(x) => store.lookup(x) match {
      case Some(t1) =>
        val meet = t1 && t
        if (meet > dom.bottom)
          (meet, store + (x, meet))
        else fail(Match(p), s"Could not match pattern $p against term $t, expected $t1")
      case None => (t, store + (x, t))
    }
    case Pat.App(cons, args) =>
      val argLists = t.matchCons(cons)

      var st = store
      val newArgs = args.zip(argLists) map { p =>
        try {
          val (v, st2) = matchPat(p._1, p._2, st)
          st = st join st2
          v
        } catch {
          case Fail(_,_) => fail(Match(p._1), s"Mismatching pattern. Expected ${p._1}, was ${p._2}")
        }
      }

      (dom.liftApp(cons, newArgs), st)
  }
}
