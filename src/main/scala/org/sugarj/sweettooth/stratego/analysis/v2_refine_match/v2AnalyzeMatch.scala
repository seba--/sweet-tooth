package org.sugarj.sweettooth.stratego.analysis.v2_refine_match

import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.analysis.base._
import org.sugarj.sweettooth.stratego.analysis.domain.Domain
import org.sugarj.sweettooth.stratego.Semantics.Fail
/**
  * Created by seba on 09/09/14.
  */
trait v2AnalyzeMatch[V, D <: Domain[V]] extends AnalyzeMatch[V,D] {
  def analyzeMatch(p: Pat, current: V, store: Store, stack: Stack): (V, Store) = {
    val (refined, mStore) = matchPat(p, current, store)

//    assert (dom.compare(refined, current), s"$refined is not more precise than $current")
//    if (!dom.compare(current, refined))
//      println(s"Refined\n  $current to\n  $refined")

    (refined, mStore)
  }

  def matchPat(p: Pat, t: V, store: Store): (V, Store) = p match {
    case Pat.Lit(v) =>
      val lit = dom.liftLit(v)
      if (dom.compare(lit, t)) // v maybe matches t
        (dom.meet(lit, t), store)
      else fail(Match(p), s"Could not match pattern $p against term $t")
    case Pat.Var(x) => store.lookup(x) match {
      case Some(t1) =>
        val meet = dom.meet(t1, t)
        if (meet != dom.bottom)
          (meet, store)
        else fail(Match(p), s"Could not match pattern $p against term $t, expected $t1")
      case None => (t, store + (x, t))
    }
    case Pat.App(cons, args) =>
      val argLists = dom.matchAppPat(cons, t)
      if (argLists.isEmpty)
        fail(Match(p), s"Mismatching pattern. Expected $p, was $t")

      val newArgs = argLists flatMap (ys =>
        try {
          var itStore = store
          val matchedArgs = args.zip(ys).map(xy => {
            val (v, matchStore) = matchPat(xy._1, xy._2, itStore)
            itStore = matchStore
            v
          })
          Some((matchedArgs, itStore))
        } catch {
          case Fail(e,msg) =>
            None
        }
        )

      if (newArgs.isEmpty)
        fail(Match(p), s"Mismatching pattern. Expected $p, was $t")

      val joinedArgs = newArgs.map(_._1).reduce((xs,ys) => xs.zip(ys).map(xy => dom.join(xy._1,xy._2)))
      val joinedStore = newArgs.map(_._2).foldLeft(store)((st, newStore) => st join newStore)

      (dom.liftApp(cons, joinedArgs), joinedStore)
  }
}
