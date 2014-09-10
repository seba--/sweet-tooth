package org.sugarj.sweettooth.stratego.analysis.v1

import org.sugarj.sweettooth.stratego.Syntax.{SVar, Exp, Pat}
import org.sugarj.sweettooth.stratego.analysis.base.AnalyzeCall
import org.sugarj.sweettooth.stratego.analysis.domain.Domain

/**
  * Created by seba on 09/09/14.
  */
trait v1AnalyzeCall[V, D <: Domain[V]] extends AnalyzeCall[V,D] with v1AnalyzeBase[V,D] {
  def analyzeCall(f: Symbol, sargs: List[Exp], targs: List[Pat], current: V, store: Store, stack: Stack): (V, Store) = {
    val d = defs.getOrElse(f, throw new RuntimeException(s"Undefined function $f"))
    if (d.svars.size != sargs.size)
      throw new RuntimeException(s"Wrong number of strategy arguments to $f. Expected ${d.tvars}, got $targs")
    if (d.tvars.size != targs.size)
      throw new RuntimeException(s"Wrong number of term arguments to $f. Expected ${d.tvars}, got $targs")

    val tStore = Map() ++ d.tvars.zip(targs map (normalize(_, store)))

    val clStore = ClosureStore(store)
    val sStore = Map() ++ d.svars.zip(sargs.map {case SVar(v) => store.slookup(v).get; case s => Closure(s, clStore)})

    if (stack.terminate(f, sStore, tStore, current, store))
      (dom.top, store)
    else {
      val extStack = stack.push(f, sStore, tStore, current, store)
      val (t, _) = analyze(d.body, current, Store(tStore, sStore), extStack)
      (t, clStore.store)
    }
  }
}
