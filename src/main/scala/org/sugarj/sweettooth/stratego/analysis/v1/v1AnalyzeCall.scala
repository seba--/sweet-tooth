package org.sugarj.sweettooth.stratego.analysis.v1

import org.sugarj.sweettooth.stratego.Semantics.Fail
import org.sugarj.sweettooth.stratego.Syntax.{SVar, Exp, Pat}
import org.sugarj.sweettooth.stratego.analysis.base.AnalyzeCall
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

/**
  * Created by seba on 09/09/14.
  */
trait v1AnalyzeCall[D <: Domain] extends AnalyzeCall[D] {
  def analyzeCall(f: Symbol, sargs: List[Exp], targs: List[Pat], current: Val, store: Store, stack: Stack): (Val, Store) = {
    val d = defs.getOrElse(f, throw new RuntimeException(s"Undefined function $f"))
    if (d.svars.size != sargs.size)
      throw new RuntimeException(s"Wrong number of strategy arguments to $f. Expected ${d.tvars}, got $targs")
    if (d.tvars.size != targs.size)
      throw new RuntimeException(s"Wrong number of term arguments to $f. Expected ${d.tvars}, got $targs")

    val tStore = Map() ++ d.tvars.zip(targs map (normalize(_, store)))

    val clStore = ClosureStore(store)
    val sStore = Map() ++ d.svars.zip(sargs.map {case SVar(v) => store.slookup(v).get; case s => Closure(s, clStore)})

    stack.terminate(f, sStore, tStore, current, store) match {
      case Some(v) => (v, store)
      case None =>
        stack.push(f, sStore, tStore, current, store)
        try {
          val (t,_) = analyze(d.body, current, Store(tStore, sStore), stack)
          stack.popSuccess(f, sStore, tStore, current, store, t)
          (t, clStore.store)
        } catch {
          case fail:Fail =>
            stack.popFail(f, sStore, tStore, current, store)
            throw fail
        }
    }
  }
}
