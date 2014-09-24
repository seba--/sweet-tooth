package org.sugarj.sweettooth.stratego.analysis.v3_struct_lists

import org.sugarj.sweettooth.stratego.Syntax.{SVar, Exp, Pat}
import org.sugarj.sweettooth.stratego.analysis.base.AnalyzeCall
import org.sugarj.sweettooth.stratego.analysis.domain.{d3_ConcDomain, Domain}
import org.sugarj.sweettooth.stratego.analysis.v1.v1AnalyzeCall

/**
  * Created by seba on 09/09/14.
  */
trait v3AnalyzeCall[V, D <: d3_ConcDomain[V]] extends v1AnalyzeCall[V,D] {
  override def analyzeCall(f: Symbol, sargs: List[Exp], targs: List[Pat], current: V, store: Store, stack: Stack): (V, Store) = {
    val d = defs.getOrElse(f, throw new RuntimeException(s"Undefined function $f"))
    if (d.svars.size != sargs.size)
      throw new RuntimeException(s"Wrong number of strategy arguments to $f. Expected ${d.tvars}, got $targs")
    if (d.tvars.size != targs.size)
      throw new RuntimeException(s"Wrong number of term arguments to $f. Expected ${d.tvars}, got $targs")

    if (f.name == "at_end_1_0")
      at_end(sargs.head, current, store, stack)
    else
      super.analyzeCall(f, sargs, targs, current, store, stack)
  }

  def at_end(s: Exp, current: V, store: Store, stack: Stack): (V, Store) = {
    // `Nil` or `Cons(?,?)`
    if (dom.compare(current, dom.liftApp('_Nil)) ||
        dom.compare(current, dom.liftApp('_Cons, dom.top, dom.top)))
      super.analyzeCall('at_end_1_0, List(s), List(), current, store, stack)
    else {
      val (sres,st) = analyze(s, dom.liftApp('_Nil), store, stack)
      (dom.liftApp('_Conc, current, sres), st)
    }
  }
}
