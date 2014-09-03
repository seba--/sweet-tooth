package org.sugarj.sweettooth.stratego.analysis

import org.sugarj.sweettooth.stratego.Syntax._

/**
 * Created by seba on 03/09/14.
 */
class Analyze(val dom: Domain) {
  case class AClosure(e: Exp, store: AStore)
  case class AStore(store: Map[Symbol, dom.T], sstore: Map[Symbol, AClosure]) {
    def lookup(s: Symbol) = store.get(s)
    def slookup(s: Symbol) = sstore.get(s)

    def +(p1: Symbol, p2: dom.T) = AStore(store + (p1 -> p2), sstore)
    def +(p1: Symbol, p2: AClosure) = AStore(store, sstore + (p1 -> p2))
    def +(p1: Symbol, p2: Exp) = AStore(store, sstore + (p1 -> AClosure(p2, this)))

    def join(other: AStore): AStore = {
      if (sstore != other.sstore)
        throw new IllegalArgumentException(s"Cannot join AStore objects with different sstore\n    ${this.sstore}\n    ${other.sstore}")

      var store = this.store

      for ((x,t) <- other.store)
        store.get(x) match {
          case None => store += (x -> t)
          case Some(t0) => store += (x -> dom.join(t0, t))
        }

      AStore(store, this.sstore)
    }
  }
  def emptyStore = AStore(Map(),Map())

  case class Fail(current: Exp, msg: String = "") extends Exception
  def fail(current: Exp, msg: String = "") = throw Fail(current, msg)

  def analyze(e: Exp, current: dom.T, defs: Defs): dom.T = {
    // use the defs from the surrounding def
    def analyze(e: Exp, current: dom.T, store: AStore): (dom.T, AStore) = e match {
      case SVar(s) => store.slookup(s) match {
        case Some(AClosure(fe, fstore)) =>
          val (t,_) = analyze(fe, current, fstore)
          (t, store)
        case None => fail(e, s"Undefined strategy variable $s")
      }
      case Build(t) => (normalize(t, store), store)
      case Match(p) => (current, matchPat(p, current, store))
      case Seq(e1, e2) =>
        val (t1, store1) = analyze(e1, current, store)
        analyze(e2, t1, store1)
      case If(cnd, thn, els) =>
        (try { Some(analyze(cnd, current, store)) }
        catch { case Fail(t, msg) => None })
        match {
          case Some((t1, store1)) => analyze(thn, t1, store1)
          case None => analyze(els, current, store)
        }
      case Call(f, sargs, targs) =>
        val d = defs.getOrElse(f, throw new RuntimeException(s"Undefined function $f"))
        if (d.svars.size != sargs.size)
          throw new RuntimeException(s"Wrong number of strategy arguments to $f. Expected ${d.tvars}, got $targs")
        if (d.tvars.size != targs.size)
          throw new RuntimeException(s"Wrong number of term arguments to $f. Expected ${d.tvars}, got $targs")
        val tStore = Map() ++ d.tvars.zip(targs map (normalize(_, store)))
        val sStore = Map() ++ d.svars.zip(sargs map (AClosure(_, store)))
        val (t, _) = analyze(d.body, current, AStore(tStore, sStore))
        (t, store)
    }

    try { analyze(e, current, emptyStore)._1 }
    catch { case e@Fail(_, msg) => println(msg); throw e }
  }

  def normalize(p: Pat, store: AStore): dom.T = p match {
    case Pat.Lit(v) => dom.liftLit(v)
    case Pat.Var(x) =>
      store.lookup(x) match {
        case Some(t) => t
        case None => fail(Build(p), s"Unbound variable $x. Existing bindings ${store.store}")
      }
    case Pat.App(cons, xs) => dom.liftApp(cons, xs map (normalize(_, store)))
  }

  def matchPat(p: Pat, t: dom.T, store: AStore): AStore = p match {
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

      var finalStore = store
      for (ys <- argLists)
        try {
          val newStore = xs.zip(ys).foldLeft[AStore](store)((st, pt) => matchPat(pt._1, pt._2, st))
          finalStore = finalStore join newStore
        } catch {
          case Fail(e,msg) => println(s"Elimated argument list ys; $msg\n    $e")
        }

      finalStore
  }
}
