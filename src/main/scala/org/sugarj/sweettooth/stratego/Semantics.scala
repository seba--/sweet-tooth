package org.sugarj.sweettooth.stratego

import org.sugarj.sweettooth.stratego.Syntax._

/**
 * Created by seba on 30/07/14.
 */
object Semantics {
  case class ClosureStore(var store: Store)
  case class Closure(e: Exp, val store: ClosureStore)

  case class Fail(current: Exp, msg: String = "") extends Exception
  def fail(current: Exp, msg: String = "") = throw Fail(current, msg)

  case class Store(store: Map[Symbol, Trm], sstore: Map[Symbol, Closure]) {
    def lookup(s: Symbol) = store.get(s)
    def slookup(s: Symbol) = sstore.get(s)

    def +(p1: Symbol, p2: Trm) = Store(store + (p1 -> p2), sstore)
    def +(p1: Symbol, p2: Closure) = Store(store, sstore + (p1 -> p2))
  }
  def emptyStore = Store(Map(),Map())

  def eval(e: Exp, current: Trm, defs: Defs): Trm = {

    // use the defs from the surrounding def
    def eval(e: Exp, current: Trm, store: Store): (Trm, Store) = e match {
      case SVar(s) => store.slookup(s) match {
        case Some(cl@Closure(fe, clStore)) =>
          val (res, fstore) = eval(fe, current, clStore.store)
          clStore.store = fstore
          (res, store)
        case None => fail(e, s"Undefined strategy variable $s")
      }
      case Build(t) => (normalize(t, store), store)
      case Match(p) => (current, matchPat(p, current, store))
      case Seq(e1, e2) =>
        val (t1, store1) = eval(e1, current, store)
        eval(e2, t1, store1)
      case If(cnd, thn, els) =>
        (try { Some(eval(cnd, current, store)) }
         catch { case Fail(t, msg) => None })
        match {
          case Some((t1, store1)) => eval(thn, t1, store1)
          case None => eval(els, current, store)
        }
      case Call(f, sargs, targs) =>
        val d = defs.getOrElse(f, throw new RuntimeException(s"Undefined function $f"))
        if (d.svars.size != sargs.size)
          throw new RuntimeException(s"Wrong number of strategy arguments to $f. Expected ${d.tvars}, got $targs")
        if (d.tvars.size != targs.size)
          throw new RuntimeException(s"Wrong number of term arguments to $f. Expected ${d.tvars}, got $targs")
        val tStore = Map() ++ d.tvars.zip(targs map (normalize(_, store)))
        val clStore = ClosureStore(store)
        val sStore = Map() ++ d.svars.zip(sargs.map {case SVar(x) => store.slookup(x).get; case x => Closure(x, clStore)})
        val (t, _) = eval(d.body, current, Store(tStore, sStore))
        (t, clStore.store)
      case Scoped(x, e) =>
        val orig = store.lookup(x)
        val (res, subStore) = eval(e, current, Store(store.store - x, store.sstore))
        orig match {
          case None => (res, subStore)
          case Some(t) => (res, Store(subStore.store + (x -> t), subStore.sstore))
        }
    }

    eval(e, current, emptyStore)._1
  }

  def normalize(p: Pat, store: Store): Trm = p match {
    case Pat.Lit(v) => Trm.Lit(v)
    case Pat.Var(x) =>
      store.lookup(x) match {
        case Some(t) => t
        case None => fail(Build(p), s"Unbound variable $x. Existing bindings ${store.store}")
      }
    case Pat.App(cons, xs) => Trm.App(cons, xs map (normalize(_, store)))
  }

  def matchPat(p: Pat, t: Trm, store: Store): Store = (p,t) match {
    case (Pat.Lit(l1), Trm.Lit(l2)) =>
      if (l1 == l2)
        store
      else fail(Match(p), s"Could not match pattern $p against term $t")
    case (Pat.Var(x), t2) => store.lookup(x) match {
      case Some(t1) =>
        if (t1 == t2)
          store
        else fail(Match(p), s"Could not match pattern $p against term $t2, expected $t1")
      case None => store + (x, t2)
    }
    case (Pat.App(cons1, xs), Trm.App(cons2, ys)) =>
      if (cons1 != cons2)
        fail(Match(p), s"Mismatching constructor. Expected $p, was $t")
      if (xs.size != ys.size)
        fail(Match(p), s"Mismatching constructor arity. Expected $p, was $t")
      xs.zip(ys).foldLeft(store)((st: Store, pt: (Pat, Trm)) => matchPat(pt._1, pt._2, st))
  }
}
