package org.sugarj.sweettooth.stratego.analysis.v3_struct_lists

import org.sugarj.sweettooth.stratego.Semantics
import org.sugarj.sweettooth.stratego.Semantics.{Fail,tryFail}
import org.sugarj.sweettooth.stratego.Syntax.{Exp, Match, Pat, Cons}
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

trait ConcatenableVal[V <: Val[V]] extends Val[V] {

  val dom: Domain[V]

  // TODO implement <=, >=
  // TODO canonical list representation with Conc ?

  abstract override def &&(v: V): V = {
    if (this == v)
      return v

    if (this.isTop || v.isTop)
      return super.&&(v)

    var sup = super.&&(v)
    tryFail {
      this.matchCons(Cons('_Nil, 0))
      v.matchCons(Cons('_Nil, 0))
      sup = sup || dom.liftApp('_Nil)
    }
    if (!this.isTop || !v.isTop)
      tryFail {
        merge(tryFail(this.matchCons(Cons('_Conc, 2))), tryFail(v.matchCons(Cons('_Conc, 2))), null, null)
        val List(hd1,tl1) = this.matchCons(Cons('_Cons, 2))
        val List(hd2,tl2) = v.matchCons(Cons('_Cons, 2))
        val (hd,tl) = (hd1 && hd2, tl1 && tl2)
        sup = sup || dom.liftApp('_Cons, hd, tl)
      }
    sup
  }

  abstract override def matchCons(cons: Cons) = {
    if (isTop) // It would even be safe to run this.matchAppPat even for `top <= t`. But does this improve precision?
      super.matchCons(cons)
    else if (cons == Cons('_Nil, 0)) {
      val superRes = tryFail{super.matchCons(cons)}
      val nilres = tryFail {
        val concres = super.matchCons(Cons('_Conc, 2))
        matchNilConc(concres(0), concres(1))
      }
      merge(nilres, superRes, Match(Pat.App('_Nil)), s"Expected _Nil, was $this")
    }
    else if (cons == Cons('_Cons, 2)) {
      val superRes = tryFail{super.matchCons(cons)}
      val consres = tryFail {
        val concres = super.matchCons(Cons('_Conc, 2))
        matchConsConc(concres(0), concres(1))
      }
      merge(consres , superRes, Match(Pat.App('_Nil)), s"Expected _Nil, was $this")
    }
    else
      super.matchCons(cons)
  }

  private def merge(v1: Option[List[V]], v2: Option[List[V]], errorExp: Exp, msg: String): List[V] = (v1,v2) match {
    case (None, None) => throw new Fail(errorExp, msg)
    case (Some(v),None) => v
    case (None,Some(v)) => v
    case (Some(v1),Some(v2)) => v1.zip(v2).map(p => p._1 || p._2)
  }

  def matchNilConc(left: V, right: V): List[V] = {
    left.matchCons(Cons('_Nil, 0))
    right.matchCons(Cons('_Nil, 0))
    List()
  }

  def matchConsConc(left: V, right: V): List[V] = {
    val leftCons = left.matchCons(Cons('_Cons, 2))
    val rightCons = right.matchCons(Cons('_Cons, 2))
    var allCons = List[V]()

    // left can be Nil
    val nilCons = tryFail {
      left.matchCons(Cons('_Nil, 0))
      right.matchCons(Cons('_Cons, 2))
    }

    val consCons = tryFail {
      val ls = left.matchCons(Cons('_Cons, 2))
      List(ls(0), dom.liftApp('_Conc, ls(1), right))
    }

    merge(nilCons, consCons, Match(Pat.App('_Cons, 'x, 'y)), s"Expected _Cons, was Conc($left, $right)")
  }
}
