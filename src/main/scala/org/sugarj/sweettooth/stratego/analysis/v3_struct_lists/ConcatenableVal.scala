package org.sugarj.sweettooth.stratego.analysis.v3_struct_lists

import org.sugarj.sweettooth.stratego.Syntax.Cons
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

trait ConcatenableVal[V <: ConcatenableVal[V]] extends Val[V] {

//  abstract override def liftApp(cons: Cons, xs: List[T]): T = cons match {
//    case Cons('_Cons, 2) => super.liftApp('_Conc, super.liftApp('_Elem, xs(0)), xs(1))
//    case _ => super.liftApp(cons, xs)
//  }

  abstract override def matchCons(cons: Cons): Set[List[V]] = {
    if (this >= dom.top) // It would even be safe to run this.matchAppPat even for `top <= t`. But does this improve precision?
      super.matchCons(cons)
    else {
      val consres = cons match {
        case Cons('_Nil, 0) =>
          val concres = super.matchCons(Cons('_Conc, 2))
          concres flatMap (x => matchNilConc(x(0), x(1)))
        case Cons('_Cons, 2) =>
          val concres = super.matchCons(Cons('_Conc, 2))
          concres flatMap (x => matchConsConc(x(0), x(1)))
        case _ => Set()
      }
      consres ++ super.matchCons(cons)
    }
  }

  def matchNilConc(left: V, right: V): Set[List[V]] =
    if(!left.matchCons(Cons('_Nil, 0)).isEmpty && !right.matchCons(Cons('_Nil, 0)).isEmpty)
      Set(List())
    else
      Set()

  def matchConsConc(left: V, right: V): Set[List[V]] = {
    val leftCons = left.matchCons(Cons('_Cons, 2))
    val rightCons = right.matchCons(Cons('_Cons, 2))
    var allCons = Set[List[V]]()

    // left can be Nil
    if (!left.matchCons(Cons('_Nil, 0)).isEmpty)
      allCons ++= rightCons

    // left can be Cons  ==>  [left.head | left.tail ++ right]
    allCons ++= leftCons map (l => List(l(0), dom.liftApp('_Conc, l(1), right)))

    allCons
  }
}