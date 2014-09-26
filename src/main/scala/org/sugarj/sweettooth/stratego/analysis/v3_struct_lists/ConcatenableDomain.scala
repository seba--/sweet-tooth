package org.sugarj.sweettooth.stratego.analysis.v3_struct_lists

import org.sugarj.sweettooth.stratego.Syntax.Cons
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

trait ConcatenableDomain extends Domain {

//  abstract override def liftApp(cons: Cons, xs: List[T]): T = cons match {
//    case Cons('_Cons, 2) => super.liftApp('_Conc, super.liftApp('_Elem, xs(0)), xs(1))
//    case _ => super.liftApp(cons, xs)
//  }

  abstract override def matchAppPat(cons: Cons, t: Val): Set[List[Val]] = {
    if (compare(top, t)) // It indeed is safe to run this.matchAppPat even for `top <= t`. But does this improve precision?
      super.matchAppPat(cons, t)
    else {
      val consres = cons match {
        case Cons('_Nil, 0) =>
          val concres = super.matchAppPat(Cons('_Conc, 2), t)
          concres flatMap (x => matchNilConc(x(0), x(1)))
        case Cons('_Cons, 2) =>
          val concres = super.matchAppPat(Cons('_Conc, 2), t)
          concres flatMap (x => matchConsConc(x(0), x(1)))
        case _ => Set()
      }
      consres ++ super.matchAppPat(cons, t)
    }
  }

  def matchNilConc(left: Val, right: Val): Set[List[Val]] =
    if(!super.matchAppPat(Cons('_Nil, 0), left).isEmpty && !super.matchAppPat(Cons('_Nil, 0), right).isEmpty)
      Set(List())
    else
      Set()

  def matchConsConc(left: Val, right: Val): Set[List[Val]] = {
    val leftCons = super.matchAppPat(Cons('_Cons, 2), left)
    val rightCons = super.matchAppPat(Cons('_Cons, 2), right)
    var allCons = Set[List[Val]]()

    // left can be Nil
    if (!super.matchAppPat(Cons('_Nil, 0), left).isEmpty)
      allCons ++= rightCons

    // left can be Cons  ==>  [left.head | left.tail ++ right]
    allCons ++= leftCons map (l => List(l(0), liftApp('_Conc, l(1), right)))

    allCons
  }
}