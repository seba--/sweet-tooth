//package org.sugarj.sweettooth.stratego.analysis.v4
//
//import org.sugarj.sweettooth.stratego.Syntax._
//import org.sugarj.sweettooth.stratego.analysis.base._
//import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}
//import org.sugarj.sweettooth.stratego.Semantics.Fail
//import org.sugarj.sweettooth.stratego.analysis.v2_refine_match.v2AnalyzeMatch
//
///**
//  * Created by seba on 09/09/14.
//  */
//trait v4AnalyzeMatch[V <: BoxableVal[V], D <: BoxDomain[V]] extends v2AnalyzeMatch[V,D] {
//  override def matchPat(p: Pat, t: V, store: Store): (V, Store) =
//    if (!t.isBox)
//      super.matchPat(p, t, store)
//    else {
//      val (refinement, mStore) = super.matchPat(p, t, store)
//      if (t eq refinement)
//        (refinement, mStore)
//      else {
//        val b = dom.makeBoxMeet(t.asInstanceOf[Box[V]], refinement)
//        (b, mStore)
//      }
//    }
//}
