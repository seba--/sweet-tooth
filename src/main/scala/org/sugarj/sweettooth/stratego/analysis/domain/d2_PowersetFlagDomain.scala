//package org.sugarj.sweettooth.stratego.analysis.domain
//
//import org.sugarj.sweettooth.stratego.Syntax.{Cons, Pat, litLT, pLT, Trm}
//
//import scala.collection.IterableLike
//
//object d2_PowersetFlagDomain {
//  import Trm.Lit
//
//  case class MFin(lits: Set[Lit[_]], apps: Map[Cons, List[Val]], inf: Boolean) extends Val {
//    def isEmpty = lits.isEmpty && apps.isEmpty
//    def size = lits.size + apps.size
//
//    override def toString =
//      if (inf && isEmpty)
//        "?"
//      else if (inf)
//        "?{" + finToString + "}"
//      else
//        "{" + finToString + "}"
//
//    def finToString: String = {
//      val slits = lits.toList.sortWith(litLT)
//      val litString = intersperse(slits, ", ")
//      val sapps = apps.toList.sortWith(pLT)
//      val appString = interspersePairs(sapps, ", ")
//
//      if (litString.isEmpty || appString.isEmpty)
//        litString + appString
//      else
//        litString + ", " + appString
//    }
//
//    def interspersePairs[T, U](xs: Iterable[(T,List[U])], s: String): String =
//      if (xs.isEmpty)
//        ""
//      else if (xs.tail.isEmpty) {
//        val p = xs.head
//        p._1 + "->(" + intersperse(p._2, ", ") + ")"
//      }
//      else {
//        val p = xs.head
//        p._1 + "->(" + intersperse(p._2, ", ") + ")" + s + interspersePairs(xs.tail, s)
//      }
//
//    def intersperse[T](xs: Iterable[T], s: String): String =
//      if (xs.isEmpty)
//        ""
//      else if (xs.tail.isEmpty)
//        xs.head.toString
//      else
//        xs.head.toString + s + intersperse(xs.tail, s)
//  }
//
//  trait D extends Domain {
//   def bottom: Val = MFin(Set(), Map(), false)
//   def top: Val = MFin(Set(), Map(), true)
//
//    def compare(morePrecise: Val, lessPrecise: Val): Boolean = (morePrecise, lessPrecise) match {
//      case (_,MFin(_,_,true)) => true
//      case (MFin(_,_,true),_) => false
//      case (MFin(lits1,apps1,_), MFin(lits2,apps2,_)) =>
//
//        def findInS2(t1: (Cons, List[Val])): Boolean = apps2.get(t1._1) match {
//          case None => false
//          case Some(args2) => t1._2.zip(args2).forall{p => compare(p._1, p._2)}
//        }
//
//        lits1.forall(lits2.contains(_)) && apps1.forall(findInS2)
//    }
//
//
//    def join(t1: Val, t2: Val) = (t1,t2) match {
//      case (MFin(lits1,apps1,inf1), MFin(lits2,apps2,inf2)) =>
//        MFin(lits1 ++ lits2, mergeUnion(apps1, apps2), inf1 || inf2)
//    }
//
//    def meet(t1: Val, t2: Val) = (t1,t2) match {
//      case (MFin(lits1,apps1,inf1), MFin(lits2,apps2,inf2)) =>
//        MFin(lits1 intersect lits2, mergeIntersect(apps1, apps2), inf1 || inf2)
//    }
//
//    def matchAppPat(cons: Cons, t: Val): Set[List[Val]] = t match {
//      case MFin(lits, apps, inf) =>
//        val args: Set[List[Val]] = apps.get(cons) match {
//          case None => Set()
//          case Some(xs) => Set(xs)
//        }
//
//        if (inf) {
//          val topArgList = for (i <- (1 to cons.ar).toList) yield top
//          args + topArgList
//        }
//        else
//          args
//    }
//
//
//   def liftLit[V](v: V) = MFin(Set(Lit(v)), Map(), false)
//   def liftApp(cons: Cons, xs: List[Val]) = MFin(Set(), Map(cons -> xs), false)
//
////    def explode(t: Val, depth: Int): List[Pat] = {
////      val litExplode = t.fin.lits.toList map {case Lit(l) => Pat.Lit(l)}
////      val appExplode = t.fin.apps flatMap(p => explodeApp(p._1, p._2, depth))
////      val ls = litExplode ++ appExplode
////      if (t.inf)
////        Pat.Var('?)::ls
////      else
////        ls
////    }
////
////    def explodeApp(cons: Cons, args: List[T], depth: Int): List[Pat] =
////      if (depth <= 1)
////        List(Pat.Var('_ooo))
////      else {
////        val ls = args map (explode(_, depth-1))
////        crossProduct(ls) map (args => Pat.App(cons, args))
////      }
////
////    def crossProduct[T](tss: List[List[T]]): List[List[T]] =
////      if (tss.isEmpty)
////        List(List())
////      else if (tss.tail.isEmpty)
////        tss.head map (List(_))
////      else {
////        val rest = crossProduct(tss.tail)
////        for (prod <- rest;
////             ts <- tss.head)
////        yield ts :: prod
////      }
//  }
//}