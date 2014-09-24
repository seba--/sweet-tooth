package org.sugarj.sweettooth.stratego.analysis.domain

import org.sugarj.sweettooth.stratego.Syntax.{Cons, Pat, litLT, pLT, Trm}

import scala.collection.IterableLike

object d2_PowersetFlagDomain {
  import Trm.Lit

  case class TS(lits: Set[Lit[_]], apps: Map[Cons, List[T]]) {
    def isEmpty = lits.isEmpty && apps.isEmpty
    def size = lits.size + apps.size

    override def toString: String = {
      val slits = lits.toList.sortWith(litLT)
      val litString = intersperse(slits, ", ")
      val sapps = apps.toList.sortWith(pLT)
      val appString = interspersePairs(sapps, ", ")

      if (litString.isEmpty || appString.isEmpty)
        litString + appString
      else
        litString + ", " + appString
    }

    def interspersePairs[T, U](xs: Iterable[(T,List[U])], s: String): String =
      if (xs.isEmpty)
        ""
      else if (xs.tail.isEmpty) {
        val p = xs.head
        p._1 + "->(" + intersperse(p._2, ", ") + ")"
      }
      else {
        val p = xs.head
        p._1 + "->(" + intersperse(p._2, ", ") + ")" + s + interspersePairs(xs.tail, s)
      }

    def intersperse[T](xs: Iterable[T], s: String): String =
      if (xs.isEmpty)
        ""
      else if (xs.tail.isEmpty)
        xs.head.toString
      else
        xs.head.toString + s + intersperse(xs.tail, s)
  }

  case class T(fin: TS, inf: Boolean) {
    override def toString =
      if (inf && fin.isEmpty)
        "?"
      else if (inf)
        "?{" + fin.toString + "}"
      else
        "{" + fin.toString + "}"

  }

  trait D extends Domain[T] {
   def bottom = T(TS(Set(), Map()), false)
   def top = T(TS(Set(), Map()), true)

    def compare(morePrecise: T, lessPrecise: T): Boolean = (morePrecise, lessPrecise) match {
      case (_,T(_,true)) => true
      case (T(_,true),_) => false
      case (T(s1,_), T(s2,_)) =>

        def findInS2(t1: (Cons, List[T])): Boolean = s2.apps.get(t1._1) match {
          case None => false
          case Some(args2) => t1._2.zip(args2).forall{p => compare(p._1, p._2)}
        }

        s1.lits.forall(s2.lits.contains(_)) && s1.apps.forall(findInS2)
    }


    def join(t1: T, t2: T) = T(TS(t1.fin.lits ++ t2.fin.lits, mergeUnion(t1.fin.apps, t2.fin.apps)), t1.inf || t2.inf)
    def meet(t1: T, t2: T) = T(TS(t1.fin.lits intersect t2.fin.lits, mergeIntersect(t1.fin.apps, t2.fin.apps)), t1.inf || t2.inf)
//    def diff(t1: T, t2: T): T = T(TS(t1.fin.lits -- t2.fin.lits, mergeDiff(t1.fin.apps, t2.fin.apps)), t1.inf)

    def matchAppPat(cons: Cons, t: T): Set[List[T]] = {
      val args: Set[List[T]] = t.fin.apps.get(cons) match {
        case None => Set()
        case Some(xs) => Set(xs)
      }

      if (t.inf) {
        val topArgList = for (i <- (1 to cons.ar).toList) yield top
        args + topArgList
      }
      else
        args
   }

   def liftLit[V](v: V) = T(TS(Set(Lit(v)), Map()), false)
   def liftApp(cons: Cons, xs: List[T]) = T(TS(Set(), Map(cons -> xs)), false)

    def explode(t: T, depth: Int): List[Pat] = {
      val litExplode = t.fin.lits.toList map {case Lit(l) => Pat.Lit(l)}
      val appExplode = t.fin.apps flatMap(p => explodeApp(p._1, p._2, depth))
      val ls = litExplode ++ appExplode
      if (t.inf)
        Pat.Var('?)::ls
      else
        ls
    }

    def explodeApp(cons: Cons, args: List[T], depth: Int): List[Pat] =
      if (depth <= 1)
        List(Pat.Var('_ooo))
      else {
        val ls = args map (explode(_, depth-1))
        crossProduct(ls) map (args => Pat.App(cons, args))
      }

    def crossProduct[T](tss: List[List[T]]): List[List[T]] =
      if (tss.isEmpty)
        List(List())
      else if (tss.tail.isEmpty)
        tss.head map (List(_))
      else {
        val rest = crossProduct(tss.tail)
        for (prod <- rest;
             ts <- tss.head)
        yield ts :: prod
      }
  }
}