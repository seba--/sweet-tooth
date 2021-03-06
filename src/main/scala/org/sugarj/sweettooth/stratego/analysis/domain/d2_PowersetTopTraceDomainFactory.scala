package org.sugarj.sweettooth.stratego.analysis.domain

import org.sugarj.sweettooth.stratego.Semantics.Fail
import org.sugarj.sweettooth.stratego.Syntax._

trait d2_PowersetTopTraceDomainFactory {
  import Trm.Lit

  type Vx <: Val[Vx]
  val domain: D

  abstract class V extends Val[Vx]
  
  class MFin(val lits: Set[Lit[_]], val apps: Map[Cons, List[Vx]], val inf: Boolean) extends V {
    def isBottom = !inf && lits.isEmpty && apps.isEmpty
    def isTop = inf
    def ||(v: Vx) = v match {
      case MFin(lits2, apps2, inf2) => domain.makeMFin(lits ++ lits2, mergeUnion(apps, apps2), inf || inf2)
    }

    def &&(v: Vx) = v match {
      case MFin(lits2, apps2, inf2) => {
        if (inf && !inf2)
          domain.makeMFin(lits2, apps2, inf2)
        else if (!inf && inf2)
          domain.makeMFin(lits, apps, inf)
        else domain.makeMFin(lits intersect lits2, mergeIntersect(apps, apps2), inf || inf2)
      }
    }

    def <=(lessPrecise: Vx) = lessPrecise match {
      case MFin(_, _, inf2) if inf => inf2
      case MFin(_, _, true) => true
      case MFin(lits2, apps2, _) =>
        def findInS2(t1: (Cons, List[Vx])): Boolean = apps2.get(t1._1) match {
          case None => false
          case Some(args2) => t1._2.zip(args2).forall{p => (p._1 <= p._2)}
        }
        lits.forall(lits2.contains(_)) && apps.forall(findInS2)
    }

    def >=(morePrecise: Vx) = morePrecise <= domain.makeMFin(lits, apps, inf)
    def matchCons(cons: Cons) = {
      apps.get(cons) match {
        case None =>
          if (inf)
            for (i <- (1 to cons.ar).toList) yield domain.top
          else
            throw new Fail(Match(Pat.App(cons, List())), s"Match failed, expected $cons was $this")
        case Some(xs) =>
          if (inf)
            xs map (_ || domain.top)
          else
            xs
      }
    }

    def isEmpty = if (inf) throw new IllegalStateException("Cannot call `isEmpty` on infinite val") else lits.isEmpty && apps.isEmpty
    def size = if (inf) throw new IllegalStateException("Cannot call `size` on infinite val") else lits.size + apps.size

    override def toString = {
      val slits = lits.toList.sortWith(litLT)
      var litString = slits.toString
      if (lits.isEmpty)
        litString = "Set("
      else
        litString = litString.substring(0, litString.length - 1)

      val appString = StringBuilder.newBuilder
      apps.foreach(p => appString append (p._1.name + "(" + listString(p._2) + "), "))
      if (!apps.isEmpty)
        appString.delete(appString.length - 2, appString.length)

      if (inf)
        "?" + litString + appString + ")"
      else
        litString + appString + ")"
    }

    def listString(xs: List[Vx]): String = xs match {
      case Nil => ""
      case x::Nil => x.toString
      case x::xs => x.toString + ", " + listString(xs)
    }

    override def hashCode = lits.hashCode * 31 + apps.hashCode + inf.hashCode

    override def equals(a: Any) =
      if (a.isInstanceOf[MFin]) {
        val f = a.asInstanceOf[MFin]
        f.lits == lits && f.apps == apps && f.inf == inf
      }
      else
        false

  }
  object MFin {
    def apply(lits: Set[Lit[_]], apps: Map[Cons, List[Vx]], inf: Boolean) = domain.makeMFin(lits, apps, inf)
    def unapply(v: Vx): Option[(Set[Lit[_]], Map[Cons, List[Vx]], Boolean)] =
      if (v.isInstanceOf[MFin]) {
        val w = v.asInstanceOf[MFin]
        Some((w.lits, w.apps, w.inf))
      }
      else
        None
  }

  trait D extends Domain[Vx] {
    def bottom: Vx = makeMFin(Set(), Map(), false)
    def top: Vx = makeMFin(Set(), Map(), true)

    def liftLit[T](v: T): Vx = makeMFin(Set(Lit(v)), Map(), false)
    def liftApp(cons: Cons, xs: List[Vx]): Vx = makeMFin(Set(), Map(cons -> xs), false)

    def makeMFin(lits: Set[Lit[_]], apps: Map[Cons, List[Vx]], inf: Boolean): Vx
  }
}