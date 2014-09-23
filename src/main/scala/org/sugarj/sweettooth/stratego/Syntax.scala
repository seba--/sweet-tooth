package org.sugarj.sweettooth.stratego

import language.implicitConversions

/**
 * Created by seba on 30/07/14.
 */
object Syntax {

  case class Cons(c: Symbol, ar: Int) {
    if (c=='_Cons && ar==1)
      println("HHHH")
    def name = c.toString
    override def toString = c.toString
  }

  abstract class Trm
  object Trm {
    case class Lit[T](v: T) extends Trm {
      override def toString =
        if (v.isInstanceOf[Char])
          s"'${v.toString}'"
        else if (v.isInstanceOf[String])
          "\"" + v.toString + "\""
        else
          v.toString
    }
    case class App(cons: Cons, xs: List[Trm]) extends Trm {
      override def toString =
        cons.name + "(" + listString(xs) + ")"

      def listString(xs: List[Trm]): String = xs match {
        case Nil => ""
        case x::Nil => x.toString
        case x::xs => x.toString + ", " + listString(xs)
      }
    }

    def App(cons: Symbol, xs: List[Trm]): App = App(Cons(cons, xs.size), xs)
    def App(cons: Symbol): App = App(cons, List())
    def App(cons: Symbol, x:Trm, xs: Trm*): App = App(cons, x::List(xs:_*))
  }
  
  abstract class Pat
  object Pat {
    case class Lit[T](v: T) extends Pat
    case class Var(x: Symbol) extends Pat
    case class App(cons: Cons, xs: List[Pat]) extends Pat

    def App(cons: Symbol, xs: List[Pat]): App = App(Cons(cons, xs.size), xs)
    def App(cons: Symbol, xs: Pat*): App = App(Cons(cons, xs.size), List(xs:_*))
  }

  abstract class Exp
  case class SVar(s: Symbol) extends Exp
  case class Build(t: Pat) extends Exp
  case class Match(p: Pat) extends Exp
  case class Seq(e1: Exp, e2: Exp) extends Exp
  case class If(cnd: Exp, thn: Exp, els: Exp) extends Exp
  case class Call(s: Symbol, sargs: List[Exp], targs: List[Pat]) extends Exp
  case class Scoped(x: Symbol, e: Exp) extends Exp

  case class Def(svars: List[Symbol], tvars: List[Symbol], body: Exp)
  type Defs = Map[Symbol, Def]


  implicit def trmToPat(t: Trm): Pat = t match {
    case Trm.Lit(l) => Pat.Lit(l)
    case Trm.App(cons, xs) => Pat.App(cons, xs map (trmToPat(_)))
  }

  implicit def patvar(s: Symbol) = Pat.Var(s)
  def ??(p: Pat) = Match(p)
  def !!(p: Pat) = Build(p)
  def Seqs(l: Exp*) = SeqsSeq(scala.Seq(l:_*))
  def SeqsSeq(l: scala.Seq[Exp]): Exp = l match {
    case scala.Seq(e) => e
    case scala.Seq(e, es@_*) => Seq(e, SeqsSeq(es))
  }
  def Def(body: Exp): Def = Def(List(), List(), body)
  def Def(svars: List[Symbol], tvars: List[Symbol], e: Exp, body: Exp*): Def = Def(svars, tvars, SeqsSeq(body.+:(e)))
  def Def(body: Exp*): Def = Def(List(), List(), SeqsSeq(body))
  implicit def appcons(s: Symbol) = AppCons(s)
  case class AppCons(s: Symbol) {
    def @@(xs: List[Pat]) = Pat.App(s, xs)
    def @@(xs: Pat*) = Pat.App(s, List(xs:_*))
  }
  def Call(s: Symbol): Call = Call(s, List(), List())

  def If_(cnd: Exp, thn: Exp*) = IfThen(cnd, SeqsSeq(thn))
  def If(cnd: Exp, thn: Exp*) = IfThen(cnd, SeqsSeq(thn))
  case class IfThen(cnd: Exp, thn: Exp) {
    def Else(els: Exp*) = If(cnd, thn, SeqsSeq(els))
  }

  def pLT[T](a: (Cons, T), b: (Cons, T)) = a._1.c.name < b._1.c.name

  def litLT(a: Trm.Lit[_], b: Trm.Lit[_]): Boolean = {
    val x = a.v
    val y = b.v

    if (x.isInstanceOf[String] && y.isInstanceOf[String])
      x.asInstanceOf[String] < y.asInstanceOf[String]
    else if (x.isInstanceOf[Char] && y.isInstanceOf[Char])
      x.asInstanceOf[Char] < y.asInstanceOf[Char]
    else if (x.isInstanceOf[Int] && y.isInstanceOf[Int])
      x.asInstanceOf[Int] < y.asInstanceOf[Int]
    else
      throw new MatchError(s"No order defined for literals $x and $y")
  }
}