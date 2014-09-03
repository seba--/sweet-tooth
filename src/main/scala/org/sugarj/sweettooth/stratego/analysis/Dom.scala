package org.sugarj.sweettooth.stratego.analysis


object Dom extends Domain {
  type T = Set[Trm]
  
  abstract class Trm
  case class Lit[T](v: T) extends Trm
  case class App(cons: Symbol, xs: List[Trm]) extends Trm {
    override def toString =
      cons.name + "(" + listString(xs) + ")"

    def listString(xs: List[Trm]): String = xs match {
      case Nil => ""
      case x::Nil => x.toString
      case x::xs => x.toString + ", " + listString(xs)
    }
  }
  case object Unknown extends Trm
  
  def App(cons: Symbol): App = App(cons, List())
  def Appl(cons: Symbol, xs: Trm*): App = App(cons, List(xs:_*))
}