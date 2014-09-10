package org.sugarj.sweettooth.stratego.lib

import org.sugarj.sweettooth.stratego.Syntax._

/**
 * Created by seba on 04/08/14.
 */
object List extends Library {
  val nil = 'nil -> Def(!!('Nil@@()))
  val cons = 'cons -> Def(??('_@@('x, 'xs)), !!('Cons@@('x, 'xs)))

  def mkList(l: List[Trm], n: Int = 0): Exp = l match {
    case Nil => Call('nil)
    case x::xs => Seqs(mkList(xs, n+1), ??(Symbol(s"xs$n")), !!('_@@(x, Symbol(s"xs$n"))), Call('cons))
  }

  val map = 'map -> Def(scala.List('s), scala.List(),
    If(??('Nil@@()),
      !!('Nil@@()),
      Seqs(
        ??('Cons@@('x, 'xs)),
        !!('x),
        SVar('s),
        ??('t),
        !!('xs),
        Call('map, scala.List(SVar('s)), scala.List()),
        ??('ts),
        !!('Cons@@('t, 'ts)))))

  val elem = 'elem -> Def(scala.List(), scala.List(),
    Seqs(
      ??('_@@('e, 'xs)),
      !!('xs),
      ??('Cons@@('y, 'ys)),
      !!('e),
      If(??('y),
        !!('_@@('e, 'xs)),
        Seq(!!('_@@('e, 'ys)), Call('elem)))
    ))


  val DEFS = Map(
    nil,
    cons,
    map,
    elem
  )
}
