package org.sugarj.sweettooth.stratego.lib

import org.sugarj.sweettooth.stratego.Syntax._

/**
 * Created by seba on 04/08/14.
 */
object List extends Library {
  val nil = 'nil_0_0 -> Def(!!('Nil@@()))
  val cons = 'cons_0_0 -> Def(??('_@@('x, 'xs)), !!('Cons@@('x, 'xs)))

  def mkList(l: List[Trm], n: Int = 0): Exp = l match {
    case Nil => Call('nil_0_0)
    case x::xs => Seqs(mkList(xs, n+1), ??(Symbol(s"xs$n")), !!('_@@(x, Symbol(s"xs$n"))), Call('cons_0_0))
  }

  val map = 'map_1_0 -> Def(scala.List('s), scala.List(),
    If(??('Nil@@()),
      !!('Nil@@()),
      Seqs(
        ??('Cons@@('x, 'xs)),
        !!('x),
        SVar('s),
        ??('t),
        !!('xs),
        Call('map_1_0, scala.List(SVar('s)), scala.List()),
        ??('ts),
        !!('Cons@@('t, 'ts)))))

  val fetch = 'fetch_1_0 -> Def(scala.List('s), scala.List(),
    ??('Cons@@('x, 'xs)),
    !!('x),
    If_(SVar('s),
      ??('y),
      !!('Cons@@('y, 'xs)))
    Else (
      !!('xs),
      Call('fetch_1_0, scala.List(SVar('s)), scala.List()),
      ??('ys),
      !!('Cons@@('x, 'ys))
    )
  )

  val elem = 'elem_0_0 -> Def(
    ??('_@@('e, 'xs)),
    !!('xs),
    Call('fetch_1_0, scala.List(??('e)), scala.List())
  )

  val at_end = 'at_end_1_0 -> Def(scala.List('s), scala.List(),
    If(??('Nil@@()),
      SVar('s),
      Seqs(
        ??('Cons@@('x, 'xs)),
        !!('xs),
        Call('at_end_1_0, scala.List(SVar('s)), scala.List()),
        ??('ys),
        !!('Cons@@('x, 'ys))))
  )

  val conc = 'conc_0_0 -> Def(
    If_(??('_@@('xs, 'ys)),
      !!('xs),
      Call('at_end_1_0, scala.List(!!('ys)), scala.List()))
    Else (
      If(??('_@@('xs, 'ys, 'zs)),
        !!('ys),
        Call('at_end_1_0, scala.List(!!('zs)), scala.List()),
        ??('ys_zs),
        !!('xs),
        Call('at_end_1_0, scala.List(!!('ys_zs)), scala.List()))
      Else (
        Call('fail_0_0)
      )
    )
  )

  val starts_with = 'starts_with_0_0 -> Def(
    If(??('_@@('xs, 'Nil@@())),
      !!('xs),
      Seqs(
        ??('_@@('Cons@@('x, 'xs), 'Cons@@('x, 'ys))),
        !!('_@@('xs, 'ys)),
        Call('starts_with_0_0)
      )
    )
  )

  val replace = 'replace_0_2 -> Def(scala.List(), scala.List('old, 'new),
    If(??('Nil@@()),
      !!('Nil@@()),
      If(Seqs(??('xs), !!('_@@('xs, 'old)), Call('starts_with_0_0)),
        Seqs(
          Call('replace_0_2, scala.List(), scala.List('old, 'new)),
          ??('rest),
          !!('_@@('new, 'rest)),
          Call('conc_0_0)
        ),
        Seqs(
          ??('Cons@@('y, 'ys)),
          !!('ys),
          Call('replace_0_2, scala.List(), scala.List('old, 'new)),
          ??('rest),
          !!('Cons@@('y, 'rest))
        )
      )
    )
  )

  val DEFS = Generic.DEFS ++ Map(
    nil,
    cons,
    map,
    fetch,
    elem,
    at_end,
    conc,
    starts_with,
    replace
  )
}
