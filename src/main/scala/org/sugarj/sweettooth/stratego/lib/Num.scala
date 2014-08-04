package org.sugarj.sweettooth.stratego.lib

import org.sugarj.sweettooth.stratego.Library
import org.sugarj.sweettooth.stratego.Syntax._

/**
 * Created by seba on 30/07/14.
 */
object Num extends Library {
  val zero = Def(!!('Zero@@()))
  val succ = Def(??('n), !!('Succ@@('n)))

  def mkNat(n: Int): Exp = n match {
    case 0 => Call('zero)
    case n => Seq(mkNat(n-1), Call('succ))
  }

  val plus = Def(
      If(??('_@@('Zero@@(), 'n)),
         !!('n),
         Seqs(
           ??('_@@('Succ@@('m), 'n)),
           !!('_@@('m, 'n)),
           Call('plus),
           ??('res),
           !!('Succ@@('res)))))

  val DEFS = Map(
    'zero -> zero,
    'succ -> succ,
    'plus -> plus
  )
}
