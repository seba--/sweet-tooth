package org.sugarj.sweettooth.stratego.lib

import org.sugarj.sweettooth.stratego.Syntax._

/**
 * Created by seba on 30/07/14.
 */
object Num extends Library {
  val zero = 'zero_0_0 -> Def(!!('Zero@@()))
  val succ = 'succ_0_0 -> Def(??('n), !!('Succ@@('n)))

  def mkNat(n: Int): Exp = n match {
    case 0 => Call('zero_0_0)
    case n => Seq(mkNat(n-1), Call('succ_0_0))
  }

  val plus = 'plus_0_0 -> Def(
      If(??('_@@('Zero@@(), 'n)),
         !!('n),
         Seqs(
           ??('_@@('Succ@@('m), 'n)),
           !!('_@@('m, 'n)),
           Call('plus_0_0),
           ??('res),
           !!('Succ@@('res)))))

  val DEFS = Map(
    zero,
    succ,
    plus
  )
}
