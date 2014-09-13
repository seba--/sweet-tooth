package org.sugarj.sweettooth.stratego.lib

import org.sugarj.sweettooth.stratego.Syntax._

/**
 * Created by seba on 30/07/14.
 */
object String extends Library {
  def buildString(s: String) = Build(makeString(s))
  def makeString(s: String) = 'String@@(makeCharList(s))
  def makeCharList(s: String): Pat =
    if (s.isEmpty)
      'Nil@@()
    else
      'Cons@@(Pat.Lit(s.head), makeCharList(s.tail))

  val empty_string = 'empty_string_0 -> Def(!!('String@@('Nil@@())))

  val conc_strings = 'conc_strings_0_0 -> Def(
    If(??('_@@('String@@('xs), 'String@@('ys))),
      !!('_@@('xs,'ys)),
      If(??('_@@('String@@('xs), 'String@@('ys), 'String@@('zs))),
        !!('_@@('xs, 'ys, 'zs)),
        Call('fail_0_0)
      )
    ),
    Call('conc_0_0),
    ??('chars),
    !!('String@@('chars))
  )

  val string_replace = 'string_replace_0_2 -> Def(scala.List(), scala.List('old, 'new),
    ??('String@@('xs)),
    !!('old),
    ??('String@@('old_chars)),
    !!('new),
    ??('String@@('new_chars)),
    !!('xs),
    Call('replace_0_2, scala.List(), scala.List('old_chars, 'new_chars)),
    !!('String@@('ys))
  )

  val DEFS = Generic.DEFS ++ List.DEFS ++ Map(
    empty_string,
    conc_strings,
    string_replace
  )
}
