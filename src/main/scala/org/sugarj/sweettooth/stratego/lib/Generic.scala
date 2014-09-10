package org.sugarj.sweettooth.stratego.lib

import org.sugarj.sweettooth.stratego.Syntax._

/**
 * Created by seba on 04/08/14.
 */
object Generic extends Library {
  val id = 'id_0_0 -> Def(??('x), !!('x))

  val fail = 'fail_0_0 -> Def(!!('Foo), ??('Bar))

  val not = 'not_1_0 -> Def(scala.List('s), scala.List(),
    If(SVar('s),
      Call('fail_0_0),
      Call('id_0_0)))

  val app = 'app_1_0 -> Def(scala.List('s), scala.List(), SVar('s))

  val DEFS =  Map(
    id,
    fail,
    not,
    app
  )
}
