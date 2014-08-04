package org.sugarj.sweettooth.stratego.lib

import org.sugarj.sweettooth.stratego.Library
import org.sugarj.sweettooth.stratego.Syntax._

/**
 * Created by seba on 04/08/14.
 */
object Generic extends Library {
  val id = Def(??('x), !!('x))

  val app = Def(scala.List('s), scala.List(), SVar('s))

  val DEFS =  Map(
    'id -> id,
    'app -> app
  )
}
