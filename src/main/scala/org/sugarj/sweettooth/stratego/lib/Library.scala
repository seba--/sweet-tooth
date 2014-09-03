package org.sugarj.sweettooth.stratego.lib

import org.sugarj.sweettooth.stratego.Syntax

/**
 * Created by seba on 04/08/14.
 */
trait Library {
  val DEFS: Map[Symbol, Syntax.Def]
}
