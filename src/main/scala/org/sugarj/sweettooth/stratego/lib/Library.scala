package org.sugarj.sweettooth.stratego.lib

import org.sugarj.sweettooth.stratego.Syntax
import org.sugarj.sweettooth.stratego.Syntax.Def

/**
 * Created by seba on 04/08/14.
 */
trait Library {
  val DEFS: Map[Symbol, Syntax.Def]

  def +(l2: Library): Library = {
    val l1 = this
    new Library {
      override val DEFS: Map[Symbol, Def] = l1.DEFS ++ l2.DEFS
    }
  }
}

