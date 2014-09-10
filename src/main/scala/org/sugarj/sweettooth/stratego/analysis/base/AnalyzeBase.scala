package org.sugarj.sweettooth.stratego.analysis.base

import org.sugarj.sweettooth.stratego.Semantics.Fail
import org.sugarj.sweettooth.stratego.Syntax.{Defs, Exp}
import org.sugarj.sweettooth.stratego.analysis.domain.Domain

/**
 * Created by seba on 09/09/14.
 */
trait AnalyzeBase[V, D <: Domain[V]] extends StoreTrait[V, D] with StackTrait[V, D] {

  val dom: D
  var defs: Defs = Map()

  def analyze(e: Exp, current: V, store: Store, stack: Stack): (V, Store) @throws[Fail]

  def fail(current: Exp, msg: String = "") = throw Fail(current, msg)
}
