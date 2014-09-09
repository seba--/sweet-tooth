package org.sugarj.sweettooth.stratego.analysis.v1

import org.sugarj.sweettooth.stratego.Syntax.Exp
import org.sugarj.sweettooth.stratego.analysis.base.AnalyzeIf
import org.sugarj.sweettooth.stratego.analysis.domain.Domain

/**
  * Created by seba on 09/09/14.
  */
trait v1AnalyzeIf[V, D <: Domain[V]] extends AnalyzeIf[V,D] {
  def analyzeIf(cnd: Exp, thn: Exp, els: Exp, current: V, store: Store, stack: Stack): (V, Store) = {
    val vCnd = try {
      Some(analyze(cnd, current, store, stack))
    } catch {
      case Fail(_, _) => None
    }

    vCnd match {
      case None => analyze(els, current, store, stack)
      case Some((t1, store1)) =>
        val (vThn, storeThn) = try {
          analyze(thn, t1, store1, stack)
        } catch {
          case _: Fail => (dom.bottom, emptyStore)
        }

        val (vEls, storeEls) = try {
          analyze(els, current, store1, stack)
        } catch {
          case f: Fail =>
            if (vThn == dom.bottom)
              throw f
            else
              (dom.bottom, emptyStore)
        }

        (dom.join(vThn, vEls), storeThn.join(storeEls))
    }
  }
}
