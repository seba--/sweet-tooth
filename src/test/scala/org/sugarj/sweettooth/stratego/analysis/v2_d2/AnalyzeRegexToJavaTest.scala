package org.sugarj.sweettooth.stratego.analysis.v2_d2

import org.sugarj.sweettooth.stratego.analysis.AnalyzeRegexToJavaStringSuite

import scala.language.implicitConversions

/**
 * Created by seba on 30/07/14.
 */
class AnalyzeRegexToJavaTest extends AnalyzeRegexToJavaStringSuite with Config {

  val bracket_top = // [??*
    dom.liftApp('String,
      dom.liftApp('Cons,
        dom.liftLit('['),
        dom.join(
          dom.liftApp('Cons, dom.top, dom.top),
          dom.liftApp('Cons, dom.liftLit(']'), dom.liftApp('Nil)))))

  val bracket_c = lift("[c]")

  val ce2str_range_top =
    dom.join(
      dom.liftApp('String, dom.liftApp('Cons, dom.top, dom.top)),
      dom.liftApp('String, dom.liftApp('Cons, dom.liftLit('-'), dom.top)))

  val aList = dom.join(
    dom.liftApp('Nil),
    dom.liftApp('Cons, dom.top, dom.top)
  )

  val ce2str_top_alts = List(
    dom.top,
    dom.liftApp('String, dom.liftApp('Cons, dom.liftLit('['), dom.liftApp('Cons, dom.liftLit(']'), aList))),
    dom.liftApp('String, dom.liftApp('Cons, dom.liftLit('['), dom.liftApp('Cons, dom.top, dom.liftApp('Cons, dom.liftLit(']'), dom.join(dom.top, dom.liftApp('Nil)))))),
    dom.liftApp('String, dom.liftApp('Cons, dom.liftLit('['), dom.liftApp('Cons, dom.top, aList))),
    dom.liftApp('String, dom.liftApp('Cons, dom.liftLit('['), dom.liftApp('Cons, dom.liftLit('-'), aList))),
    dom.liftApp('String, dom.liftApp('Cons, dom.liftLit('['), dom.liftApp('Cons, dom.liftLit('^'), aList))),
    dom.liftApp('String, dom.liftApp('Cons, dom.liftLit('['), dom.top)),
    dom.liftApp('String, dom.liftApp('Cons, dom.liftLit('.'), dom.liftApp('Nil))),
    dom.liftApp('String, dom.liftApp('Cons, dom.liftLit('\\'), dom.top))
  )
  val ce2str_top = ce2str_top_alts.reduce(dom.join)

  val regex_top =
    dom.liftApp('Lit,
      dom.liftApp('String,
        dom.liftApp('Cons,
          dom.liftApp('Chars,
            dom.liftApp('String, dom.top)),
          dom.liftApp('Nil))))
}
