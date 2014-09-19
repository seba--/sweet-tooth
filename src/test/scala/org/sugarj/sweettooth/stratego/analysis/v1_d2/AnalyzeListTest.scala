package org.sugarj.sweettooth.stratego.analysis.v1_d2

import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.analysis.AnalyzeListSuite

import scala.language.implicitConversions

/**
* Created by seba on 30/07/14.
*/
class AnalyzeListTest extends AnalyzeListSuite with Config {
  val nil = lift(Trm.App('Nil))
  val cons1 = lift(Trm.App('Cons, Trm.App('Foo), Trm.App('Nil)))
  def cons(l: List[Trm]) = lift(Trm.App('Foo)::l)
  val cons_top_Nil = dom.liftApp('Cons, dom.top, dom.liftApp('Nil))
  val cons_Zero_top = dom.liftApp('Cons, dom.liftApp('Nil), dom.top)
  val pair_to_list_top_top = dom.liftApp('Cons, dom.top, dom.liftApp('Cons, dom.top, dom.liftApp('Nil)))
  val pair_to_list_top_Zero = dom.liftApp('Cons, dom.top, dom.liftApp('Cons, dom.liftApp('Zero), dom.liftApp('Nil)))
  val pair_to_list_Zero_top = dom.liftApp('Cons, dom.liftApp('Zero), dom.liftApp('Cons, dom.top, dom.liftApp('Nil)))
  val pair_to_list_Zero_One = dom.liftApp('Cons, dom.liftApp('Zero), dom.liftApp('Cons, dom.liftApp('One), dom.liftApp('Nil)))
  def map(l: List[Trm]) = lift(l.map(elem => Trm.App('_, Trm.App('Zero), Trm.App(Symbol(s"Elem_${l.length - l.indexOf(elem) - 1}")))))
  val map_top = dom.join(dom.liftApp('Nil), dom.liftApp('Cons, dom.liftApp('Zero), dom.top))
}
