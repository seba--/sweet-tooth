package org.sugarj.sweettooth.stratego.analysis.v1_d2

import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.analysis.AnalyzeListSuite

import scala.language.implicitConversions

/**
* Created by seba on 30/07/14.
*/
class AnalyzeListTest extends AnalyzeListSuite with Config {
  val nil = lift(Trm.App('_Nil))
  val cons1 = lift(Trm.App('_Cons, Trm.App('Foo), Trm.App('_Nil)))
  def cons(l: List[Trm]) = lift(Trm.App('Foo)::l)
  val cons_top_Nil = dom.liftApp('_Cons, dom.top, dom.liftApp('_Nil))
  val cons_Zero_top = dom.liftApp('_Cons, dom.liftApp('_Nil), dom.top)
  val pair_to_list_top_top = dom.liftApp('_Cons, dom.top, dom.liftApp('_Cons, dom.top, dom.liftApp('_Nil)))
  val pair_to_list_top_Zero = dom.liftApp('_Cons, dom.top, dom.liftApp('_Cons, dom.liftApp('Zero), dom.liftApp('_Nil)))
  val pair_to_list_Zero_top = dom.liftApp('_Cons, dom.liftApp('Zero), dom.liftApp('_Cons, dom.top, dom.liftApp('_Nil)))
  val pair_to_list_Zero_One = dom.liftApp('_Cons, dom.liftApp('Zero), dom.liftApp('_Cons, dom.liftApp('One), dom.liftApp('_Nil)))
  def map(l: List[Trm]) = lift(l.map(elem => Trm.App('_, Trm.App('Zero), Trm.App(Symbol(s"Elem_${l.length - l.indexOf(elem) - 1}")))))
  val map_top = dom.join(dom.liftApp('_Nil), dom.liftApp('_Cons, dom.liftApp('Zero), dom.top))
  val conc_top = dom.mliftApp('_Cons, dom.top, dom.top)
  val conc_FooBar_top = dom.liftApp('_Cons, dom.liftApp('Foo), dom.liftApp('_Cons, dom.liftApp('Bar), dom.top))
  val conc_top_FooBar = dom.liftApp('_Cons, dom.mliftApp('Foo), dom.mliftApp('_Cons, dom.liftApp('Bar), dom.liftApp('_Nil)))
}
