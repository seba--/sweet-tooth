package org.sugarj.sweettooth.stratego.analysis.v4_d1

import org.sugarj.sweettooth.stratego.analysis.AnalyzeGenericSuite

import scala.language.implicitConversions

/**
* Created by seba on 30/07/14.
*/
class AnalyzeGenericTest extends AnalyzeGenericSuite with Config {
  val id_top = dom.top
  val id_Zero = dom.liftApp('Zero)
  val fail_top = dom.bottom
  val fail_Zero = dom.bottom
  val not_id_top = dom.top // dom.bottom
  val not_id_Zero = dom.liftApp('Zero)
  val not_fail_top = dom.top
  val not_fail_Zero = dom.liftApp('Zero)
  val not_isFoo_Foo = dom.liftApp('Foo)
  val not_isFoo_Bar = dom.liftApp('Bar)
}
