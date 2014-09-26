package org.sugarj.sweettooth.stratego.analysis

import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.analysis.domain.Val
import org.sugarj.sweettooth.stratego.lib.Generic

import scala.language.implicitConversions

/**
* Created by seba on 30/07/14.
*/
abstract class AnalyzeGenericSuite extends AnalysisSuite {
  val baseLib = Generic

  val id_top: Val
  test_analysis("id top")(Call('id_0_0), dom.top)(id_top)
  
  val id_Zero: Val
  test_analysis("id zero")(Call('id_0_0), dom.liftApp('Zero))(id_Zero)

  val fail_top: Val
  test_analysis(s"fail top")(Call('fail_0_0), dom.top)(fail_top)

  val fail_Zero: Val
  test_analysis("fail zero")(Call('fail_0_0), dom.liftApp('Zero))(fail_Zero)

  val not_id_top: Val
  test_analysis("not id top")(Call('not_1_0, List(Call('id_0_0)), List()), dom.top)(not_id_top)

  val not_id_Zero: Val
  test_analysis("not id zero")(Call('not_1_0, List(Call('id_0_0)), List()), dom.liftApp('Zero))(not_id_Zero)

  val not_fail_top: Val
  test_analysis("not fail top")(Call('not_1_0, List(Call('fail_0_0)), List()), dom.top)(not_fail_top)

  val not_fail_Zero: Val
  test_analysis("not fail zero")(Call('not_1_0, List(Call('fail_0_0)), List()), dom.liftApp('Zero))(not_fail_Zero)

  val not_isFoo_Foo: Val
  test_analysis("not ?Foo Foo")(Call('not_1_0, List(??('Foo@@())), List()), dom.liftApp('Foo))(not_isFoo_Foo)

  val not_isFoo_Bar: Val
  test_analysis("not ?Foo Bar")(Call('not_1_0, List(??('Foo@@())), List()), dom.liftApp('Bar))(not_isFoo_Bar)
}
