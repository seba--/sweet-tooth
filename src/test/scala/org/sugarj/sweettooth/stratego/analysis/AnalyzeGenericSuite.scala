package org.sugarj.sweettooth.stratego.analysis

import org.sugarj.sweettooth.stratego.Semantics._
import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.analysis.base.Analysis
import org.sugarj.sweettooth.stratego.analysis.domain.Domain
import org.sugarj.sweettooth.stratego.lib.Generic._

import scala.language.implicitConversions

/**
* Created by seba on 30/07/14.
*/
abstract class AnalyzeGenericSuite extends AnalysisSuite {
  val prefix = this.getClass.getPackage.getName.substring(getClass.getPackage.getName.lastIndexOf('.') + 1)

  type V
  type D <: Domain[V]
  val dom: D

  val analysis: Analysis[V,D]

  def lift(t: Trm) = dom.lift(t)

  val id_top: V
  test(s"$prefix: id top") {
    assertDomT(id_top)(analysis.analyze(Call('id), dom.top, DEFS))
  }

  val id_Zero: V
  test(s"$prefix: id zero") {
    assertDomT(id_Zero)(analysis.analyze(Call('id), dom.liftApp('Zero), DEFS))
  }

  val fail_top: V
  test(s"$prefix: fail top") {
    assertDomT(fail_top)(analysis.analyze(Call('fail), dom.top, DEFS))
  }

  val fail_Zero: V
  test(s"$prefix: fail zero") {
    assertDomT(fail_Zero)(analysis.analyze(Call('fail), dom.liftApp('Zero), DEFS))
  }

  val not_id_top: V
  test(s"$prefix: not id top") {
    assertDomT(not_id_top)(analysis.analyze(Call('not, List(Call('id)), List()), dom.top, DEFS))
  }

  val not_id_Zero: V
  test(s"$prefix: not id zero") {
    assertDomT(not_id_Zero)(analysis.analyze(Call('not, List(Call('id)), List()), dom.liftApp('Zero), DEFS))
  }

  val not_fail_top: V
  test(s"$prefix: not fail top") {
    assertDomT(not_fail_top)(analysis.analyze(Call('not, List(Call('fail)), List()), dom.top, DEFS))
  }

  val not_fail_Zero: V
  test(s"$prefix: not fail zero") {
    assertDomT(not_fail_Zero)(analysis.analyze(Call('not, List(Call('fail)), List()), dom.liftApp('Zero), DEFS))
  }

  val not_isFoo_Foo: V
  test(s"$prefix: not ?Foo Foo") {
    assertDomT(not_isFoo_Foo)(analysis.analyze(Call('not, List(??('Foo@@())), List()), dom.liftApp('Foo), DEFS))
  }

  val not_isFoo_Bar: V
  test(s"$prefix: not ?Foo Bar") {
    assertDomT(not_isFoo_Bar)(analysis.analyze(Call('not, List(??('Foo@@())), List()), dom.liftApp('Bar), DEFS))
  }
}
