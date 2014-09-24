package org.sugarj.sweettooth.stratego.analysis

import org.scalatest.FunSuite
import org.sugarj.sweettooth.stratego.analysis.base.Analysis
import org.sugarj.sweettooth.stratego.analysis.domain.{d2_PowersetFlagDomain, d1_PowersetDomain, Domain}

import scala.language.implicitConversions


class BasicTests extends FunSuite {
  val prefix = this.getClass.getPackage.getName.substring(getClass.getPackage.getName.lastIndexOf('.') + 1)

//  type V
//  type D <: Domain[V]
  object dom extends d2_PowersetFlagDomain.D

//  test (s"$prefix: diff 1") {
//    val x = dom.liftApp('Foo, dom.join(dom.liftLit(1), dom.liftLit(2)), dom.join(dom.liftLit(3), dom.liftLit(4)))
//    val y = dom.liftApp('Foo, dom.join(dom.liftLit(1), dom.liftLit(2)), dom.join(dom.liftLit(3), dom.liftLit(4)))
//    val r = dom.bottom
//    val diff = dom.diff(x, y)
//    assertResult(r)(diff)
//  }
//
//  test (s"$prefix: diff 2") {
//    val x = dom.liftApp('Foo, dom.join(dom.liftLit(1), dom.liftLit(2)), dom.join(dom.liftLit(3), dom.liftLit(4)))
//    val y = dom.liftApp('Foo, dom.liftLit(1),                           dom.join(dom.liftLit(3), dom.liftLit(4)))
//    val r = dom.liftApp('Foo, dom.liftLit(2),                           dom.join(dom.liftLit(3), dom.liftLit(4)))
//    val diff = dom.diff(x, y)
//    assertResult(r)(diff)
//  }
//
//  test (s"$prefix: diff 3") {
//    val x = dom.liftApp('Foo, dom.join(dom.liftLit(1), dom.liftLit(2)), dom.join(dom.liftLit(3), dom.liftLit(4)))
//    val y = dom.liftApp('Foo, dom.liftLit(1),                           dom.liftLit(3))
//    val r = dom.liftApp('Foo, dom.join(dom.liftLit(1), dom.liftLit(2)), dom.join(dom.liftLit(3), dom.liftLit(4)))
//    val diff = dom.diff(x, y)
//    assertResult(r)(diff)
//  }
//
//  test (s"$prefix: diff 4") {
//    val x = dom.liftApp('Foo, dom.join(dom.liftLit(1), dom.top), dom.join(dom.liftLit(3), dom.liftLit(4)))
//    val y = dom.liftApp('Foo, dom.liftLit(1),                    dom.liftLit(3))
//    val r = dom.liftApp('Foo, dom.top,                           dom.liftLit(4))
//    val diff = dom.diff(x, y)
//    assertResult(r)(diff)
//  }
}
