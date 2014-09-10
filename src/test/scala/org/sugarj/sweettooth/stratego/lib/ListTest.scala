package org.sugarj.sweettooth.stratego.lib

import org.scalatest._
import org.sugarj.sweettooth.stratego.Semantics._
import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.lib.List._
import org.sugarj.sweettooth.stratego.{Semantics, Syntax}

import language.implicitConversions

/**
 * Created by seba on 30/07/14.
 */
class ListTest extends FunSuite {

  implicit def mkListTrm(l: List[Trm]) = eval(mkList(l), Trm.App('Foo, scala.List()), DEFS)
  def mkListOfLength(n: Int): scala.List[Trm] =
    if (n == 0)
      scala.List()
    else
      Trm.App(Symbol(s"Foo$n"))::mkListOfLength(n-1)

  def assertTrm(expected: Trm)(actual: Trm) = assertResult(expected)(actual)

  test("nil") {
    assertTrm(Trm.App('Nil))(eval(Call('nil), Trm.App('Foo), DEFS))
  }

  test("cons1") {
    assertTrm(Trm.App('Cons, Trm.App('Foo), Trm.App('Nil)))(
              eval(Call('cons), Trm.App('_, Trm.App('Foo), Trm.App('Nil)), DEFS))
  }

  test("mkList"){
    assert(mkListTrm(scala.List()) != null)
    assert(mkListTrm(scala.List(Trm.App('Foo1))) != null)
    assert(mkListTrm(scala.List(Trm.App('Foo1), Trm.App('Foo2))) != null)
  }

  test("cons") {
    for (i <- 1 to 20) {
      val l = mkListOfLength(i)
      assertTrm(Trm.App('Foo)::l)(eval(Call('cons), Trm.App('_, Trm.App('Foo), l), DEFS))
    }
  }

  test(s"map") {
    for (i <- 1 to 20) {
      val l = mkListOfLength(i)
      val s = Scoped('x, Seq(Match('x), Build('_@@('Zero@@(), 'x))))
      assertTrm(l.map(t => Trm.App('_, Trm.App('Zero), t)))(
                eval(Call('map, scala.List(s), scala.List()), l, DEFS ++ Num.DEFS))
    }
  }
}
