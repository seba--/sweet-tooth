package org.sugarj.sweettooth.stratego.analysis.v4_d1

import org.scalatest.FunSuite
import org.sugarj.sweettooth.stratego.Syntax._

/**
 * Created by seba on 30/07/14.
 */
class BoxTest extends FunSuite with Config {

  val NAT = {
    val box = dom.makeBox(dom.top)
    val nat = dom.liftApp('Zero) || dom.liftApp('Succ, box)
    box.target = nat
    nat
  }

  val atLeastOne1 = {
    val box = dom.makeBox(dom.top)
    val res = dom.liftApp('Zero) || dom.liftApp('Succ, box)
    box.target = res
    dom.liftApp('Succ, res)
  }
  val atLeastOne2 = {
    val box = dom.makeBox(dom.top)
    val res = dom.liftApp('Succ, dom.liftApp('Zero) || box)
    box.target = res
    res
  }
  val atLeastOne3 = {
    dom.liftApp('Succ, dom.liftApp('Zero) || atLeastOne2)
  }

  test("atLeastOne1 == atLeastOne2") { assert(atLeastOne1 == atLeastOne2) }
  test("atLeastOne1 == atLeastOne3") { assert(atLeastOne1 == atLeastOne3) }
  test("atLeastOne2 == atLeastOne3") { assert(atLeastOne2 == atLeastOne3) }


  val pentatope = {
    val a = 'a -> Def(Call('b), Call('c), Call('d), Call('e))
    val b = 'b -> Def(Call('a), Call('c), Call('d), Call('e))
    val c = 'c -> Def(Call('a), Call('b), Call('d), Call('e))
    val d = 'd -> Def(Call('a), Call('b), Call('c), Call('e))
    val e = 'e -> Def(Call('a), Call('b), Call('c), Call('d))

    val defs = Map(a,b,c,d,e)
    analysis.analyze(Call('a), dom.top, defs)
  }

  println(pentatope)
}
