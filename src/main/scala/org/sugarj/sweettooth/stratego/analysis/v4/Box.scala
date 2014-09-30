package org.sugarj.sweettooth.stratego.analysis.v4

import org.sugarj.sweettooth.stratego.Syntax.Cons
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

trait MutableVal[V <: Val[V]] extends Val[V] {
  def current: V
}

trait BoxedVal[V <: Val[V]] extends MutableVal[V] {
  var target: V
  def current = target
}

trait NoBox[V <: Val[V]] extends Val[V] {
  val dom: BoxDomain[V]

  abstract override def ||(v: V) = v match {
    case b: MutableVal[V] => b || this.asInstanceOf[V]
//    case b: MMeet[V] => dom.makeMJoin(b, v)
//    case b: MJoin[V] => dom.makeMJoin(b, v)
    case _ => super.||(v)
  }
  abstract override def &&(v: V) = v match {
    case b: MutableVal[V] => b && this.asInstanceOf[V]
//    case b: MMeet[V] => dom.makeMMeet(b, v)
//    case b: MJoin[V] => dom.makeMMeet(b, v)
    case _ => super.&&(v)
  }

  abstract override def <=(lessPrecise: V) = lessPrecise match {
    case Box(target) => this <= target
    case _ => super.<=(lessPrecise)
  }
  abstract override def >=(morePrecise: V) = morePrecise match {
    case Box(target) => this <= target
    case _ => super.>=(morePrecise)
  }
}

abstract class Box[V <: Val[V]] extends BoxedVal[V] {
  val dom: BoxDomain[V]

  var target: V = _
  val num = Box.nextBoxNum
  def isBox = true

  def isBottom = target.isBottom
  def isTop = target.isTop

  def ||(v: V) = if (this eq v) v else dom.makeMJoin(this, v)
  def &&(v: V) = if (this eq v) v else dom.makeMMeet(this, v)

  def <=(lessPrecise: V) = this == lessPrecise || target <= lessPrecise
  def >=(morePrecise: V) = this == morePrecise || target >= morePrecise

  def matchCons(cons: Cons) = target.matchCons(cons)

  var syncToString = false
  override def toString =
    if (syncToString)
      s"Box->#$num"
    else {
      syncToString = true
      val res = s"Box#$num($target)"
      syncToString = false
      res
    }

  var syncHashCode = false
  override def hashCode =
    if (syncHashCode)
      104033
    else {
      syncHashCode = true
      val res = target.hashCode
      syncHashCode = false
      res
    }

  var syncEquals = Set[(Int, Int)]()
  override def equals(a: Any) = a match {
    case bv: Box[_] =>
      if (this.num == bv.num)
        true
      else if (syncEquals.contains((this.num, bv.num)))
        true
      else {
        syncEquals += this.num -> bv.num
        syncEquals += bv.num -> this.num
        this.target == bv.target
      }
    case _ => {
      val h = -Math.abs(a.hashCode + 1)
      if (syncEquals.contains((this.num, h)))
        true
      else {
        syncEquals += this.num -> h
        syncEquals += h -> this.num
        this.target == a
      }
    }
  }
}
object Box {
  def unapply[V <: Val[V]](v: Val[V]): Option[V] =
    if (v.isInstanceOf[Box[_]])
      Some(v.asInstanceOf[Box[V]].target)
    else
      None

  private var count = 0
  def nextBoxNum = synchronized {
    val num = count
    count += 1
    num
  }
}

abstract class MMeet[V <: Val[V]](val b: MutableVal[V], val ref: V) extends MutableVal[V] {
  val dom: BoxDomain[V]
  print("")

  def current = b.current && ref

  def isBottom = b.isBottom || ref.isBottom
  def isTop = b.isTop && ref.isTop

  def ||(v: V): V = dom.makeMMeet(dom.makeMJoin(b, v), ref || v) // (b || v) && (ref || v)
  def &&(v: V): V = dom.makeMMeet(b, ref && v)

  def <=(v: V) = current <= v
  def >=(v: V) = current >= v

  def matchCons(cons: Cons) = b.matchCons(cons) intersect ref.matchCons(cons)

  override def toString = s"MMeet($b, $ref)"
}
object MMeet {
  def unapply[V <: Val[V]](v: Val[V]): Option[(MutableVal[V],V)] = v match {
    case m: MMeet[V] => Some((m.b, m.ref))
    case _ => None
  }
}

abstract class MJoin[V <: Val[V]](val b: MutableVal[V], val ref: V) extends MutableVal[V] {
  val dom: BoxDomain[V]
  print("")

  def current = b.current || ref

  def isBottom = b.isBottom || ref.isBottom
  def isTop = b.isTop && ref.isTop

  def ||(v: V): V = dom.makeMJoin(b, ref || v)
  def &&(v: V): V = v match {
    case MJoin(b2, ref2) if b eq b2 => dom.makeMJoin(b, ref && ref2)
    case MMeet(b2, ref2) if b eq b2 => dom.makeMJoin(dom.makeMMeet(b, ref2), dom.makeMMeet(b, ref && ref2))
    case _ => dom.makeMJoin(dom.makeMMeet(b, v), ref && v)
  }

  def <=(v: V) = current <= v
  def >=(v: V) = current >= v

  def matchCons(cons: Cons) = {
    val bs = b.matchCons(cons)
    val rs = ref.matchCons(cons)
    bs ++ rs
  }

  override def toString = s"MJoin($b, $ref)"
}
object MJoin {
  def unapply[V <: Val[V]](v: Val[V]): Option[(MutableVal[V],V)] = v match {
    case m: MJoin[V] => Some((m.b, m.ref))
    case _ => None
  }
}

//abstract class MMatch[V <: Val[V]](val b: MutableVal[V], val cons: Cons, val index: Int) extends MutableVal[V] {
//  val dom: BoxDomain[V]
//  print("")
//
//  def actual = b.matchCons
//
//  def isBottom = b.matchCons(cons)
//  def isTop = b.isTop && ref.isTop
//
//  def ||(v: V) = dom.makeMJoin(b, ref || v)
//  def &&(v: V) = v match {
//    case MJoin(b2, ref2) if b eq b2 => dom.makeMJoin(b, ref && ref2)
//    case MMeet(b2, ref2) if b eq b2 => dom.makeMJoin(dom.makeMMeet(b, ref2), dom.makeMMeet(b, ref && ref2))
//    case _ => dom.makeMJoin(dom.makeMMeet(b, v), ref && v)
//  }
//
//  def <=(v: V) = (b || ref) <= v
//  def >=(v: V) = (b || ref) >= v
//
//  def matchCons(cons: Cons) = {
//    val bs = b.matchCons(cons)
//    val rs = ref.matchCons(cons)
//    bs ++ rs
//  }
//
//  override def toString = s"MJoin($b, $ref)"
//}
//object MMatch {
//  def unapply[V <: Val[V]](v: Val[V]): Option[(MutableVal[V],V)] = v match {
//    case m: MJoin[V] => Some((m.b, m.ref))
//    case _ => None
//  }
//}

trait BoxDomain[V <: Val[V]] extends Domain[V] {
  def makeBox(v: V): V with BoxedVal[V]
  def makeMMeet(b: MutableVal[V], ref: V): V with MutableVal[V]
  def makeMJoin(b: MutableVal[V], ref: V): V with MutableVal[V]
}
