package org.sugarj.sweettooth.stratego.analysis.v4

import org.sugarj.sweettooth.stratego.Syntax.Cons
import org.sugarj.sweettooth.stratego.analysis.domain.{Val, Domain}

trait MutableVal[V <: Val[V]] extends Val[V] {
  val dom: BoxDomain[V]
  // returns non-MutableVal value
  def current: V

  def matchCons(cons: Cons): List[V] = dom.makeMMatches(this, cons)
}

trait BoxedVal[V <: Val[V]] extends MutableVal[V] {
  var target: V
  def isStable: Boolean
  def markStable(): Unit
}

trait NoBox[V <: Val[V]] extends Val[V] {
  val dom: BoxDomain[V]

  abstract override def ||(v: V) = v match {
    case b: MutableVal[V] => b || this.asInstanceOf[V]
    case _ => super.||(v)
  }
  abstract override def &&(v: V) = v match {
    case b: MutableVal[V] => b && this.asInstanceOf[V]
    case _ => super.&&(v)
  }

  abstract override def <=(lessPrecise: V) = lessPrecise match {
    case b: MutableVal[V] => b >= this.cast
    case _ => super.<=(lessPrecise)
  }
  abstract override def >=(morePrecise: V) = morePrecise match {
    case b: MutableVal[V] => b <= this.cast
    case _ => super.>=(morePrecise)
  }

  override def equals(a: Any) = a match {
    case b: MutableVal[V] => b equals this
    case _ => super.equals(a)
  }
}

abstract class Box[V <: Val[V]] extends BoxedVal[V] {
  val dom: BoxDomain[V]

  private var stable: Boolean = false
  def isStable = stable
  def markStable() {
    stable = true
  }


  private var _target: V = _
  def target = _target
  def target_=(v: V) {
    if (stable)
      throw new IllegalStateException(s"Cannot mutate stable value $this")
    _target = v
  }

  def current = target match {
    case x: MutableVal[V] => x.current
    case _ => target
  }

  val num = Box.nextBoxNum
  def isBox = true

  def isBottom = target.isBottom
  def isTop = target.isTop

  var syncJoin = false
  def ||(v: V) = dom.makeMJoin(this, v)

  var syncMeet = false
  def &&(v: V) = dom.makeMMeet(this, v)

//  def <=(lessPrecise: V) = this == lessPrecise || target <= lessPrecise
  var syncLT: Option[V] = None
  def <=(lessPrecise: V) = (this eq lessPrecise) || (syncLT match {
    case None =>
      syncLT = Some(lessPrecise)
      val res = target <= lessPrecise
      syncLT = None
      res
    case Some(v) =>
      if (lessPrecise eq v)
        true
      else
        lessPrecise <= v
  })

  var syncGT: Option[V] = None
  def >=(morePrecise: V) = (this eq morePrecise) || (syncGT match {
    case None =>
      syncGT = Some(morePrecise)
      val res = target >= morePrecise
      syncGT = None
      res
    case Some(v) =>
      if (morePrecise eq v)
        true
      else
        morePrecise >= v
  })
  
  var syncToString = false
  override def toString = {
    val stab = if (stable) "" else "!"
    if (syncToString) {
      s"Box$stab->#$num"
    }
    else {
      syncToString = true
      val res = s"Box$stab#$num($target)"
      syncToString = false
      res
    }
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

  var syncEquals: Set[(Int,Int)] = Set()
  override def equals(a: Any) = a match {
    case x: AnyRef if this eq x => true
    case a: Val[V] => this <= a.cast && this >= a.cast
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

trait MMeet[V <: Val[V]] extends MutableVal[V] {
  val dom: BoxDomain[V]

  val b: MutableVal[V]
  val ref: V

  def current = b.current && ref

  def isBottom = b.isBottom || ref.isBottom
  def isTop = b.isTop && ref.isTop

  def ||(v: V): V = dom.makeMJoin(this, v)
  def &&(v: V): V = dom.makeMMeet(this, v)

  def <=(v: V) = b <= v || ref <= v
  def >=(v: V) = b >= v && ref >= v

  override def toString = s"MMeet($b, $ref)"
  override def hashCode = b.hashCode * 173 + ref.hashCode
  override def equals(a: Any) = a match {
    case x: AnyRef if this eq x => true
    case x if x.isInstanceOf[V] =>
      val v = x.asInstanceOf[V]
      this <= v && v <= this.asInstanceOf[V]
    case _ => false
  }

}
object MMeet {
  def unapply[V <: Val[V]](v: Val[V]): Option[(MutableVal[V],V)] = v match {
    case m: MMeet[V] => Some((m.b, m.ref))
    case _ => None
  }
}

trait MJoin[V <: Val[V]] extends MutableVal[V] {
  val dom: BoxDomain[V]

  val b: MutableVal[V]
  val ref: V

  def current = b.current || ref

  def isBottom = b.isBottom || ref.isBottom
  def isTop = b.isTop && ref.isTop

  def ||(v: V): V = dom.makeMJoin(this, v)
  def &&(v: V): V = dom.makeMMeet(this, v)

  def <=(v: V) = b <= v && ref <= v
  def >=(v: V) = b >= v || ref >= v

  override def toString = s"MJoin($b, $ref)"
  override def hashCode = b.hashCode * 173 + ref.hashCode
  override def equals(a: Any) = a match {
    case x: AnyRef if this eq x => true
    case x if x.isInstanceOf[V] =>
      val v = x.asInstanceOf[V]
      this <= v && v <= this.asInstanceOf[V]
    case _ => false
  }
}
object MJoin {
  def unapply[V <: Val[V]](v: Val[V]): Option[(MutableVal[V],V)] = v match {
    case m: MJoin[V] => Some((m.b, m.ref))
    case _ => None
  }
}

trait MMatch[V <: Val[V]] extends MutableVal[V] {
  val dom: BoxDomain[V]

  val b: MutableVal[V]
  val cons: Cons
  val index: Int

  def current = b.current.matchCons(cons)(index)

  def isBottom = current.isBottom
  def isTop = current.isTop

  def ||(v: V): V = if (this eq v) v else dom.makeMJoin(this, v)
  def &&(v: V): V = if (this eq v) v else dom.makeMMeet(this, v)

  def <=(v: V) = current <= v
  def >=(v: V) = current >= v

  override def toString = s"MMatch($b, $cons/$index)"
  override def hashCode = b.hashCode * 173 + cons.hashCode * 13 + index
  override def equals(a: Any) = a match {
    case x: AnyRef if this eq x => true
    case x if x.isInstanceOf[V] =>
      val v = x.asInstanceOf[V]
      this <= v && v <= this.asInstanceOf[V]
    case _ => false
  }
}
object MMatch {
  def unapply[V <: Val[V]](v: Val[V]): Option[(MutableVal[V],Cons,Int)] = v match {
    case m: MMatch[V] => Some((m.b, m.cons, m.index))
    case _ => None
  }
}

trait BoxDomain[V <: Val[V]] extends Domain[V] {
  def makeBox(v: V): V with BoxedVal[V]
  def _makeMMeet(b: MutableVal[V], ref: V): V with MutableVal[V]
  def makeMMeet(b: MutableVal[V], ref: V): V = (b,ref) match {
    case (_,_) if b eq ref => b.cast
    case (bot,_) if bot.isBottom => this.bottom
    case (_,bot) if bot.isBottom => this.bottom
    case (MMeet(b1, ref1), _) => (b1 && ref) && ref1
    case (_, MMeet(b2, ref2)) => (b && b2.cast) && ref2
    case (MJoin(b1, ref1), _) => (b1 && ref) || (ref1 && ref)
    case (_, MJoin(b2, ref2)) => (b && b2.cast) || (b && ref2)
    case (_,_) if b <= ref => b.cast
    case (_,_) if b >= ref => ref
    case (_,_) => _makeMMeet(b, ref)
  }
  def _makeMJoin(b: MutableVal[V], ref: V): V with MutableVal[V]
  def makeMJoin(b: MutableVal[V], ref: V): V = (b,ref) match {
    case (_,_) if b eq ref => b.cast
    case (bot,_) if bot.isBottom => b.cast
    case (_,bot) if bot.isBottom => b.cast
    case (MMeet(b1, ref1), _) => _makeMJoin(b, ref)
    case (_, MMeet(b2, ref2)) => _makeMJoin(b, ref)
    case (MJoin(b1, ref1), _) => (b1 || ref) || ref1
    case (_, MJoin(b2, ref2)) => (b || b2.cast) || ref2
    case (_,_) if !ref.isTop && b <= ref => ref
    case (_,_) if !b.isTop && b >= ref => b.cast
    case (_,_) => _makeMJoin(b, ref)
  }
  def makeMMatch(b: MutableVal[V], cons: Cons, index: Int): V with MutableVal[V]

  def makeMMatches(b: MutableVal[V], cons: Cons): List[V with MutableVal[V]] =
    (0 until cons.ar).toList map (makeMMatch(b, cons, _))
}
