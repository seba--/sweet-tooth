package org.sugarj.sweettooth.stratego.analysis.domain

object d2_PowersetFlagDomain {
   type T = (Set[Trm],Boolean) // None represents the infinite set, Some represents finite sets

   abstract class Trm
   case class Lit[V](v: V) extends Trm
   case class App(cons: Symbol, xs: List[T]) extends Trm {
     override def toString = cons.name + "(" + listString(xs) + ")"

     def listString(xs: List[T]): String = xs match {
       case Nil => ""
       case x::Nil => x.toString
       case x::xs => x.toString + ", " + listString(xs)
     }
   }

   def App(cons: Symbol): App = App(cons, List())
   def App(cons: Symbol, x: T, xs: T*): App = App(cons, x::List(xs:_*))


   object D extends Domain[T] {
     def bottom: T = (Set(), false)
     def top: T = (Set(), true)

     def compare(morePrecise: T, lessPrecise: T): Boolean = (morePrecise, lessPrecise) match {
       case (_,(_,true)) => true
       case ((_,true),_) => false
       case ((s1,_), (s2,_)) =>
         def findInS2(t1: Trm): Boolean = t1 match {
           case t1: Lit[_] => s2.contains(t1)
           case App(cons1, args1) =>
             s2 map {
               case App(`cons1`, args2) if args1.size == args2.size =>
                 if (args1.zip(args2).forall{case (a1, a2) => compare(a1, a2)})
                   return true
             }
             false
         }

         s1.forall(findInS2(_))
     }

     def join(t1: T, t2: T): T = (mergeUnion(t1._1, t2._1), t1._2 || t2._2)


     def mergeUnion(s1: Set[Trm], s2: Set[Trm]) = {
       var lits = Set[Trm]()
       var apps = Map[Symbol, List[T]]()

       def it(s: Set[Trm]) = s.map(t => t match {
         case l: Lit[_] => lits += l
         case App(cons, xs) =>
           apps.get(cons) match {
             case None => apps += cons -> xs
             case Some(ys) =>
               val args = ys zip xs
               val joined = args.map(p => join(p._1,p._2))
               apps += cons -> joined
           }
       })

       it(s1)
       it(s2)
       lits ++ apps.map(p => App(p._1,p._2))
     }

     def meet(t1: T, t2: T): T = (mergeIntersect(t1._1, t2._1), t1._2 && t2._2)

     def mergeIntersect(s1: Set[Trm], s2: Set[Trm]) = {
       type M = collection.mutable.Map[Symbol, List[T]]

       var lits = Set[Trm]()
       def makeJoinedMap(s: Set[Trm], m: M) = s.map(t => t match {
         case l: Lit[_] => lits += l
         case App(cons, xs) =>
           m.get(cons) match {
             case None => m += cons -> xs
             case Some(ys) =>
               val args = ys zip xs
               val joined = args.map(p => join(p._1,p._2))
               m += cons -> joined
           }
       })

       var apps = Map[Symbol, List[T]]()
       def makedMetMap(m1: M, m2: M) = m2.map(t => t match {
         case (cons, xs) =>
           m1.get(cons) match {
             case None => // no meet
             case Some(ys) =>
               val args = ys zip xs
               val met = args.map(p => meet(p._1,p._2))
               if (met.isEmpty || !met.forall(_ == bottom))
                 apps += cons -> met
           }
       })

       var apps1 = collection.mutable.Map[Symbol, List[T]]()
       makeJoinedMap(s1, apps1)

       lits = Set()
       var apps2 = collection.mutable.Map[Symbol, List[T]]()
       makeJoinedMap(s2, apps2)

       makedMetMap(apps1, apps2)
       lits ++ apps.map(p => App(p._1,p._2))
     }

     def matchAppPat(cons: Symbol, arity: Int, t: T): Set[List[T]] = {
       val args = for (App(`cons`, xs) <- t._1 if xs.size == arity) yield xs
       if (t._2) {
         val topArgList = for (i <- (1 to arity).toList) yield top
         args + topArgList
       }
       else
         args
     }


     def liftLit[V](v: V) = (Set(Lit(v)), false)
     def liftApp(cons: Symbol, xs: List[T]) = (Set(App(cons, xs)), false)
     def liftApp(cons: Symbol, xs: T*) = (Set(App(cons, List(xs:_*))), false)
   }
 }