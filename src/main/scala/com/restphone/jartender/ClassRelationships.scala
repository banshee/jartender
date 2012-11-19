package com.restphone.jartender

import scalaz._
import Scalaz._
import scala.annotation.tailrec

object ClassRelationships {
  def findProvidesClass(c: UsesClass, xs: Iterable[ClassfileElement]): Option[ProvidesClass] = xs collectFirst { case a: ProvidesClass if a.classMatches(c) => a }
  def findProvidesClass(c: JavaIdentifier, xs: Iterable[ClassfileElement]): Option[ProvidesClass] = findProvidesClass(UsesClass(c), xs)

  def ancestors(c: ProvidesClass, xs: Iterable[ProvidesClass]): List[ProvidesClass] = {
    c.parents flatMap {findProvidesClass(_, xs)} flatMap {x => x :: ancestors(x, xs)}
  }
  
  def children(c: ProvidesClass, xs: Iterable[ProvidesClass]) = xs filter { x => ancestors(x, xs) contains c }
  
  def overridesOfMethod(m: UsesMethod, xs: Iterable[ProvidesMethod]) = {
    
  }
  
}
//
//  List(ProvidesClass(50,33,InternalName(com/restphone/jartender/Animal),None,InternalName(java/lang/Object),List(InternalName(com/restphone/jartender/CarbonBasedLifeform))), ProvidesClass(50,1537,InternalName(com/restphone/jartender/CarbonBasedLifeform),None,InternalName(java/lang/Object),List())) was not equal to Some(()) (ClassRelationshipsTest.scala:39)
