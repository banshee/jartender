package com.restphone.jartender

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import scala.annotation.tailrec
import scala.collection._
import scala.collection.mutable.SynchronizedMap
import scala.collection.mutable
import scala.util.Random.shuffle
import scalaz._
import Scalaz._

class ClassRelationshipsTest extends FunSuite with ShouldMatchers {
  val animalClasses = {
    val a1 = DependencyAnalyser.buildItemsFromClassName( JavaIdentifier( "com.restphone.jartender.Animal" ).internalName )
    val a2 = DependencyAnalyser.buildItemsFromClassName( JavaIdentifier( "com.restphone.jartender.Dog" ).internalName )
    val a3 = DependencyAnalyser.buildItemsFromClassName( JavaIdentifier( "com.restphone.jartender.CarbonBasedLifeform" ).internalName )
    val result = ( a1 |@| a2 |@| a3 ) { ( a, b, c ) => a.elements ++ b.elements ++ c.elements }
    result.toOption.get
  }

  test( "can find the parent classes for a class" ) {
    val c = UsesClass( InternalName( "com/restphone/jartender/Dog" ).javaIdentifier )
    val pc = ClassRelationships.findProvidesClass( c, animalClasses )
    pc should be( 'defined )
    val ancestors = ClassRelationships.ancestors( pc.get, animalClasses collect { case a: ProvidesClass => a } )
    val expected = List( "com.restphone.jartender.Animal", "java.lang.Object" ) map JavaIdentifier
    ancestors map { _.javaIdentifier.s } should be( List( "com.restphone.jartender.Animal", "com.restphone.jartender.CarbonBasedLifeform" ) )
  }

  test( "can find a ProvidesClass given a java identifier" ) {
    val c = UsesClass( InternalName( "com/restphone/jartender/Dog" ).javaIdentifier )
    val pc = ClassRelationships.findProvidesClass( c, animalClasses )
    pc should be( Some( ProvidesClass( 50, 33, InternalName( "com/restphone/jartender/Dog" ), None, InternalName( "com/restphone/jartender/Animal" ), List() ) ) )
    pc.get.parents should be( List( JavaIdentifier( "com.restphone.jartender.Animal" ) ) )
  }
}

// Sample traits and classes for these tests
trait CarbonBasedLifeform {
  def talk: String
}
class Animal extends CarbonBasedLifeform {
  def talk = "<crickets>"
}
class Dog extends Animal {
  override def talk = "woof"
}
