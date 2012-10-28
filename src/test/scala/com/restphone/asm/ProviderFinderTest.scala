package com.restphone.asm

import com.restphone.asm._
import org.scalatest.FunSuite
import com.google.common.collect._
import org.junit.runner.RunWith
import org.scalatest.junit._

class Fnord {
  def someMethod = 0
}
object Fnord {
  val objectValue = 0
  def objectMethod = 0
}
trait FooTrait {
  def fnord
}

@RunWith( classOf[ JUnitRunner ] )
class ProviderFinderTest extends FunSuite {
//  test( "can parse class by name" ) {
//    val fnord = "com/restphone/asm/Fnord"
//    expectResult( Some( true ) ) {
//      for {
//        r <- ProviderFinder.buildItemsFromClassName( fnord )
//      } yield {
////        assert( r.contains( ProvidesClass( fnord ) ) )
//        assert (5 === r.size)
//        true
//      }
//    }
//  }
  test( "can parse an interface" ) {
    val name = "com/restphone/asm/FooTrait"
    expectResult( Some( true ) ) {
      for {
        r <- ProviderFinder.buildItemsFromClassName( name )
      } yield {
//        assert( r.contains( ProvidesClass( fnord ) ) )
        println(r);
        true
      }
    }
  }

  test( "can parse an annotation" ) {
    val name = "com/restphone/asmSample/SimpleAnnotation"
    expectResult( Some( true ) ) {
      for {
        r <- ProviderFinder.buildItemsFromClassName( name )
      } yield {
//        assert( r.contains( ProvidesClass( fnord ) ) )
        println("here is annoation")
        println(r);
        true
      }
    }
  }
}
