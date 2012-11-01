package com.restphone.jartender

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit._
import com.restphone.asm._
import com.restphone.asmSample._
import scalaz.Lens

class FnordClass {
  class Subfnord {
  }
  @SimpleAnnotation(a = "foo", b = new SecondAnnotation) def thisHasANestedAnnotation = 0
  def thisIsAMethod(f: FnordClass) = {
    println("something" + f.another)
    "done"
  }
  def another = "antoher"
}
trait FooTrait {
  def fnord
}

@RunWith(classOf[JUnitRunner])
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
  test("can parse an interface") {
    val name = "com/restphone/asm/FooTrait"
    expectResult(Some(true)) {
      for {
        r <- ProviderFinder.buildItemsFromClassName(name)
      } yield {
        //        assert( r.contains( ProvidesClass( fnord ) ) )
        println(name + "****\n\n")
        println(r.mkString("\n"));
        println(name + "----\n\n")
        true
      }
    }
  }

  test("can parse an annotation") {
    val name = "com/restphone/asmSample/SimpleAnnotation"
    expectResult(Some(true)) {
      for {
        r <- ProviderFinder.buildItemsFromClassName(name)
      } yield {
        //        assert( r.contains( ProvidesClass( fnord ) ) )
        println(r);
        true
      }
    }
  }

  test("can parse a class with a subclass") {
    val name = "com/restphone/asm/FnordClass"
    expectResult(Some(true)) {
      for {
        r <- ProviderFinder.buildItemsFromClassName(name)
      } yield {
        //        assert( r.contains( ProvidesClass( fnord ) ) )
        showResult(name, r)
        true
      }
    }
  }

  test("can build a lens") {
    import scalaz._
    import Scalaz._
    println("fnord2")
    case class TestMe(xs: List[Int], y: String)
    var foo = List(1, 2)
    val testMeListIntLens: Lens[TestMe, List[Int]] = Lens.lensu(
      (tm, lst) => tm.copy(xs = lst),
      tm => tm.xs)
    val listLens: Lens[List[Int], Int] = Lens.lensu(
      (lst, i) => i :: lst.tail,
      _.head)
    val testMeStringLens: Lens[TestMe, String] = Lens.lensu(
      (tm, s) => tm.copy(y = s),
      _.y)
    var listUnderTest = List(1, 2)
    val testMe = TestMe(listUnderTest, "stringOne")
  }
  
  def showResult(name: String, elements: List[Provider]) = {
    val s = elements.mkString("\n")
    val q = ""
    println(f"Results for $name are\n$s")
  }
}
