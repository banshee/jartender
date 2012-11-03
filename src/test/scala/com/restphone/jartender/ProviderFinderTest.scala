package com.restphone.jartender

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import scalaz._
import scalaz.Lens
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import Scalaz._
import scalaz._

trait SampleTrait {
  def aMethod
}

@RunWith(classOf[JUnitRunner])
class ProviderFinderTest extends FunSuite {
  test("can parse an interface") {
    val name = "com/restphone/jartender/SampleTrait"
    expectResult(Some(true)) {
      for {
        r <- ProviderFinder.buildItemsFromClassName(name)
      } yield {
        showResult(name, r)
        true
      }
    }
  }

  test("can parse an annotation") {
    val name = "com/restphone/jartender/AnnotationI"
    expectResult(Some(true)) {
      for {
        r <- ProviderFinder.buildItemsFromClassName(name)
      } yield {
        showResult(name, r)
        true
      }
    }
  }

  def listToStreamOfLists[T](lst: List[T]): Stream[List[T]] = lst match {
    case h :: t => lst #:: listToStreamOfLists(t)
    case Nil => Stream.empty
  }

  test("can parse a class with a subclass") {
    val name = "com/restphone/jartender/JartenderSample"
    expectResult(Some(true)) {
      for {
        r <- ProviderFinder.buildItemsFromClassName(name)
      } yield {
        showResult(JavaStringHolder.jartenderSample, r)
        listToStreamOfLists(r) collectFirst { case (_: UsesAnnotation) :: (_: UsesAnnotation) :: t => true } isDefined
      }
    }
  }

  test("can parse a subclass") {
    val name = "com/restphone/jartender/JartenderSample$JartenderSampleSubclass"
    expectResult(Some(true)) {
      for {
        r <- ProviderFinder.buildItemsFromClassName(name)
      } yield {
        showResult(JavaStringHolder.jartenderSample, r)
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
