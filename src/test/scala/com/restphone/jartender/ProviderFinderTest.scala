package com.restphone.jartender

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import scalaz.Lens
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import scalaz._
import Scalaz._
import org.scalatest.matchers.ShouldMatchers

@AnnotationI(a = "i", b = new AnnotationII)
trait SampleTrait {
  @AnnotationI(a = "i", b = new AnnotationII)
  def aMethod
}

@RunWith(classOf[JUnitRunner])
class ProviderFinderTest extends FunSuite with ShouldMatchers {
  def buildSampleTrait = {
    val name = "com/restphone/jartender/SampleTrait"
    val xs = ProviderFinder.buildItemsFromClassName(name)
    xs should be('defined)
    showResult(name, xs.get)
    listToStreamOfLists(xs.get)
  }

  test("can parse a trait (and traits are interfaces)") {
    val sublists = buildSampleTrait
    val result =
      sublists collectFirst { case ProvidesField(_, "aStaticStringFieldWithAnnotation", _, _, _) :: (_: UsesAnnotation) :: (_: UsesAnnotation) :: t => true }
    result should be(some(true))
  }

  test("can parse an annotation") {
    val name = "com/restphone/jartender/AnnotationI"
    expectResult(Some(true)) {
      for {
        r <- ProviderFinder.buildItemsFromClassName(name)
      } yield {
        true
      }
    }
  }

  def listToStreamOfLists[T](lst: List[T]): Stream[List[T]] = lst match {
    case h :: t => lst #:: listToStreamOfLists(t)
    case Nil => Stream.empty
  }

  def buildJartenderSample = {
    val name = "com/restphone/jartender/JartenderSample"
    val xs = ProviderFinder.buildItemsFromClassName(name)
    xs should be('defined)
    listToStreamOfLists(xs.get)
  }

  test("can parse nested annotations on a field") {
    val sublists = buildJartenderSample
    val result =
      sublists collectFirst { case ProvidesField(_, "aStaticStringFieldWithAnnotation", _, _, _) :: (_: UsesAnnotation) :: (_: UsesAnnotation) :: t => true }
    result should be(some(true))
  }

  test("can parse nested annotations on a method") {
    val sublists = buildJartenderSample
    val result =
      sublists collectFirst { case ProvidesMethod(_, "testClassMethod", _, _, _) :: (_: UsesAnnotation) :: (_: UsesAnnotation) :: t => true }
    result should be(some(true))
  }

  test("can parse nested annotations on a method parameter") {
    val sublists = buildJartenderSample
    val result =
      sublists collectFirst { case ProvidesMethod(_, "testClassMethod", _, _, _) :: _ :: _ :: (_: UsesParameterAnnotation) :: (_: UsesParameterAnnotation) :: t => true }
    result should be(some(true))
  }

  test("can parse nested annotations on a local variable") {
    // These never end up in bytecode
  }

  test("can parse nested annotations on a class") {
    val sublists = buildJartenderSample
    val result =
      sublists collectFirst { case ProvidesClass(_, _, "com/restphone/jartender/JartenderSample", _, _, _) :: (_: UsesAnnotation) :: (_: UsesAnnotation) :: t => true }
    result should be(some(true))
  }

  test("can parse a subclass") {
    val name = "com/restphone/jartender/JartenderSample$JartenderSampleSubclass"
    expectResult(Some(true)) {
      for {
        r <- ProviderFinder.buildItemsFromClassName(name)
      } yield {
        true
      }
    }
  }

  test("can build a lens") {
    import scalaz._
    import Scalaz._
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
