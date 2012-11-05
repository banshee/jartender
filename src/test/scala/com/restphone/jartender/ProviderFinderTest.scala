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
trait SampleTrait extends SampleTraitII {
  @AnnotationI(a = "i", b = new AnnotationII)
  def aMethod
}

trait SampleTraitII

@RunWith(classOf[JUnitRunner])
class ProviderFinderTest extends FunSuite with ShouldMatchers {
  def buildSampleTrait = {
    val name = "com/restphone/jartender/SampleTrait"
    val xs = ProviderFinder.buildItemsFromClassName(name)
    xs should be('defined)
    listToStreamOfLists(xs.get)
  }

  test("can analyze a trait (and traits are interfaces)") {
    val sublists = buildSampleTrait
    val providesClassWithNameSampleTrait = ProvidesClass.createProvidesClassMatcher(_.name == "com/restphone/jartender/SampleTrait")
    val result =
      sublists collectFirst { case providesClassWithNameSampleTrait :: (_: UsesAnnotation) :: (_: UsesAnnotation) :: t => true }
    result should be(some(true))
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

  test("can analyze nested annotations on a field") {
    val sublists = buildJartenderSample
    val result =
      sublists collectFirst { case ProvidesField(_, "aStaticStringFieldWithAnnotation", _, _, _) :: (_: UsesAnnotation) :: (_: UsesAnnotation) :: t => true }
    result should be(some(true))
  }

  test("can analyze nested annotations on a method") {
    val sublists = buildJartenderSample
    val result =
      sublists collectFirst { case ProvidesMethod(_, "testClassMethod", _, _, _) :: (_: UsesAnnotation) :: (_: UsesAnnotation) :: t => true }
    result should be(some(true))
  }

  test("can analyze nested annotations on a method parameter") {
    val sublists = buildJartenderSample
    val result =
      sublists collectFirst { case ProvidesMethod(_, "testClassMethod", _, _, _) :: _ :: _ :: (_: UsesParameterAnnotation) :: (_: UsesParameterAnnotation) :: t => true }
    result should be(some(true))
  }

  test("can analyze nested annotations on a local variable") {
    // These never end up in bytecode
  }

  test("can analyze nested annotations on a class") {
    val sublists = buildJartenderSample
    val result =
      sublists collectFirst { case ProvidesClass(_, _, "com/restphone/jartender/JartenderSample", _, _, _) :: (_: UsesAnnotation) :: (_: UsesAnnotation) :: t => true }
    result should be(some(true))
  }

  test("can analyze a subclass") {
    val name = "com/restphone/jartender/JartenderSample$JartenderSampleSubclass"
    val xs = ProviderFinder.buildItemsFromClassName(name)
    xs should be('defined)

    val methodInSubclass = xs.get collectFirst {case ProvidesMethod(_, "methodInSubclass", _, _, _) => true}
    methodInSubclass should be (some(true))
  }

  def showResult(name: String, elements: List[Provider]) = {
    val s = elements.mkString("\n")
    val q = ""
    println(f"Results for $name are\n$s")
  }
}
