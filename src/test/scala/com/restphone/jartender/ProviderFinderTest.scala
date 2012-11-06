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
    val providesClassWithNameSampleTrait = ProvidesClass.createProvidesClassMatcher(_.internalName == "com/restphone/jartender/SampleTrait")
    val result =
      sublists collectFirst { case providesClassWithNameSampleTrait :: (_: UsesAnnotation) :: (_: UsesAnnotation) :: t => true }
    result should be(some(true))
  }

  def listToStreamOfLists[T](lst: List[T]): Stream[List[T]] = lst match {
    case h :: t => lst #:: listToStreamOfLists(t)
    case Nil => Stream.empty
  }

  def buildJartenderBase = {
    val name = "com/restphone/jartender/JartenderSample"
    val xs = ProviderFinder.buildItemsFromClassName(name)
    xs should be('defined)
    showResult(name, xs.get)
    xs.get
  }
  
  def buildJartenderSample = {
    listToStreamOfLists(buildJartenderBase)
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
      sublists collectFirst { case ProvidesMethod(_, "testClassMethod", _, _, _) :: _ :: _ :: (_: UsesParameterAnnotation) :: (_: UsesAnnotation) :: t => true }
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
  
  test("can extract classes from ProvidesClass") {
    val pc = ProvidesClass(49,33,"com/restphone/jartender/JartenderSample", null, "java/lang/Object", List("com/restphone/jartender/InterfaceI"))
    val result = ProviderFinder.extractClasses(pc)
    val expected = Set("com.restphone.jartender.JartenderSample", "com.restphone.jartender.InterfaceI") map UsesClass
    result should be (expected)
  }

  test("can extract classes from ProvidesField") {
    val pf = ProvidesField(9, "aStaticStringFieldWithAnnotation", "Ljava/lang/String;", null, null)
    val result = ProviderFinder.extractClasses(pf)
    val expected = Set("java.lang.String") map UsesClass
    result should be (expected)
  }

  test("can extract classes from ProvidesMethod") {
    val pm = ProvidesMethod(1, "aGenericMethod", "(Ljava/lang/Object;)Ljava/lang/String;", Some("<T:Ljava/lang/Object;>(TT;)Ljava/lang/String;"), List("java/lang/RuntimeException"))
    val result = ProviderFinder.extractClasses(pm)
    val expected = Set("java.lang.String", "java.lang.Object", "java.lang.RuntimeException") map UsesClass
    result should be (expected)
  }

  test("can extract classes from UsesAnnotation") {
    val ua = UsesAnnotation("Lcom/restphone/jartender/AnnotationI;", Some(false))
    val result = ProviderFinder.extractClasses(ua)
    val expected = Set("com.restphone.jartender.AnnotationI") map UsesClass
    result should be (expected)
  }

  test("can extract classes from UsesAnnotationEnum") {
    val uae = UsesAnnotationEnum(null, "Lcom/restphone/jartender/AnnotationEnum;", "SAMPLEVALUE1")
    val result = ProviderFinder.extractClasses(uae)
    val expected = Set("com.restphone.jartender.AnnotationEnum") map UsesClass
    result should be (expected)
    
  }
}
