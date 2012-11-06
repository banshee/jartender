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

  test("can extract all the methods from a class file") {
    val tst = buildJartenderSample.head
    val methods = tst collect { case x : UsesMethod => x } 
    methods.toSet should be ('snark)
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
    val expected = Set("com.restphone.jartender.JartenderSample", "com.restphone.jartender.InterfaceI") map UsesClass
    pc.usesClasses should be (expected)
  }

  test("can extract classes from ProvidesField") {
    val pf = ProvidesField(9, "aStaticStringFieldWithAnnotation", "Ljava/lang/String;", null, null)
    val expected = Set("java.lang.String") map UsesClass
    pf.usesClasses should be (expected)
  }

  test("can extract classes from ProvidesMethod") {
    val pm = ProvidesMethod(1, "aGenericMethod", "(Ljava/lang/Object;)Ljava/lang/String;", Some("<T:Ljava/lang/Object;>(TT;)Ljava/lang/String;"), List("java/lang/RuntimeException"))
    val expected = Set("java.lang.String", "java.lang.Object", "java.lang.RuntimeException") map UsesClass
    pm.usesClasses should be (expected)
  }

  test("can extract classes from UsesAnnotation") {
    val ua = UsesAnnotation("Lcom/restphone/jartender/AnnotationI;", Some(false))
    val expected = Set("com.restphone.jartender.AnnotationI") map UsesClass
    ua.usesClasses should be (expected)
  }

  test("can extract classes from UsesAnnotationEnum") {
    val uae = UsesAnnotationEnum(null, "Lcom/restphone/jartender/AnnotationEnum;", "SAMPLEVALUE1")
    val expected = Set("com.restphone.jartender.AnnotationEnum") map UsesClass
    uae.usesClasses should be (expected)
  }

  test("can extract classes from UsesParameterAnnotation") {
    val pa = UsesParameterAnnotation("Lcom/restphone/jartender/AnnotationI;")
    val expected = Set("com.restphone.jartender.AnnotationI") map UsesClass
    pa.usesClasses should be (expected)
  }

  test("can extract classes from UsesMethod") {
    val um = UsesMethod(182, "com/restphone/jartender/JartenderSampleII", "aGenericMethod", "(Ljava/lang/Object;)Ljava/lang/String;")
    val expected = Set("com.restphone.jartender.JartenderSampleII", "java.lang.Object", "java.lang.String") map UsesClass
    um.usesClasses should be (expected)
  }

  test("can extract classes from UsesField") {
    val um = UsesField(178, "java/lang/System", "out", "Ljava/io/PrintStream;")
    val expected = Set("java.lang.System", "java.io.PrintStream") map UsesClass
    um.usesClasses should be (expected)
  }

  test("can extract classes from UsesException") {
    val um = UsesException("java/lang/RuntimeException")
    val expected = Set("java.lang.RuntimeException") map UsesClass
    um.usesClasses should be (expected)
  }
}
