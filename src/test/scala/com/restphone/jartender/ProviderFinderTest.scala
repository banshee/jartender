package com.restphone.jartender

import org.scalatest.FunSuite
import scalaz.Lens
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import scalaz._
import Scalaz._
import org.scalatest.matchers.ShouldMatchers
import java.net.URLClassLoader

trait SampleTraitII
@AnnotationI(a = "i", b = new AnnotationII)
trait SampleTrait extends SampleTraitII {
  @AnnotationI(a = "i", b = new AnnotationII)
  def aMethod
}

class ProviderFinderTest extends FunSuite with ShouldMatchers {
  def printclasspath = {
    val loader =  ClassLoader.getSystemClassLoader.asInstanceOf[URLClassLoader]
    val urls = loader.getURLs
    urls foreach {u => println{u.getFile}}
  }
  
  def buildSampleTrait = {
    val name = InternalName("com/restphone/jartender/SampleTrait")
    println(f"trait is " + classOf[com.restphone.jartender.SampleTrait])
    println("classpath is")
    printclasspath
    val xs = ProviderFinder.buildItemsFromClassName(name)
    xs should be('defined)
    listToStreamOfLists(xs.get)
  }

  test("can analyze a trait (and traits are interfaces)") {
    val sublists = buildSampleTrait
    val providesClassWithNameSampleTrait = ProvidesClass.createProvidesClassMatcher(_.internalName.s == "com/restphone/jartender/SampleTrait")
    val result =
      sublists collectFirst { case providesClassWithNameSampleTrait :: (_: UsesAnnotation) :: (_: UsesAnnotation) :: t => true }
    result should be(some(true))
  }

  def listToStreamOfLists[T](lst: List[T]): Stream[List[T]] = lst match {
    case h :: t => lst #:: listToStreamOfLists(t)
    case Nil => Stream.empty
  }

  def buildJartenderBase = {
    val name = InternalName("com/restphone/jartender/JartenderSample")
    val xs = ProviderFinder.buildItemsFromClassName(name)
    xs should be('defined)
    showResult(name.s, xs.get)
    xs.get
  }

  def buildJartenderSample = {
    listToStreamOfLists(buildJartenderBase)
  }

  test("can analyze nested annotations on a field") {
    val sublists = buildJartenderSample
    val result =
      sublists collectFirst { case ProvidesField(_, InternalName("aStaticStringFieldWithAnnotation"), _, _, _) :: (_: UsesAnnotation) :: (_: UsesAnnotation) :: t => true }
    result should be(some(true))
  }

  test("can analyze nested annotations on a method") {
    val sublists = buildJartenderSample
    val result =
      sublists collectFirst { case ProvidesMethod(_, JavaIdentifier("testClassMethod"), _, _, _) :: (_: UsesAnnotation) :: (_: UsesAnnotation) :: t => true }
    result should be(some(true))
  }

  test("can analyze nested annotations on a method parameter") {
    val sublists = buildJartenderSample
    val result =
      sublists collectFirst { case ProvidesMethod(_, JavaIdentifier("testClassMethod"), _, _, _) :: _ :: _ :: (_: UsesParameterAnnotation) :: (_: UsesAnnotation) :: t => true }
    result should be(some(true))
  }

  test("can analyze nested annotations on a local variable") {
    // These never end up in bytecode
  }

  test("can analyze nested annotations on a class") {
    val sublists = buildJartenderSample
    val result =
      sublists collectFirst { case ProvidesClass(_, _, InternalName("com/restphone/jartender/JartenderSample"), _, _, _) :: (_: UsesAnnotation) :: (_: UsesAnnotation) :: t => true }
    result should be(some(true))
  }

  test("can analyze a subclass") {
    val name = InternalName("com/restphone/jartender/JartenderSample$JartenderSampleSubclass")
    val xs = ProviderFinder.buildItemsFromClassName(name)
    xs should be('defined)

    val methodInSubclass = xs.get collectFirst { case ProvidesMethod(_, JavaIdentifier("methodInSubclass"), _, _, _) => true }
    methodInSubclass should be(some(true))
  }

  def showResult(name: String, elements: List[ClassfileElement]) = {
    val s = elements.mkString("\n")
    val q = ""
    println(f"Results for $name are\n$s")
  }

  test("can extract classes from ProvidesClass") {
    val pc = ProvidesClass(49, 33, InternalName("com/restphone/jartender/JartenderSample"), null, InternalName("java/lang/Object"), List(InternalName("com/restphone/jartender/InterfaceI")))
    val expected = Set("com.restphone.jartender.JartenderSample", "com.restphone.jartender.InterfaceI") map {JavaIdentifier(_)} map UsesClass
    pc.usesClasses should be(expected)
  }

  test("can extract classes from ProvidesField") {
    val pf = ProvidesField(9, InternalName("aStaticStringFieldWithAnnotation"), TypeDescriptor("Ljava/lang/String;"), null, null)
    val expected = Set("java.lang.String") map {JavaIdentifier(_)} map UsesClass
    pf.usesClasses should be(expected)
  }

  test("can extract classes from ProvidesMethod") {
    val pm = ProvidesMethod(1, JavaIdentifier("aGenericMethod"), MethodDescriptor("(Ljava/lang/Object;)Ljava/lang/String;"), Some(Signature("<T:Ljava/lang/Object;>(TT;)Ljava/lang/String;")), List(InternalName("java/lang/RuntimeException")))
    val expected = Set("java.lang.String", "java.lang.Object", "java.lang.RuntimeException") map {JavaIdentifier(_)} map UsesClass
    pm.usesClasses should be(expected)
  }

  test("can extract classes from UsesAnnotation") {
    val ua = UsesAnnotation(TypeDescriptor("Lcom/restphone/jartender/AnnotationI;"), some(false))
    val expected = Set("com.restphone.jartender.AnnotationI") map {JavaIdentifier(_)} map UsesClass
    ua.usesClasses should be(expected)
  }

  test("can extract classes from UsesAnnotationEnum") {
    val uae = UsesAnnotationEnum(null, TypeDescriptor("Lcom/restphone/jartender/AnnotationEnum;"), "SAMPLEVALUE1")
    val expected = Set("com.restphone.jartender.AnnotationEnum") map {JavaIdentifier(_)} map UsesClass
    uae.usesClasses should be(expected)
  }

  test("can extract classes from UsesParameterAnnotation") {
    val pa = UsesParameterAnnotation(TypeDescriptor("Lcom/restphone/jartender/AnnotationI;"))
    val expected = Set("com.restphone.jartender.AnnotationI") map {JavaIdentifier(_)} map UsesClass
    pa.usesClasses should be(expected)
  }

  test("can extract classes from UsesMethod") {
    val um = UsesMethod(182, InternalName("com/restphone/jartender/JartenderSampleII"), JavaIdentifier("aGenericMethod"), MethodDescriptor("(Ljava/lang/Object;)Ljava/lang/String;"))
    val expected = Set("com.restphone.jartender.JartenderSampleII", "java.lang.Object", "java.lang.String") map {JavaIdentifier(_)} map UsesClass
    um.usesClasses should be(expected)
  }

  test("can extract classes from UsesField") {
    val um = UsesField(178, InternalName("java/lang/System"), "out", TypeDescriptor("Ljava/io/PrintStream;"))
    val expected = Set("java.lang.System", "java.io.PrintStream") map {JavaIdentifier(_)} map UsesClass
    um.usesClasses should be(expected)
  }

  test("can extract classes from UsesException") {
    val um = UsesException(InternalName("java/lang/RuntimeException"))
    val expected = Set("java.lang.RuntimeException") map {JavaIdentifier(_)} map UsesClass
    um.usesClasses should be(expected)
  }

  test("can build map of ProvidesClass => ProvidesMethod and ProvidesField") {
    val pc = ProvidesClass(49, 33, InternalName("com/restphone/jartender/JartenderSample"), null, InternalName("java/lang/Object"), List(InternalName("com/restphone/jartender/InterfaceI")))
    val pm = ProvidesMethod(1, JavaIdentifier("aGenericMethod"), MethodDescriptor("(Ljava/lang/Object;)Ljava/lang/String;"), Some(Signature("<T:Ljava/lang/Object;>(TT;)Ljava/lang/String;")), List(InternalName("java/lang/RuntimeException")))
    val pf = ProvidesField(9, InternalName("aStaticStringFieldWithAnnotation"), TypeDescriptor("Ljava/lang/String;"), null, null)
    val result = ProviderFinder.buildProvidedItems(List(pc, pf, pm))
    result.targetClass should be (pc.javaIdentifier)
    result.provides should be (Set(pm, pf))
  }
}

