package com.restphone.jartender

import org.scalatest.FunSuite
import scalaz.Lens
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import scalaz._
import Scalaz._
import org.scalatest.matchers.ShouldMatchers
import java.net.URLClassLoader
import java.util.jar.JarFile

trait SampleTraitII
@AnnotationI(a = "i", b = new AnnotationII)
trait SampleTrait extends SampleTraitII {
  @AnnotationI(a = "i", b = new AnnotationII)
  def aMethod
}

class ProviderFinderTest extends FunSuite with ShouldMatchers {
  def printclasspath = {
    val loader = ClassLoader.getSystemClassLoader.asInstanceOf[URLClassLoader]
    val urls = loader.getURLs
    urls foreach { u => println { u.getFile } }
  }

  def buildSampleTrait = {
    val name = InternalName("com/restphone/jartender/SampleTrait")
    val xs = DependencyAnalyser.buildItemsFromClassName(name)
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

//  test("bigtest") {
//    //    val a = DependencyAnalyser.buildItemsFromJarfile(new JarFile("/users/james/.ivy2/cache/org.scalaz/scalaz-core_2.10.0-SNAPSHOT/bundles/scalaz-core_2.10.0-SNAPSHOT-7.0-SNAPSHOT.jar"))
//    val a = DependencyAnalyser.buildItemsFromJarfile(new JarFile("/tmp/scala-library.jar"))
//    a.toList
//  }

  def buildJartenderBase = {
    val name = InternalName("com/restphone/jartender/JartenderSample")
    val xs = DependencyAnalyser.buildItemsFromClassName(name)
    xs should be('defined)
    xs.get
  }

  def buildJartenderSample = {
    listToStreamOfLists(buildJartenderBase)
  }

  test("can analyze nested annotations on a field") {
    val sublists = buildJartenderSample
    val result =
      sublists collectFirst { case ProvidesField(_, _, InternalName("aStaticStringFieldWithAnnotation"), _, _, _) :: (_: UsesAnnotation) :: (_: UsesAnnotation) :: t => true }
    result should be(some(true))
  }

  test("can analyze nested annotations on a method") {
    val sublists = buildJartenderSample
    val result =
      sublists collectFirst { case ProvidesMethod(_, _, JavaIdentifier("testClassMethod"), _, _, _) :: (_: UsesAnnotation) :: (_: UsesAnnotation) :: t => true }
    result should be(some(true))
  }

  test("can analyze nested annotations on a method parameter") {
    val sublists = buildJartenderSample
    val result =
      sublists collectFirst { case ProvidesMethod(_, _, JavaIdentifier("testClassMethod"), _, _, _) :: _ :: _ :: (_: UsesParameterAnnotation) :: (_: UsesAnnotation) :: t => true }
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
    val xs = DependencyAnalyser.buildItemsFromClassName(name)
    xs should be('defined)

    val methodInSubclass = xs.get collectFirst { case ProvidesMethod(_, _, JavaIdentifier("methodInSubclass"), _, _, _) => true }
    methodInSubclass should be(some(true))
  }

  def showResult(name: String, elements: List[ClassfileElement]) = {
    val s = elements.mkString("\n")
    val q = ""
    println(f"Results for $name are\n$s")
  }

  test("can extract classes from ProvidesClass") {
    val pc = ProvidesClass(49, 33, InternalName("com/restphone/jartender/JartenderSample"), null, InternalName("java/lang/Object"), List(InternalName("com/restphone/jartender/InterfaceI")))
    val expected = Set("com.restphone.jartender.JartenderSample", "com.restphone.jartender.InterfaceI", "java.lang.Object") map JavaIdentifier map UsesClass
    pc.usesClasses should be(expected)
  }

  test("can extract classes from ProvidesField") {
    val pf = ProvidesField(InternalName("none"), 9, InternalName("aStaticStringFieldWithAnnotation"), TypeDescriptor("Ljava/lang/String;"), null, null)
    val expected = Set("java.lang.String") map JavaIdentifier map UsesClass
    pf.usesClasses should be(expected)
  }

  test("can extract classes from ProvidesMethod") {
    val pm = ProvidesMethod(InternalName("none"), 1, JavaIdentifier("aGenericMethod"), MethodDescriptor("(Ljava/lang/Object;)Ljava/lang/String;"), Some(Signature("<T:Ljava/lang/Object;>(TT;)Ljava/lang/String;")), List(InternalName("java/lang/RuntimeException")))
    val expected = Set("java.lang.String", "java.lang.Object", "java.lang.RuntimeException") map JavaIdentifier map UsesClass
    pm.usesClasses should be(expected)
  }

  test("can extract classes from UsesAnnotation") {
    val ua = UsesAnnotation(TypeDescriptor("Lcom/restphone/jartender/AnnotationI;"), some(false))
    val expected = Set("com.restphone.jartender.AnnotationI") map JavaIdentifier map UsesClass
    ua.usesClasses should be(expected)
  }

  test("can extract classes from UsesAnnotationEnum") {
    val uae = UsesAnnotationEnum(null, TypeDescriptor("Lcom/restphone/jartender/AnnotationEnum;"), "SAMPLEVALUE1")
    val expected = Set("com.restphone.jartender.AnnotationEnum") map JavaIdentifier map UsesClass
    uae.usesClasses should be(expected)
  }

  test("can extract classes from UsesParameterAnnotation") {
    val pa = UsesParameterAnnotation(TypeDescriptor("Lcom/restphone/jartender/AnnotationI;"))
    val expected = Set("com.restphone.jartender.AnnotationI") map JavaIdentifier map UsesClass
    pa.usesClasses should be(expected)
  }

  test("can extract classes from UsesMethod") {
    val um = UsesMethod(InternalName("com/restphone/jartender/JartenderSampleII"), JavaIdentifier("aGenericMethod"), MethodDescriptor("(Ljava/lang/Object;)Ljava/lang/String;"))
    val expected = Set("com.restphone.jartender.JartenderSampleII", "java.lang.Object", "java.lang.String") map JavaIdentifier map UsesClass
    um.usesClasses should be(expected)
  }

  test("can extract classes from UsesField") {
    val um = UsesField(InternalName("java/lang/System"), "out", TypeDescriptor("Ljava/io/PrintStream;"))
    val expected = Set("java.lang.System", "java.io.PrintStream") map JavaIdentifier map UsesClass
    um.usesClasses should be(expected)
  }

  test("can extract classes from UsesException") {
    val um = UsesException(InternalName("java/lang/RuntimeException"))
    val expected = Set("java.lang.RuntimeException") map JavaIdentifier map UsesClass
    um.usesClasses should be(expected)
  }

  test("can build map of ProvidesClass => ProvidesMethod and ProvidesField") {
    val pc = ProvidesClass(49, 33, InternalName("com/restphone/jartender/JartenderSample"), null, InternalName("java/lang/Object"), List(InternalName("com/restphone/jartender/InterfaceI")))
    val pm = ProvidesMethod(InternalName("none"), 1, JavaIdentifier("aGenericMethod"), MethodDescriptor("(Ljava/lang/Object;)Ljava/lang/String;"), Some(Signature("<T:Ljava/lang/Object;>(TT;)Ljava/lang/String;")), List(InternalName("java/lang/RuntimeException")))
    val pf = ProvidesField(InternalName("none"), 9, InternalName("aStaticStringFieldWithAnnotation"), TypeDescriptor("Ljava/lang/String;"), null, null)
    val result = DependencyClassVisitor.buildProvidedItems(List(pc, pf, pm))
    result.targetClass should be(pc.javaIdentifier)
    result.provides should be(Set(pm, pf))
  }
  
  test("can put together a list of relevant dependencies given a set of providers and a set of users") {
    val pc = ProvidesClass(49, 33, InternalName("com/restphone/jartender/JartenderSample"), null, InternalName("java/lang/Object"), List(InternalName("com/restphone/jartender/InterfaceI")))
    val pm = ProvidesMethod(InternalName("com/restphone/jartender/JartenderSample"), 1, JavaIdentifier("aGenericMethod"), MethodDescriptor("(Ljava/lang/Object;)Ljava/lang/String;"), Some(Signature("<T:Ljava/lang/Object;>(TT;)Ljava/lang/String;")), List(InternalName("java/lang/RuntimeException")))
    val pf = ProvidesField(InternalName("com/restphone/jartender/JartenderSample"), 9, InternalName("aStaticStringFieldWithAnnotation"), TypeDescriptor("Ljava/lang/String;"), null, null)

    val uf = UsesField(InternalName("com/restphone/jartender/JartenderSample"), "aStaticStringFieldWithAnnotation", TypeDescriptor("Ljava/lang/String;"))
    val um = UsesMethod(InternalName("com/restphone/jartender/JartenderSample"), JavaIdentifier("aGenericMethod"), MethodDescriptor("(Ljava/lang/Object;)Ljava/lang/String;"))

    val result = DependencyAnalyser.buildMatchingDependencies(Set(pc, pm, pf), DependencyClassVisitor.expand_UsesClasses(Set(uf, um)))
    result should be (Set(uf, um, pc.matchAgainst))
  }
}
