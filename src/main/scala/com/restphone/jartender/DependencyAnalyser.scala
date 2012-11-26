package com.restphone.jartender

import java.io.FileInputStream
import java.util.jar.JarFile
import scala.Option.option2Iterable
import scala.actors.Futures.future
import scala.collection.JavaConverters.enumerationAsScalaIteratorConverter
import org.objectweb.asm.ClassReader
import scalaz._
import Scalaz._
import java.io.File
import java.util.jar.JarEntry

object DependencyAnalyser {
  def buildItems( cr: ClassReader )( pf: DependencyClassVisitor ) = {
    cr.accept( pf, 0 )
    pf.getProvidedElements
  }

  sealed abstract class FileResult {
    def elements: List[ClassfileElement]
  }
  case class JarfileResult( jarfile: JarFile, entry: String, elements: List[ClassfileElement] ) extends FileResult
  case class ClassfileResult( classfile: File, elements: List[ClassfileElement] ) extends FileResult
  case class ClassnameResult( classname: InternalName, elements: List[ClassfileElement] ) extends FileResult

  def buildItemsFromClassName( klass: InternalName, pf: DependencyClassVisitor = DependencyClassVisitor() ): ClassnameResult = {
    ClassnameResult( klass, buildItems( new ClassReader( klass.s ) )( pf ).toList )
  }

  def buildItemsFromClassFile( filename: String, pf: DependencyClassVisitor = DependencyClassVisitor() ): ClassfileResult = {
    val result = for {
      fis <- Option( new FileInputStream( filename ) )
      cr <- Option( new ClassReader( fis ) )
    } yield {
      buildItems( cr )( pf ).toList
    }
    ClassfileResult( new File( filename ), result getOrElse List.empty )
  }

  def buildItemsFromJarfile( j: JarFile ): List[JarfileResult] = {
    import scala.collection.JavaConverters._
    import scala.actors.Futures._
    val futures = for {
      IsClassfileEntry( entry ) <- j.entries.asScala.toList
      inputStream <- Option( j.getInputStream( entry ) )
      cr = new ClassReader( inputStream )
      result = future {
        buildItems( cr )( DependencyClassVisitor() )
      }
    } yield ( entry.getName, result )
    futures map { case ( name, result ) => JarfileResult( j, name, result() ) }
  }

  def buildItemsFromFile( f: File ): List[FileResult] = {
    f match {
      case IsJarfile( x ) => buildItemsFromJarfile( new JarFile( x ) )
      case IsClassfile( x ) => List( buildItemsFromClassFile( x.toString ) )
    }
  }

  def unapplyForFileEndsWithSuffix( suffix: String ) = new Object {
    def unapply( f: File ) = if ( f.getName.endsWith( suffix ) ) some( f ) else none
  }
  val IsJarfile = unapplyForFileEndsWithSuffix( ".jar" )
  val IsClassfile = unapplyForFileEndsWithSuffix( ".class" )
  val IsClassfileEntry = new Object {
    def unapply( j: JarEntry ) = if ( j.getName.endsWith( ".class" ) ) some( j ) else none
  }

  /**
   * Given a set of providers and a set of users, return only the users that are provided
   * by those providers.
   *
   * For example, in AndroidProguardScala, we get a set of jars that need to be shrunk and some user code
   * that uses those jars.  We can figure out what those jars provide, and then we match that list against the list
   * of things that the user code calls.  If that list changes, we need to run proguard again, but if the list
   * stays the same, we can skip the proguard step.
   */
  def buildMatchingDependencies( providers: Set[ProvidesElement], users: Set[UsesElement] ): Set[UsesElement] = {
    val classesProvidedTransformedToTheirEquivalentUsesElement = providers map { _.matchAgainst }
    val classesUsed = users flatMap { _.usesClasses }
    users & classesProvidedTransformedToTheirEquivalentUsesElement
  }
}
