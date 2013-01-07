package com.restphone.jartender

import java.io.File
import java.io.FileInputStream
import java.util.jar.JarEntry
import java.util.jar.JarFile

import scala.collection.JavaConverters.enumerationAsScalaIteratorConverter

import org.objectweb.asm.ClassReader

import com.restphone.jartender.FileFailureValidation.FailureValidation
import com.restphone.jartender.FileFailureValidation.FailureWithoutException
import com.restphone.jartender.FileFailureValidation.convertExceptions
import com.restphone.jartender.FileFailureValidation.javaFileToExtraFailureInformation

import scalaz.Scalaz._

import scala.language.reflectiveCalls

object DependencyAnalyser {
  def buildItems( cr: ClassReader )( pf: DependencyClassVisitor ) = {
    cr.accept( pf, 0 )
    pf.getProvidedElements
  }

  sealed abstract class FileResult {
    def elements: List[ClassfileElement]
  }
  case class JarfileResult( jarfile: JarFile, jarfileElements: List[FailureValidation[JarfileElementResult]] ) extends FileResult {
    val elements = jarfileElements filter { _.isSuccess } flatMap { _.toOption.get.elements }
  }
  case class JarfileElementResult( jarfile: JarFile, entry: String, elements: List[ClassfileElement] ) extends FileResult
  case class ClassfileResult( classfile: File, elements: List[ClassfileElement] ) extends FileResult
  case class ClassnameResult( classname: InternalName, elements: List[ClassfileElement] ) extends FileResult

  def buildItemsFromClassName( klass: InternalName, pf: DependencyClassVisitor = new DependencyClassVisitor() ): FailureValidation[ClassnameResult] = {
    val name = klass.s + ".class"
    convertExceptions( name ) {
      val stream = Thread.currentThread.getContextClassLoader.getResourceAsStream( name )
      ClassnameResult( klass, buildItems( new ClassReader( stream ) )( pf ).toList ).success
    }
  }

  def buildItemsFromClassFile( filename: String, pf: DependencyClassVisitor = new DependencyClassVisitor ): FailureValidation[ClassfileResult] = {
    convertExceptions( filename ) {
      val fis = new FileInputStream( filename )
      ClassfileResult( new File( filename ), buildItems( new ClassReader( fis ) )( pf ).toList ).success
    }
  }

  private def buildJarEntry( jarfile: JarFile )( jarEntry: JarEntry ): FailureValidation[JarfileElementResult] = {
    convertExceptions( jarEntry.getName ) {
      val inputStream = jarfile.getInputStream( jarEntry )
      val cr = new ClassReader( inputStream )
      val items = buildItems( cr )( new DependencyClassVisitor )
      inputStream.close
      JarfileElementResult( jarfile, jarEntry.getName, items ).success
    }
  }

  def buildItemsFromJarfile( j: JarFile ): FailureValidation[JarfileResult] = {
    convertExceptions( j.getName ) {
      val jarentries = j.entries.asScala.toList collect { case IsClassfileEntry( entry ) => entry }
      val jarelements = ( jarentries.par map buildJarEntry( j ) ).seq.toList
      JarfileResult( j, jarelements ).success
    }
  }

  def buildItemsFromFile( f: File ): FailureValidation[FileResult] =
    convertExceptions( f.getName ) {
      f match {
        case IsJarfile( x ) => buildItemsFromJarfile( new JarFile( x ) )
        case IsClassfile( x ) => buildItemsFromClassFile( x.toString )
        case _ => FailureWithoutException( "buildItemsFromFile", none, f).failNel
      }
    }

  private def suffixMatches[T]( s: String, suffix: String, result: T ) = if ( s.toLowerCase.endsWith( suffix ) ) some( result ) else none
  private def unapplyForFileEndsWithSuffix( suffix: String ) = new Object {
    def unapply( f: File ) = suffixMatches( f.getName, suffix, f )
  }
  val IsJarfile = unapplyForFileEndsWithSuffix( ".jar" )
  val IsClassfile = unapplyForFileEndsWithSuffix( ".class" )
  val IsClassfileEntry = new Object {
    def unapply( j: JarEntry ) = suffixMatches( j.getName, ".class", j )
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
