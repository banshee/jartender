package com.restphone.jartender

import java.io.FileInputStream
import java.util.jar.JarFile

import scala.Option.option2Iterable
import scala.actors.Futures.future
import scala.collection.JavaConverters.enumerationAsScalaIteratorConverter

import org.objectweb.asm.ClassReader

import scalaz._
import Scalaz._

object DependencyAnalyser {
  def buildItems(cr: ClassReader)(pf: DependencyClassVisitor) = {
    cr.accept(pf, 0)
    pf.getProvidedElements
  }

  def buildItemsFromClassName(klass: InternalName, pf: DependencyClassVisitor = DependencyClassVisitor()): Option[List[ClassfileElement]] = {
    Some(buildItems(new ClassReader(klass.s))(pf).toList)
  }

  def buildItemsFromClassFile(filename: String, pf: DependencyClassVisitor = DependencyClassVisitor()): Option[List[ClassfileElement]] = {
    for {
      fis <- Option(new FileInputStream(filename))
      cr <- Option(new ClassReader(fis))
    } yield {
      buildItems(cr)(pf).toList
    }
  }

  def buildItemsFromJarfile(j: JarFile) = {
    import scala.collection.JavaConverters._
    import scala.actors.Futures._
    for {
      entry <- j.entries().asScala if (entry.getName().toLowerCase().endsWith(".class"))
      inputStream <- some(j.getInputStream(entry))
      cr = new ClassReader(inputStream)
      result = future {
        buildItems(cr)(DependencyClassVisitor())
      }
    } yield result
  }

  def buildMatchingDependencies(providers: Set[ProvidesElement], users: Set[UsesElement]): Set[UsesElement] = {
    val classesProvidedTransformedToTheirEquivalentUsesElement = providers map { _.matchAgainst }
    val classesUsed = users flatMap {_.usesClasses}
    println("cx: " + classesProvidedTransformedToTheirEquivalentUsesElement)
    users & classesProvidedTransformedToTheirEquivalentUsesElement
  }
}
