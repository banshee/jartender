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
import scala.util.control.Exception._
import java.io.IOException

object Utilities {
  def convertIoExceptionToValidation( filename: String ) = catching( classOf[IOException] ) withApply { x => ( f"IO Exception for file ${filename} : $x" ).failNel }
}