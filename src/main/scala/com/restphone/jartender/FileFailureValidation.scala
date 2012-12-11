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

object FileFailureValidation {
  def convertIoExceptionToValidation( context: String ) = {
    val a = catching( classOf[IOException] ) withApply { x => ( FileFailure( context, x.getLocalizedMessage ) ).failNel }
    val b = catching( classOf[RuntimeException] ) withApply { x => ( FileFailure( context, x.getLocalizedMessage ) ).failNel }
    a or b
  }
  case class FileFailure( context: String, errorMessage: String )
  type FileFailureValidation[T] = ValidationNEL[FileFailure, T]
}
