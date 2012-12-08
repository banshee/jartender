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
  def convertIoExceptionToValidation( file: File ) = catching( classOf[IOException] ) withApply { x => ( FileFailure( file, x.getLocalizedMessage ) ).failNel }
  case class FileFailure( file: File, errorMessage: String )
  type FileFailureValidation[T] = ValidationNEL[FileFailure, T]
}
