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
  case class FileFailure( context: String, errorMessage: String )
  type FileFailureValidation[T] = ValidationNEL[FileFailure, T]

  def convertIoExceptionToValidation( context: String ) = {
    val a = catching( classOf[IOException] ) withApply { x => ( FileFailure( context, x.getLocalizedMessage ) ).failNel }
    val b = catching( classOf[RuntimeException] ) withApply { x => ( FileFailure( context, x.getLocalizedMessage ) ).failNel }
    a or b
  }

  def validDirectory( f: File, failureMsg: String ): FileFailureValidation[File] =
    if ( f.isDirectory ) f.success else FileFailure( failureMsg, f.getPath + " is not a valid directory" ).failureNel

  def validatedFile( f: File, failureMsg: String ): FileFailureValidation[File] =
    if ( f.isFile && f.canRead && f.length > 0 ) f.success else FileFailure( failureMsg, f.getPath + " is not readable with length > 0" ).failureNel

  def validatedTempFile( validationContext: String, prefix: String, suffix: String, directory: File ): FileFailureValidation[File] = {
    for {
      dir <- validDirectory( directory, validationContext )
      tmpfile <- convertIoExceptionToValidation( validationContext ) { File.createTempFile( prefix, suffix, dir ).success }
    } yield tmpfile
  }
}
