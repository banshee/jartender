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

  def validatedFile( f: File, failureMsg: String ): FileFailureValidation[File] = for {
    _ <- validatedIsFile( f, failureMsg )
    _ <- ( validatedCanReadFile( f, failureMsg ) |@| validatedLengthNonZero( f, failureMsg ) ) { ( _, _ ) => f }
  } yield f

  def validatedIsFile( f: File, failureContext: String ) = fileValidation( f, f.isFile, failureContext, "is not a file" )

  def validatedCanReadFile( f: File, failureContext: String ) = fileValidation( f, f.canRead, failureContext, "is not readable" )

  def validatedLengthNonZero( f: File, failureContext: String ) = fileValidation( f, f.length <= 0, failureContext, "is empty" )

  def validatedTempFile( validationContext: String, prefix: String, suffix: String, directory: File ): FileFailureValidation[File] = {
    for {
      dir <- validDirectory( directory, validationContext )
      tmpfile <- convertIoExceptionToValidation( validationContext ) { File.createTempFile( prefix, suffix, dir ).success }
    } yield tmpfile
  }

  def fileValidation( f: File, validation: => Boolean, failureContext: String, failureMessage: String = "" ) =
    validation match {
      case true => f.success
      case false => FileFailure( failureContext, failureMessage + " " + f.getPath ).failureNel
    }
}
