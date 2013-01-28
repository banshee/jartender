package com.restphone.jartender

import java.io.File
import java.io.IOException

import scala.util.control.Exception.catching
import scala.util.control.Exception.throwableSubtypeToCatcher

import scalaz._
import scalaz.Scalaz._
import scalaz.ValidationNEL
import scala.language.implicitConversions

object FileFailureValidation {
  trait ExtraFailureInformation
  implicit def javaFileToExtraFailureInformation( javaFile: File ) = some( new ExtraFailureInformation { val f = javaFile } )

  sealed abstract class AbstractFailure {
    def context: String
  }
  case class FailureWithoutException( context: String, msg: Option[String] = none, extra: Option[ExtraFailureInformation] = none ) extends AbstractFailure
  case class FailureWithException( context: String, originalException: Throwable, extra: Option[ExtraFailureInformation] ) extends AbstractFailure

  type FailureValidation[T] = ValidationNEL[AbstractFailure, T]

  def convertExceptionToValidation( exceptions: List[Class[_]] )( context: String, extra: Option[ExtraFailureInformation] ) =
    catching( exceptions: _* ) withApply { exception =>
      new FailureWithException( context, exception, extra ).failureNel
    }

  def convertExceptions( context: String, extra: Option[ExtraFailureInformation] = none ) =
    convertExceptionToValidation( stdExceptions )( context, extra )

  implicit val stdExceptions: List[Class[_]] = List( classOf[IOException], classOf[RuntimeException] )

  def validDirectory( f: File, failureMsg: String ): FailureValidation[File] =
    if ( f.isDirectory ) f.success else FailureWithoutException( failureMsg, some( f.getPath + " is not a valid directory" ), f ).failureNel

  def validatedFile( f: File, failureMsg: String ): FailureValidation[File] = for {
    newFile <- ( new File( f.getPath ) ).success
    _ <- validatedIsFile( newFile, failureMsg )
    _ <- ( validatedCanReadFile( newFile, failureMsg ) |@| validatedLengthNonZero( newFile, failureMsg ) ) { ( _, _ ) => newFile }
  } yield f

  def validatedIsFile( f: File, failureContext: String ) = fileValidation( f, f.isFile, failureContext, "is not a file" )

  def validatedCanReadFile( f: File, failureContext: String ) = fileValidation( f, f.canRead, failureContext, "is not readable" )

  def validatedLengthNonZero( f: File, failureContext: String ) = fileValidation( f, f.length > 0, failureContext, "is empty" )

  def validatedTempFile( validationContext: String, prefix: String, suffix: String, directory: File ): FailureValidation[File] = {
    for {
      dir <- validDirectory( directory, validationContext )
      tmpfile <- convertExceptions( validationContext, none ) { File.createTempFile( prefix, suffix, dir ).success }
    } yield tmpfile
  }

  def fileValidation( f: File, validation: => Boolean, failureContext: String, failureMessage: String ) =
    validation match {
      case true => f.success
      case false => FailureWithoutException( failureContext, some( failureMessage + ": " + f.getPath ), f ).failureNel
    }
}
