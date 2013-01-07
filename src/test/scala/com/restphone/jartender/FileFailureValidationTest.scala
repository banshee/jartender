package com.restphone.jartender

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import com.restphone.scalatestutilities.ScalaTestMatchers.beOfType
import com.google.common.io.Files
import java.io.File
import com.google.common.base.Charsets
import scala.util.control.Exception._
import java.io.IOException
import java.io.ObjectStreamException
import scalaz._
import Scalaz._
import scala.PartialFunction._

import com.restphone.jartender.FileFailureValidation._
import com.restphone.scalatestutilities.HasSuccessValue._

class FileFailureValidationTest extends FunSuite with ShouldMatchers {
  test( "should be able to use convertExceptionToValidation" ) {
    val safeCast = convertExceptionToValidation( classOf[ClassCastException] :: stdExceptions )_
    val result: FailureValidation[String] = safeCast("testing", none) { throw new ClassCastException }
    result should be ('failure)
  }
}
