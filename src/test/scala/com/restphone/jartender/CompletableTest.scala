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

import com.restphone.scalatestutilities.HasSuccessValue._

class CompletableTest extends FunSuite with ShouldMatchers {
  import Completable._

  implicit val stringLengthGreaterThan0 = ( x: String ) => condOpt( x ) {
    case s: String if s.length > 0 => s
  }

  test( "can find the first complete item" ) {
    def x1: Validation[Int, String] = { 1.failure }
    def x2 = { "".success }
    def x3 = "done".success

    val result = x1 completeOrElse x2 completeOrElse x3

    result.successValue should be( "done" )
  }

  test( "can find the second complete item" ) {
    def x1: Validation[Int, String] = 1.failure
    def x2 = "x2".success
    def x3 = "x3".success

    val result = x1 completeOrElse x2 completeOrElse x3

    result.successValue should be( "x2" )
  }

  test( "returns the last error if everything fails" ) {
    def x1: Validation[Int, String] = 1.failure
    def x2 = 2.failure
    def x3 = 3.failure

    val result = x1 completeOrElse x2 completeOrElse x3

    result.failureValue should be (3)
  }

  test( "should not evaluate the second term" ) {
    var shouldStayTrue = true

    def x1: Validation[Int, String] = "success".success
    def x2: Validation[Int, String] = { shouldStayTrue = false; "".success }

    val result = x1 completeOrElse x2

    result.successValue should be( "success" )
    shouldStayTrue should be( true )
  }
}
