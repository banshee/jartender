package com.restphone.jartender

import scala.PartialFunction.condOpt

object TestUtilities {
  private val extractFilePathExpr = """file:/(.*)""".r
  def getResource( s: String ) = {
    val resource = Thread.currentThread.getContextClassLoader.getResource( s )
    val result = condOpt(resource.toString) { case extractFilePathExpr( f ) => f }
    result.getOrElse(throw new RuntimeException("cannot find resource " + s))
  }
}