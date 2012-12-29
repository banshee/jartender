package com.restphone.jartender

trait ProvidesLogging {
  def logMsg(msg: String)
  def logError(msg: String)
}
