package com.restphone.jartender

import java.io.File

import com.restphone.jartender.FileFailureValidation._

trait Shrinker {
  def jartenderCacheParameters: JartenderCacheParameters
  def execute(): FailureValidation[File]
}