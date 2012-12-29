package com.restphone.jartender

import java.io.File

import com.restphone.jartender.FileFailureValidation.FileFailureValidation

trait Shrinker {
  def jartenderCacheParameters: JartenderCacheParameters
  def execute(): FileFailureValidation[File]
}