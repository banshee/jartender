package com.restphone.jartender

import java.io.File

import com.restphone.jartender.FileFailureValidation.FileFailureValidation

trait Shrinker {
  def execute( conf: JartenderCacheParameters ): FileFailureValidation[File]
}