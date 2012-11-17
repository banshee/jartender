package com.restphone.jartender

sealed abstract class DependencyElement

case class DependsOnClass(klass: ProvidesClass) extends DependencyElement