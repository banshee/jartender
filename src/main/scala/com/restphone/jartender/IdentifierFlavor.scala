package com.restphone.jartender

import scalaz._ 
import Scalaz._

/**
 * IdentifierFlavor is the parent of method descriptors, type descriptors, java identifiers and signatures
 */
trait IdentifierFlavor extends Any {
  def usesClasses: Set[UsesClass]
}
case class InternalName(s: String) extends AnyVal with IdentifierFlavor {
  def javaIdentifier = JavaIdentifier(s.replace("/", "."))
  def usesClasses = javaIdentifier.usesClasses
}
case class MethodDescriptor(s: String) extends AnyVal with IdentifierFlavor {
  def usesClasses = DependencyClassVisitor.methodDescriptorToUsesClass(this)
}
case class TypeDescriptor(s: String) extends AnyVal with IdentifierFlavor {
  def usesClasses = DependencyClassVisitor.typeDescriptorToUsesClass(this)
}
case class JavaIdentifier(s: String) extends AnyVal with IdentifierFlavor {
  def usesClasses = Set(UsesClass(this))
  def internalName: InternalName = InternalName(s.replace(".", "/"))
}
case class Signature(s: String) extends AnyVal with IdentifierFlavor {
  def usesClasses = Set.empty
}
object OptionalSignature {
  def apply(s: String) = s match {
    case null => None
    case s => some(Signature(s))
  }
}
