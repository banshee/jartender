package com.restphone.jartender

sealed abstract class ClassfileElement {
  def usesClasses: Set[UsesClass]
}
sealed abstract class ProvidesElement extends ClassfileElement {
  def matchAgainst: UsesElement
}
sealed abstract class UsesElement extends ClassfileElement {
    def usesClasses: Set[UsesClass]
}

/**
 * @param interfaces Internal names
 * @param signature Information for generics
 *
 * Note that interfaces are classes with access bits of ACC_INTERFACE and ACC_ABSTRACT set (0x400, 0x200)
 */
case class ProvidesClass(
  version: Int,
  access: Int,
  internalName: InternalName,
  signature: Option[Signature],
  superName: InternalName,
  interfaces: List[InternalName]) extends ProvidesElement {
  import scalaz._
  import Scalaz._
  val javaIdentifier = internalName.javaIdentifier;
  val internalNames = internalName :: interfaces
  def usesClasses = DependencyClassVisitor.convert_identifiers_to_UsesClasses(internalName :: interfaces)
  def matchAgainst = UsesClass(javaIdentifier)
}
object ProvidesClass {
  def createProvidesClassMatcher(fn: ProvidesClass => Boolean) = new Object {
    def unapply(f: ProvidesClass) = fn(f)
  }
}
case class ProvidesField(
  klassname: InternalName,
  access: Int,
  name: InternalName,
  typeDescriptor: TypeDescriptor,
  signature: Option[Signature],
  value: Option[Object]) extends ProvidesElement with UsesClassesIsBuiltFromTypeDescriptor {
  val fullyQualifiedJavaIdentifier = klassname.javaIdentifier.s + "." + name.javaIdentifier.s;
  def key = fullyQualifiedJavaIdentifier
  def matchAgainst = UsesField(klassname, name.s, typeDescriptor)
}
object ProvidesField {
  def createProvidesFieldMatcher(fn: ProvidesField => Boolean) = new Object {
    def unapply(f: ProvidesField) = fn(f)
  }
}
case class ProvidesMethod(
  klassname: InternalName,
  access: Int,
  name: JavaIdentifier,
  desc: MethodDescriptor,
  signature: Option[Signature],
  exceptions: List[InternalName]) extends ProvidesElement {
  def usesClasses = DependencyClassVisitor.convert_identifiers_to_UsesClasses(desc :: exceptions)
  val key = f"$klassname $name $desc"
  def matchAgainst = UsesMethod(klassname, name, desc)
}
case class UsesClass(javaIdentifier: JavaIdentifier) extends UsesElement {
  def usesClasses = Set(this)
}

case class UsesAnnotation(typeDescriptor: TypeDescriptor, visibleAtRuntime: Option[Boolean]) extends UsesElement with UsesClassesIsBuiltFromTypeDescriptor
case class UsesAnnotationArray(name: String) extends UsesElement with UsesClassesIsTheEmptySet
case class UsesAnnotationEnum(name: Option[String], typeDescriptor: TypeDescriptor, value: String) extends UsesElement with UsesClassesIsBuiltFromTypeDescriptor
case class UsesParameterAnnotation(typeDescriptor: TypeDescriptor) extends UsesElement with UsesClassesIsBuiltFromTypeDescriptor
case class UsesMethod(owner: InternalName, name: JavaIdentifier, desc: MethodDescriptor) extends UsesElement {
  def usesClasses = DependencyClassVisitor.convert_identifiers_to_UsesClasses(List(desc, owner))
}
case class UsesField(owner: InternalName, name: String, desc: TypeDescriptor) extends UsesElement {
  def usesClasses = DependencyClassVisitor.convert_identifiers_to_UsesClasses(List(desc, owner))
  def fullyQualifiedJavaIdentifier = owner.javaIdentifier.s + "." + name;
}
case class UsesException(exceptionType: InternalName) extends UsesElement {
  def usesClasses = exceptionType.usesClasses
}

trait UsesClassesIsBuiltFromTypeDescriptor {
  def typeDescriptor: TypeDescriptor
  def usesClasses = typeDescriptor.usesClasses
}
trait UsesClassesIsTheEmptySet {
  def usesClasses = Set.empty[UsesClass]
}

class ProviderCollection(items: Iterable[ProvidesElement]) {
}