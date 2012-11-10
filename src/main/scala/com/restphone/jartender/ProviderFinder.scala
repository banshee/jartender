package com.restphone.jartender

import java.io.FileInputStream
import org.objectweb.asm.ClassReader
import org.objectweb.asm.Opcodes
import org.objectweb.asm.FieldVisitor
import scalaz.Lens._
import scalaz._
import Scalaz._
import scala.collection.mutable.Stack
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Handle
import org.objectweb.asm.AnnotationVisitor
import org.objectweb.asm.Label

trait IdentifierFlavor extends Any

case class InternalName(s: String) extends {
  def javaIdentifier = JavaIdentifier(s.replace("/", "."))
}
case class MethodDescriptor(s: String) extends AnyVal with IdentifierFlavor {
  def usesClasses = ProviderFinder.methodDescriptorToUsesClass(this)
}
case class TypeDescriptor(s: String) extends AnyVal with IdentifierFlavor
case class JavaIdentifier(s: String) extends AnyVal with IdentifierFlavor
case class Signature(s: String) extends AnyVal with IdentifierFlavor

trait ProvidesInternalName {
  def internalName: InternalName
}
trait ProvidesJavaIdentifier {
  def javaIdentifer: JavaIdentifier
}
trait ProvidesTypeDescriptor {
  def typeDescriptor: TypeDescriptor
}
trait ProvidesJavaIdentifierFromInternalName extends ProvidesJavaIdentifier with ProvidesInternalName {
  def javaIdentifier = internalName.javaIdentifier
}
trait UsesClassFromJavaIdentifier extends ProvidesJavaIdentifier {
  def usesClasses = Set(JavaIdentifier)
}
trait UsesClassFromTypeDescriptor extends ProvidesTypeDescriptor {
    def usesClasses = ProviderFinder.typeDescriptorToUsesClass(typeDescriptor)
}

sealed abstract class Provider {
  def usesClasses: Set[UsesClass]
  def javaIdentifiersToUsesClass(xs: Iterable[JavaIdentifier]) = xs map {UsesClass(_)} toSet
}

/**
 * @param interfaces Internal names
 * @param signature Information for generics
 */
case class ProvidesClass(
  version: Int,
  access: Int,
  internalName: InternalName,
  signature: Option[Signature],
  superName: InternalName,
  interfaces: List[InternalName]) extends Provider with ProvidesJavaIdentifierFromInternalName {
  // Note that interfaces are classes with access bits of ACC_INTERFACE and ACC_ABSTRACT set (0x400, 0x200)
  //  def field(access: Int, name: String, desc: String, signature: String, value: Object, annotations: List[UsesClass]) = ProvidesField(access, name, desc, Option(signature), value)
  //  def method(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]) = ProvidesMethod(access, name, desc, Option(signature), exceptions.toList)
  val items = internalName :: interfaces
  def usesClasses = items map { i => UsesClass(i.javaIdentifier) } toSet
}
object ProvidesClass {
  def createProvidesClassMatcher(fn: ProvidesClass => Boolean) = new Object {
    def unapply(f: ProvidesClass) = fn(f)
  }
}
case class ProvidesField(access: Int, name: InternalName, desc: TypeDescriptor, signature: Option[Signature], value: Option[Object]) extends Provider {
  def usesClasses = ProviderFinder.typeDescriptorToUsesClass(desc)
}
object ProvidesField {
  def createProvidesFieldMatcher(fn: ProvidesField => Boolean) = new Object {
    def unapply(f: ProvidesField) = fn(f)
  }
}
case class ProvidesMethod(
  access: Int,
  name: JavaIdentifier,
  desc: MethodDescriptor,
  signature: Option[Signature],
  exceptions: List[InternalName]) extends Provider {
  def usesClasses = {
    val descriptorTypes = ProviderFinder.methodDescriptorToUsesClass(desc)
    val exceptionTypes = exceptions map { _.javaIdentifier } map UsesClass
    descriptorTypes ++ exceptionTypes
  }
}
case class UsesClass(javaIdentifier: JavaIdentifier) extends Provider with UsesClassFromJavaIdentifier

case class UsesAnnotation(typeDescriptor: TypeDescriptor, visibleAtRuntime: Option[Boolean]) extends Provider {
  def usesClasses = ProviderFinder.typeDescriptorToUsesClass(typeDescriptor)
}
case class UsesAnnotationArray(name: String) extends Provider {
  def usesClasses = Set.empty
}
case class UsesAnnotationEnum(name: Option[String], typeDescriptor: TypeDescriptor, value: String) extends Provider {
  def usesClasses = ProviderFinder.typeDescriptorToUsesClass(typeDescriptor)
}
case class UsesParameterAnnotation(descriptor: String) extends Provider {
  def usesClasses = ProviderFinder.typeDescriptorToUsesClass(descriptor)
}
case class UsesMethod(opcode: Int, owner: String, name: String, desc: String) extends Provider {
  def usesClasses = ProviderFinder.methodDescriptorToUsesClass(desc) ++ ProviderFinder.internalNamesToUsesClass(List(owner))
}
case class UsesField(opcode: Int, owner: String, name: String, desc: String) extends Provider {
  def usesClasses = ProviderFinder.typeDescriptorToUsesClass(desc) ++ ProviderFinder.internalNamesToUsesClass(List(owner))
}
case class UsesException(exceptionType: String) extends Provider {
  def usesClasses = Set(ProviderFinder.internalNameToUsesClass(exceptionType))
}

case class ProviderFinder extends org.objectweb.asm.ClassVisitor(Opcodes.ASM4) {
  type Elements = List[Provider]

  def asString(elements: Elements) = {
    elements.mkString("\n")
  }

  private val elements = Stack[Provider]()

  def getProvidedElements: Elements = elements.reverse.toList

  private def nullToEmptyList[T](xs: Array[T]) = xs match {
    case null => List.empty: List[T]
    case _ => xs.toList
  }

  override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]) = {
    val cls = ProvidesClass(
      version = version,
      access = access,
      internalName = name,
      signature = Option(signature),
      superName = superName,
      interfaces = nullToEmptyList(interfaces))
    elements.push(cls)
  }

  def pushAnnotationAndReturnANewVisitor(desc: String, visibleAtRuntime: Option[Boolean], usesGenerator: (String, Option[Boolean]) => Provider = { (n, v) => UsesAnnotation(n, v) }): AnnotationVisitor = {
    elements.push(usesGenerator(desc, visibleAtRuntime))
    new AnnotationVisitor(Opcodes.ASM4) {
      override def visitAnnotation(name: String, desc: String): AnnotationVisitor = pushAnnotationAndReturnANewVisitor(desc, visibleAtRuntime)
      override def visitArray(name: String) = pushAnnotationAndReturnANewVisitor("", None, { (_, _) => UsesAnnotationArray(name) })
      override def visitEnum(name: String, desc: String, value: String) = elements.push(UsesAnnotationEnum(Option(name), desc, value))
    }
  }

  override def visitField(access: Int, name: String, desc: String, signature: String, value: Object) = {
    val f = ProvidesField(access, name, desc, Option(signature), Option(value))
    elements.push(f)
    new FieldVisitor(Opcodes.ASM4) {
      override def visitAnnotation(desc: String, visible: Boolean) = pushAnnotationAndReturnANewVisitor(desc, some(visible))
    }
  }

  override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]) = {
    //    currentClassProvider = currentClassProvider.head.method( access, name, desc, signature, exceptions ) :: currentClassProvider.tail
    elements.push(ProvidesMethod(access, name, desc, Option(signature), nullToEmptyList(exceptions)))

    new MethodVisitor(Opcodes.ASM4) {
      override def visitAnnotation(desc: String, visible: Boolean) =
        pushAnnotationAndReturnANewVisitor(desc, some(visible))

      override def visitParameterAnnotation(parameter: Int, desc: String, visible: Boolean) =
        pushAnnotationAndReturnANewVisitor(desc, some(visible), (name, _) => UsesParameterAnnotation(name))

      //     public void visitEnum(String name, String desc, String value) {

      override def visitFieldInsn(opcode: Int, owner: String, name: String, desc: String) =
        elements.push(UsesField(opcode, owner, name, desc))

      override def visitMethodInsn(opcode: Int, owner: String, name: String, desc: String) =
        elements.push(UsesMethod(opcode, owner, name, desc))

      override def visitTryCatchBlock(start: Label, end: Label, handler: Label, exceptionType: String) =
        elements.push(UsesException(exceptionType))

      override def visitInvokeDynamicInsn(name: String, desc: String, bsm: Handle, bsmArgs: Object*) =
        null
    }
  }

  override def visitAnnotation(desc: String, visible: Boolean) = pushAnnotationAndReturnANewVisitor(desc, some(visible))
}

object ProviderFinder {
  def buildItems(cr: ClassReader)(implicit pf: ProviderFinder) = {
    cr.accept(pf, 0)
    pf.getProvidedElements
  }

  implicit val standardProviderFinder = ProviderFinder()
  def buildItemsFromClassName(klass: String, pf: ProviderFinder = ProviderFinder()): Option[List[Provider]] =
    Some(buildItems(new ClassReader(klass))(pf).toList)
  def buildItemsFromClassFile(filename: String, pf: ProviderFinder = ProviderFinder()): Option[List[Provider]] = {
    for {
      fis <- Option(new FileInputStream(filename))
      cr <- Option(new ClassReader(fis))
    } yield {
      buildItems(cr)(pf).toList
    }
  }

  //     val result: ClassMap = ProviderFinder.buildClassMap(List[pc, pm, pf])

  case class ClassProvides(targetClass: String, provides: Set[Provider])

  def buildProvidedItems(xs: List[Provider]) = {
    val provided = xs filter {
      case _: ProvidesField | _: ProvidesMethod => true
      case _ => false
    }

    val klass = xs.head.asInstanceOf[ProvidesClass]

    ClassProvides(klass.javaIdentifier.s, provided.toSet)
  }

  def parseResultUsesClass(fn: { def typesUsed: Set[JavaName] }) = fn.typesUsed map { _.toJava } map JavaIdentifier map UsesClass
  def typeDescriptorToUsesClass(descriptor: TypeDescriptor): Set[UsesClass] = parseResultUsesClass(JavaSignatureParser.parse(descriptor.s).get)
  def methodDescriptorToUsesClass(descriptor: MethodDescriptor): Set[UsesClass] = parseResultUsesClass(JavaSignatureParser.parseMethod(descriptor.s).get)

  def extractClasses(p: Provider): Set[UsesClass] = p.usesClasses
}
