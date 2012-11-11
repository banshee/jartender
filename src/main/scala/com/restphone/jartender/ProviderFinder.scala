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

trait IdentifierFlavor extends Any {
  def usesClasses: Set[UsesClass]
}
case class InternalName(s: String) extends AnyVal with IdentifierFlavor {
  def javaIdentifier = JavaIdentifier(s.replace("/", "."))
  def usesClasses = javaIdentifier.usesClasses
}
case class MethodDescriptor(s: String) extends AnyVal with IdentifierFlavor {
  def usesClasses = ProviderFinder.methodDescriptorToUsesClass(this)
}
case class TypeDescriptor(s: String) extends AnyVal with IdentifierFlavor {
  def usesClasses = ProviderFinder.typeDescriptorToUsesClass(this)
}
case class JavaIdentifier(s: String) extends AnyVal with IdentifierFlavor {
  def usesClasses = Set(UsesClass(this))
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
trait UsesClassesIsBuiltFromTypeDescriptor {
  def typeDescriptor: TypeDescriptor
  def usesClasses = typeDescriptor.usesClasses
}
trait UsesClassesIsTheEmptySet {
  def usesClasses = Set.empty[UsesClass]
}
sealed abstract class Provider {
  def usesClasses: Set[UsesClass]
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
  interfaces: List[InternalName]) extends Provider {
  import scalaz._
  import Scalaz._
  // Note that interfaces are classes with access bits of ACC_INTERFACE and ACC_ABSTRACT set (0x400, 0x200)
  //  def field(access: Int, name: String, desc: String, signature: String, value: Object, annotations: List[UsesClass]) = ProvidesField(access, name, desc, Option(signature), value)
  //  def method(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]) = ProvidesMethod(access, name, desc, Option(signature), exceptions.toList)
  val javaIdentifier = internalName.javaIdentifier;
  val internalNames = internalName :: interfaces
  def usesClasses = ProviderFinder.convertListOfProviderToUsesClasses(internalName :: interfaces)
}
object ProvidesClass {
  def createProvidesClassMatcher(fn: ProvidesClass => Boolean) = new Object {
    def unapply(f: ProvidesClass) = fn(f)
  }
}
case class ProvidesField(access: Int, name: InternalName, typeDescriptor: TypeDescriptor, signature: Option[Signature], value: Option[Object]) extends Provider with UsesClassesIsBuiltFromTypeDescriptor
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
  def usesClasses = ProviderFinder.convertListOfProviderToUsesClasses(desc :: exceptions)
}
case class UsesClass(javaIdentifier: JavaIdentifier) extends Provider {
  def usesClasses = Set(this)
}

case class UsesAnnotation(typeDescriptor: TypeDescriptor, visibleAtRuntime: Option[Boolean]) extends Provider with UsesClassesIsBuiltFromTypeDescriptor

case class UsesAnnotationArray(name: String) extends Provider with UsesClassesIsTheEmptySet
case class UsesAnnotationEnum(name: Option[String], typeDescriptor: TypeDescriptor, value: String) extends Provider with UsesClassesIsBuiltFromTypeDescriptor
case class UsesParameterAnnotation(typeDescriptor: TypeDescriptor) extends Provider with UsesClassesIsBuiltFromTypeDescriptor
case class UsesMethod(opcode: Int, owner: InternalName, name: JavaIdentifier, desc: MethodDescriptor) extends Provider {
  def usesClasses = ProviderFinder.convertListOfProviderToUsesClasses(List(desc, owner))
}
case class UsesField(opcode: Int, owner: InternalName, name: String, desc: TypeDescriptor) extends Provider {
  def usesClasses = ProviderFinder.convertListOfProviderToUsesClasses(List(desc, owner))
}
case class UsesException(exceptionType: InternalName) extends Provider {
  def usesClasses = exceptionType.usesClasses
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
      internalName = InternalName(name),
      signature = OptionalSignature(signature),
      superName = InternalName(superName),
      interfaces = nullToEmptyList(interfaces) map InternalName)
    elements.push(cls)
  }

  def pushAnnotationAndReturnANewVisitor(desc: TypeDescriptor, visibleAtRuntime: Option[Boolean], usesGenerator: (TypeDescriptor, Option[Boolean]) => Provider = { (n, v) => UsesAnnotation(n, v) }): AnnotationVisitor = {
    elements.push(usesGenerator(desc, visibleAtRuntime))
    new AnnotationVisitor(Opcodes.ASM4) {
      override def visitAnnotation(name: String, typeDescriptor: String): AnnotationVisitor = pushAnnotationAndReturnANewVisitor(TypeDescriptor(typeDescriptor), visibleAtRuntime)
      override def visitArray(name: String) = pushAnnotationAndReturnANewVisitor(TypeDescriptor(""), None, { (_, _) => UsesAnnotationArray(name) })
      override def visitEnum(name: String, typeDescriptor: String, value: String) = elements.push(UsesAnnotationEnum(Option(name), TypeDescriptor(typeDescriptor), value))
    }
  }

  override def visitField(access: Int, name: String, desc: String, signature: String, value: Object) = {
    val f = ProvidesField(access, InternalName(name), TypeDescriptor(desc), OptionalSignature(signature), Option(value))
    elements.push(f)
    new FieldVisitor(Opcodes.ASM4) {
      override def visitAnnotation(desc: String, visible: Boolean) = pushAnnotationAndReturnANewVisitor(TypeDescriptor(desc), some(visible))
    }
  }

  override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]) = {
    //    currentClassProvider = currentClassProvider.head.method( access, name, desc, signature, exceptions ) :: currentClassProvider.tail
    elements.push(ProvidesMethod(access, JavaIdentifier(name), MethodDescriptor(desc), OptionalSignature(signature), nullToEmptyList(exceptions) map InternalName))

    new MethodVisitor(Opcodes.ASM4) {
      override def visitAnnotation(desc: String, visible: Boolean) =
        pushAnnotationAndReturnANewVisitor(TypeDescriptor(desc), some(visible))

      override def visitParameterAnnotation(parameter: Int, desc: String, visible: Boolean) =
        pushAnnotationAndReturnANewVisitor(TypeDescriptor(desc), some(visible), (name, _) => UsesParameterAnnotation(name))

      //     public void visitEnum(String name, String desc, String value) {

      override def visitFieldInsn(opcode: Int, owner: String, name: String, desc: String) =
        elements.push(UsesField(opcode, InternalName(owner), name, TypeDescriptor(desc)))

      override def visitMethodInsn(opcode: Int, owner: String, name: String, desc: String) =
        elements.push(UsesMethod(opcode, InternalName(owner), JavaIdentifier(name), MethodDescriptor(desc)))

      override def visitTryCatchBlock(start: Label, end: Label, handler: Label, exceptionType: String) =
        elements.push(UsesException(InternalName(exceptionType)))

      override def visitInvokeDynamicInsn(name: String, desc: String, bsm: Handle, bsmArgs: Object*) =
        null
    }
  }

  override def visitAnnotation(desc: String, visible: Boolean) = pushAnnotationAndReturnANewVisitor(TypeDescriptor(desc), some(visible))
}

object ProviderFinder {
  def buildItems(cr: ClassReader)(implicit pf: ProviderFinder) = {
    cr.accept(pf, 0)
    pf.getProvidedElements
  }

  implicit val standardProviderFinder = ProviderFinder()
  def buildItemsFromClassName(klass: InternalName, pf: ProviderFinder = ProviderFinder()): Option[List[Provider]] =
    Some(buildItems(new ClassReader(klass.s))(pf).toList)
  def buildItemsFromClassFile(filename: String, pf: ProviderFinder = ProviderFinder()): Option[List[Provider]] = {
    for {
      fis <- Option(new FileInputStream(filename))
      cr <- Option(new ClassReader(fis))
    } yield {
      buildItems(cr)(pf).toList
    }
  }

  //     val result: ClassMap = ProviderFinder.buildClassMap(List[pc, pm, pf])

  case class ClassProvides(targetClass: JavaIdentifier, provides: Set[Provider])

  def buildProvidedItems(xs: List[Provider]) : ClassProvides = {
    val provided = xs filter {
      case _: ProvidesField | _: ProvidesMethod => true
      case _ => false
    }

    val klass = xs.head.asInstanceOf[ProvidesClass]

    ClassProvides(klass.javaIdentifier, provided.toSet)
  }

  def convertListOfProviderToUsesClasses(xs: Iterable[IdentifierFlavor]) = xs map { _.usesClasses } reduce (_ ++ _)

  def typeDescriptorToUsesClass(descriptor: TypeDescriptor): Set[UsesClass] = parseResultUsesClass(JavaSignatureParser.parse(descriptor.s).get)
  def methodDescriptorToUsesClass(descriptor: MethodDescriptor): Set[UsesClass] = parseResultUsesClass(JavaSignatureParser.parseMethod(descriptor.s).get)
  
  private def parseResultUsesClass(fn: { def typesUsed: Set[JavaName] }) = fn.typesUsed map { _.toJava } map JavaIdentifier map UsesClass
}
