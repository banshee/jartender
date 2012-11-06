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

sealed abstract class ClassModifiers
case object IsInterface extends ClassModifiers
case object IsAnnotation extends ClassModifiers
case object IsEnum extends ClassModifiers
case object IsStatic extends ClassModifiers

sealed abstract class Provider
case class ProvidesClass(
  version: Int,
  access: Int,
  name: String,
  signature: String,
  superName: String,
  interfaces: List[String]) extends Provider {
  // Note that interfaces are classes with access bits of ACC_INTERFACE and ACC_ABSTRACT set (0x400, 0x200)
  def field(access: Int, name: String, desc: String, signature: String, value: Object, annotations: List[UsesClass]) = ProvidesField(access, name, desc, signature, value)
  def method(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]) = ProvidesMethod(access, name, desc, Option(signature), exceptions.toList)
//  override def toString = f"ProvidesClass[name=$name,\n interfaces=$interfaces\n]"
}
object ProvidesClass {
  def createProvidesClassMatcher(fn: ProvidesClass => Boolean) = new Object {
    def unapply(f: ProvidesClass) = fn(f)
  }
}
case class ProvidesField(access: Int, name: String, desc: String, signature: String, value: Object) extends Provider {
  override def toString = f"ProvidesField[name=${name} desc=$desc]"
}
object ProvidesField {
  def createProvidesFieldMatcher(fn: ProvidesField => Boolean) = new Object {
    def unapply(f: ProvidesField) = fn(f)
  }
}
case class ProvidesMethod(
  access: Int,
  name: String,
  desc: String,
  signature: Option[String],
  exceptions: List[String]) extends Provider {
  //  override def toString = f"ProvidesMethod[name=${name} desc=$desc]"
}
case class UsesClass(name: String) extends Provider
case class UsesAnnotation(name: String, visibleAtRuntime: Option[Boolean]) extends Provider
case class UsesAnnotationArray(name: String) extends Provider
case class UsesAnnotationEnum(name: String, desc: String, value: String) extends Provider
case class UsesParameterAnnotation(name: String) extends Provider
case class UsesMethod(opcode: Int, owner: String, name: String, desc: String) extends Provider
case class UsesField(opcode: Int, owner: String, name: String, desc: String) extends Provider
case class UsesException(exceptionType: String) extends Provider

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
    val cls = ProvidesClass(version = version,
      access = access,
      name = name,
      signature = signature,
      superName = superName,
      interfaces = interfaces.toList)
    elements.push(cls)
  }

  val defaultUsesAnnotationGenerator : (String, Option[Boolean]) => Provider = { (n, v) => UsesAnnotation(n, v) }
  
  def pushAnnotationAndReturnANewVisitor(desc: String, visibleAtRuntime: Option[Boolean], usesGenerator: (String, Option[Boolean]) => Provider = { (n, v) => UsesAnnotation(n, v) }): AnnotationVisitor = {
    elements.push(usesGenerator(desc, visibleAtRuntime))
    new AnnotationVisitor(Opcodes.ASM4) {
      override def visitAnnotation(name: String, desc: String): AnnotationVisitor = pushAnnotationAndReturnANewVisitor(desc, visibleAtRuntime)
      override def visitArray(name: String) = pushAnnotationAndReturnANewVisitor("", None,  { (_, _) => UsesAnnotationArray(name) })
      override def visitEnum(name: String, desc: String, value: String) = elements.push(UsesAnnotationEnum(name, desc, value))
    }
  }

  override def visitField(access: Int, name: String, desc: String, signature: String, value: Object) = {
    val f = ProvidesField(access, name, desc, signature, value)
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
}
