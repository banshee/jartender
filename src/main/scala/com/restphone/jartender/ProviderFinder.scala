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
  override def toString = f"ProvidesClass[name=$name,\n interfaces=$interfaces\n]"
}
case class ProvidesField(access: Int, name: String, desc: String, signature: String, value: Object) extends Provider {
  override def toString = f"ProvidesField[name=${name} desc=$desc]"
}

object ProvidesFieldWithName {
  def unapply(f: ProvidesField) = { if (f.name === nm) some(f) else None }
}

case class ProvidesMethod(
  access: Int,
  name: String,
  desc: String,
  signature: Option[String],
  exceptions: List[String]) extends Provider {
  override def toString = f"ProvidesMethod[name=${name} desc=$desc]"
}
case class UsesClass(name: String) extends Provider
case class UsesAnnotation(name: String) extends Provider
case class UsesParameterAnnotation(name: String) extends Provider
case class UsesMethod(opcode: Int, owner: String, name: String, desc: String) extends Provider
case class UsesField(opcode: Int, owner: String, name: String, desc: String) extends Provider

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

  def pushAnnotationAndReturnANewVisitor(desc: String, visibleAtRuntime: Boolean, usesGenerator: String => Provider = { UsesAnnotation(_) }): AnnotationVisitor = {
    elements.push(usesGenerator(desc))
    new AnnotationVisitor(Opcodes.ASM4) {
      override def visitAnnotation(name: String, desc: String): AnnotationVisitor = pushAnnotationAndReturnANewVisitor(desc, visibleAtRuntime, usesGenerator)
    }
  }

  override def visitField(access: Int, name: String, desc: String, signature: String, value: Object) = {
    val f = ProvidesField(access, name, desc, signature, value)
    elements.push(f)
    new FieldVisitor(Opcodes.ASM4) {
      override def visitAnnotation(desc: String, visible: Boolean) = pushAnnotationAndReturnANewVisitor(desc, visible)
    }
  }

  override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]) = {
    //    currentClassProvider = currentClassProvider.head.method( access, name, desc, signature, exceptions ) :: currentClassProvider.tail
    elements.push(ProvidesMethod(access, name, desc, Option(signature), nullToEmptyList(exceptions)))
    new MethodVisitor(Opcodes.ASM4) {
      //     public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
      override def visitAnnotation(desc: String, visible: Boolean) =
        pushAnnotationAndReturnANewVisitor(desc, visible)

      override def visitParameterAnnotation(parameter: Int, desc: String, visible: Boolean) =
        pushAnnotationAndReturnANewVisitor(desc, visible, UsesParameterAnnotation(_))

      override def visitFieldInsn(opcode: Int, owner: String, name: String, desc: String) =
        elements.push(UsesField(opcode, owner, name, desc))

      //     public void visitMethodInsn(int opcode, String owner, String name, String desc) {
      override def visitMethodInsn(opcode: Int, owner: String, name: String, desc: String) =
        elements.push(UsesMethod(opcode, owner, name, desc))

      //     public void visitInvokeDynamicInsn(String name, String desc, Handle bsm, Object... bsmArgs) {
      override def visitInvokeDynamicInsn(name: String, desc: String, bsm: Handle, bsmArgs: Object*) =
        null
    }
  }

  override def visitAnnotation(desc: String, visible: Boolean) = pushAnnotationAndReturnANewVisitor(desc, visible)
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
