package com.restphone.asm

import java.io.FileInputStream
import scala.collection.Set
import scala.collection.mutable
import org.objectweb.asm.ClassReader
import org.objectweb.asm.Opcodes
import org.objectweb.asm.AnnotationVisitor
import org.objectweb.asm.FieldVisitor
import scalaz.Lens
import scalaz.Lens._
import scala.collection.mutable.Stack

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
  interfaces: List[String],
  annotations: Stack[UsesClass],
  methods: Stack[ProvidesMethod],
  fields: Stack[ProvidesField]
  ) extends Provider {
  // Note that interfaces are classes with access bits of ACC_INTERFACE and ACC_ABSTRACT set (0x400, 0x200)
  def field(access: Int, name: String, desc: String, signature: String, value: Object, annotations: List[UsesClass]) = ProvidesField(this, access, name, desc, signature, value)
  def method(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]) = ProvidesMethod(this, access, name, desc, signature, exceptions)
  override def toString = f"ProvidesClass[name=$name]"
}
case class ProvidesField(klass: ProvidesClass, access: Int, name: String, desc: String, signature: String, value: Object,
  annotations: Stack[UsesClass] = Stack()) extends Provider {
  override def toString = f"ProvidesField[name=${klass.name}.${name}.$desc]"
}
case class ProvidesMethod(klass: ProvidesClass,
  access: Int,
  name: String,
  desc: String,
  signature: String,
  exceptions: Array[String],
  annotations: Stack[UsesClass] = Stack()) extends Provider {
//  override def toString = f"ProvidesMethod[name=${klass.name}.${name}.$desc]"
}

sealed abstract class ElementUser
case class UsesClass(name: String) extends ElementUser
case class UsesMethod(name: String, klass: String) extends ElementUser
case class UsesField(name: String, klass: String) extends ElementUser

case class ProviderFinder extends org.objectweb.asm.ClassVisitor(Opcodes.ASM4) {
  var currentClassProvider = Stack[ProvidesClass]()

  def replaceHeadClass(c: ProvidesClass) = currentClassProvider.update(0, c)

  def getProvidedElements = currentClassProvider

  override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]) = {
    val cls = ProvidesClass(version = version, access = access,
      name = name,
      signature = signature,
      superName = superName,
      interfaces = interfaces.toList,
      annotations = Stack(),
      methods = Stack(),
      fields = Stack())
    currentClassProvider.push(cls)
  }

  override def visitField(access: Int, name: String, desc: String, signature: String, value: Object) = {
    val currentClass = currentClassProvider.head
    val f = currentClass.field(access, name, desc, signature, value, annotations = List.empty)
    currentClass.fields.push(f)
    new FieldVisitor(Opcodes.ASM4) {
      override def visitAnnotation(desc: String, visibleAtRuntime: Boolean) = {
        f.annotations.push(UsesClass(desc))
        null
      }
    }
  }

  override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]) = {
    //    currentClassProvider = currentClassProvider.head.method( access, name, desc, signature, exceptions ) :: currentClassProvider.tail
    val h = currentClassProvider.top
    val m = h.method( access, name, desc, signature, exceptions )
    h.methods.push(m)
    null
  }

  override def visitAnnotation(desc: String, visible: Boolean) = {
    val h = currentClassProvider.top
    h.annotations.push(UsesClass(desc))
    null
  }

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
