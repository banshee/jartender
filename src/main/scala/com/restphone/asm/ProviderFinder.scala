package com.restphone.asm

import java.io.FileInputStream
import scala.collection.Set
import scala.collection.mutable
import org.objectweb.asm.ClassReader
import org.objectweb.asm.Opcodes
import org.objectweb.asm.AnnotationVisitor
import org.objectweb.asm.FieldVisitor

sealed abstract class ClassModifiers
case object IsInterface extends ClassModifiers
case object IsAnnotation extends ClassModifiers
case object IsEnum extends ClassModifiers
case object IsStatic extends ClassModifiers

sealed abstract class Provider
case class ProvidesClass(
  version : Int,
  access : Int,
  name : String,
  signature : String,
  superName : String,
  interfaces : List[ String ],
  annotations : List[ UsesClass ],
  methods : List[ ProvidesMethod ],
  fields : List[ ProvidesField ] ) extends Provider {
  // Note that interfaces are classes with access bits of ACC_INTERFACE and ACC_ABSTRACT set (0x400, 0x200)
  def field( access : Int, name : String, desc : String, signature : String, value : Object ) = ProvidesField( this, access, name, desc, signature, value )
  def method( access : Int, name : String, desc : String, signature : String, exceptions : Array[ String ] ) = ProvidesMethod( this, access, name, desc, signature, exceptions )
  override def toString = f"ProvidesClass[name=$name]"
}
case class ProvidesField( klass : ProvidesClass, access : Int, name : String, desc : String, signature : String, value : Object ) extends Provider {
  override def toString = f"ProvidesField[name=${klass.name}.${name}.$desc]"
}
case class ProvidesMethod( klass : ProvidesClass, access : Int, name : String, desc : String, signature : String, exceptions : Array[ String ] ) extends Provider {
  override def toString = f"ProvidesMethod[name=${klass.name}.${name}.$desc]"
}
case class ProvidesAnnotation( name : String, visible : Boolean ) extends Provider

sealed abstract class ElementUser
case class UsesClass( name : String ) extends ElementUser
case class UsesMethod( name : String, klass : String ) extends ElementUser
case class UsesField( name : String, klass : String ) extends ElementUser

case class ProviderFinder extends org.objectweb.asm.ClassVisitor( Opcodes.ASM4 ) {
  var currentClassProvider = List[ ProvidesClass ]()

  private val elements = mutable.ArrayBuffer[ Provider ]()

  def getProvidedElements = elements.toList

  override def visit( version : Int, access : Int, name : String, signature : String, superName : String, interfaces : Array[ String ] ) = {
    val cls = ProvidesClass( version = version, access = access,
      name = name,
      signature = signature,
      superName = superName,
      interfaces = interfaces.toList,
      annotations = List.empty,
      methods = List.empty,
      fields = List.empty )
    currentClassProvider = cls :: currentClassProvider
  }

  override def visitField( access : Int, name : String, desc : String, signature : String, value : Object ) = {
    val headProvider = currentClassProvider.head
    val f = headProvider.field( access, name, desc, signature, value )
    val newHead = headProvider.copy(fields = f :: headProvider.fields)
    currentClassProvider = newHead :: currentClassProvider.tail
    new FieldVisitor(Opcodes.ASM4) {
      override def visitAnnotation(desc: String, visibleAtRuntime: Boolean) = {
        null
      }
    }
  }

  override def visitMethod( access : Int, name : String, desc : String, signature : String, exceptions : Array[ String ] ) = {
    elements += currentClassProvider.get.method( access, name, desc, signature, exceptions )
    null
  }

  override def visitAnnotation( desc : String, visible : Boolean ) = {
    elements += ProvidesAnnotation( desc, visible )
    null
  }

  def annotationVisitorCreator( parent : Option[ org.objectweb.asm.AnnotationVisitor ] = None ) : AnnotationVisitor = {
    new org.objectweb.asm.AnnotationVisitor( Opcodes.ASM4 ) {
      override def visit( name : String, value : Object ) = {
        println( f"in visit, name is $name, parent is $parent" )
      }

      override def visitEnum( name : String, desc : String, value : String ) = {
      }

      //    public AnnotationVisitor visitAnnotation(String name, String desc) {
      override def visitAnnotation( name : String, desc : String ) : AnnotationVisitor = {
        annotationVisitorCreator( Some( this ) )
      }

      //    public AnnotationVisitor visitArray(String name) {
      override def visitArray( name : String ) : AnnotationVisitor = {
        annotationVisitorCreator( Some( this ) )
      }
    }
  }
}

//case class AccessElements(i: Int) {
//    def toList = {
//      def toList_impl(value: Int, result: List[ClassModifiers]): List[ClassModifiers] = {
//        value match {
//          case n if (n & Opcodes.ACC_INTERFACE) > 0 => toList_impl(n ^ Opcodes.ACC_INTERFACE, IsInterface :: result)
//          case n if (n & Opcodes.ACC_ANNOTATION) > 0 => toList_impl(n ^ Opcodes.ACC_ANNOTATION, IsAnnotation :: result)
//          case n if (n & Opcodes.ACC_ENUM) > 0 => toList_impl(n ^ Opcodes.ACC_ENUM, IsEnum :: result)
//          case n if (n & Opcodes.ACC_STATIC) > 0 => toList_impl(n ^ Opcodes.ACC_STATIC, IsStatic :: result)
//          case _ => result
//        }
//      }
//      toList_impl(i, List.empty)
//    }
//  }
//
//  object AccessElements {
//    implicit def toSet(x: AccessElements): Set[ClassModifiers] = Set(x.toList: _*)
//  }
//
//  def getElements: Set[Provider] = Set(elements: _*)
//}
//
object ProviderFinder {
  def buildItems( cr : ClassReader )( implicit pf : ProviderFinder ) = {
    cr.accept( pf, 0 )
    pf.getProvidedElements
  }

  implicit val standardProviderFinder = ProviderFinder()
  def buildItemsFromClassName( klass : String, pf : ProviderFinder = ProviderFinder() ) : Option[ List[ Provider ] ] =
    Some( buildItems( new ClassReader( klass ) )( pf ) )
  def buildItemsFromClassFile( filename : String, pf : ProviderFinder = ProviderFinder() ) : Option[ List[ Provider ] ] = {
    for {
      fis <- Option( new FileInputStream( filename ) )
      cr <- Option( new ClassReader( fis ) )
    } yield {
      buildItems( cr )( pf )
    }
  }
}
