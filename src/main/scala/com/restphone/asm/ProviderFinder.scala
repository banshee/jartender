package com.restphone.asm

import java.io.FileInputStream

import scala.collection.Set
import scala.collection.mutable

import org.objectweb.asm.ClassReader
import org.objectweb.asm.Opcodes

sealed abstract class ClassModifiers
case object IsInterface extends ClassModifiers
case object IsAnnotation extends ClassModifiers
case object IsEnum extends ClassModifiers
case object IsStatic extends ClassModifiers

sealed abstract class Provider
case class ProvidesClass( c : String ) extends Provider {
  def field( name : String ) = ProvidesField( this, name )
  def method( name : String, access : Set[ ClassModifiers ] ) = ProvidesMethod( this, name, access )
}
case class ProvidesInterface( klass : ProvidesClass, i : String ) extends Provider
case class ProvidesField( klass : ProvidesClass, f : String ) extends Provider
case class ProvidesMethod( klass : ProvidesClass, f : String, access : Set[ ClassModifiers ] ) extends Provider

case class ProviderFinder extends org.objectweb.asm.ClassVisitor( Opcodes.ASM4 ) {
  var currentClassProvider : Option[ ProvidesClass ] = None

  val elements = mutable.ArrayBuffer[ Provider ]()

  override def visit( version : Int, access : Int, name : String, signature : String, superName : String, interfaces : Array[ String ] ) = {
    currentClassProvider = Some( ProvidesClass( name ) )
    elements += currentClassProvider.get
  }

  override def visitField( access : Int, name : String, desc : String, signature : String, value : Object ) = {
    elements += currentClassProvider.get.field( name )
    null
  }

  override def visitMethod( access : Int, name : String, desc : String, signature : String, exceptions : Array[ String ] ) = {
    elements += currentClassProvider.get.method( name, AccessElements( access ) )
    null
  }

  case class AccessElements( i : Int ) {
    def toList = {
      def toList_impl( value : Int, result : List[ ClassModifiers ] ) : List[ ClassModifiers ] = {
        value match {
          case n if ( n & Opcodes.ACC_INTERFACE ) > 0 => toList_impl( n ^ Opcodes.ACC_INTERFACE, IsInterface :: result )
          case n if ( n & Opcodes.ACC_ANNOTATION ) > 0 => toList_impl( n ^ Opcodes.ACC_ANNOTATION, IsAnnotation :: result )
          case n if ( n & Opcodes.ACC_ENUM ) > 0 => toList_impl( n ^ Opcodes.ACC_ENUM, IsEnum :: result )
          case n if ( n & Opcodes.ACC_STATIC ) > 0 => toList_impl( n ^ Opcodes.ACC_STATIC, IsStatic :: result )
          case _ => result
        }
      }
      toList_impl( i, List.empty )
    }
  }
  object AccessElements {
    implicit def toSet( x : AccessElements ) : Set[ ClassModifiers ] = Set( x.toList : _* )
  }

  def getElements : Set[ Provider ] = Set( elements : _* )
}

object ProviderFinder {
  def buildItems( cr : ClassReader )( implicit pf : ProviderFinder ) = {
    cr.accept( pf, 0 )
    pf.getElements
  }
  implicit val standardProviderFinder = ProviderFinder()
  def buildItemsFromClassName( klass : String ) : Option[ Set[ Provider ] ] = Some(buildItems( new ClassReader( klass ) ))
  def buildItemsFromClassFile( filename : String ) : Option[ Set[ Provider ] ] = {
    for {
      fis <- Option( new FileInputStream( filename ) )
      cr <- Option( new ClassReader( fis ) )
    } yield {
      buildItems( cr )
    }
  }
}
