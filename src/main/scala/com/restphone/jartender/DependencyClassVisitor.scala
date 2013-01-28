package com.restphone.jartender

import java.io.FileInputStream
import java.util.jar.JarFile
import scala.Option.option2Iterable
import scala.actors.Futures.future
import scala.collection.JavaConverters.enumerationAsScalaIteratorConverter
import scala.collection.mutable.Stack
import org.objectweb.asm.AnnotationVisitor
import org.objectweb.asm.ClassReader
import org.objectweb.asm.FieldVisitor
import org.objectweb.asm.Handle
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes
import scalaz.Scalaz._
import com.restphone.javasignatureparser.JavaSignatureParser
import com.restphone.javasignatureparser.JavaName
import scala.language.reflectiveCalls

class DependencyClassVisitor extends org.objectweb.asm.ClassVisitor( Opcodes.ASM4 ) {
  type Elements = List[ ClassfileElement ]

  private var currentClassname : InternalName = InternalName( "none" )

  def asString( elements : Elements ) = {
    elements.mkString( "\n" )
  }

  private val elements = Stack[ ClassfileElement ]()

  def getProvidedElements : Elements = elements.reverse.toList

  private def nullToEmptyList[ T ]( xs : Array[ T ] ) = xs match {
    case null => List.empty
    case _    => xs.toList
  }

  override def visit( version : Int, access : Int, name : String, signature : String, superName : String, interfaces : Array[ String ] ) = {
    require( name != null, f"name cannot be null; signature is ${signature}, superName is ${superName}" )
    require( superName != null, f"superName cannot be null for class $name" )
    require( interfaces != null, f"interfaces cannot be null for class $name" )
    require( interfaces.toList forall { _ != null }, f"no interface can be null for class $name" )
    val cls = ProvidesClass(
      version = version,
      access = access,
      internalName = InternalName( name ),
      signature = OptionalSignature( signature ),
      superName = InternalName( superName ),
      interfaces = nullToEmptyList( interfaces ) filter { _ != null } map InternalName )
    elements.push( cls )
    currentClassname = cls.internalName
  }

  def pushAnnotationAndReturnANewVisitor( desc : TypeDescriptor, visibleAtRuntime : Option[ Boolean ], usesGenerator : ( TypeDescriptor, Option[ Boolean ] ) => ClassfileElement = { ( n, v ) => UsesAnnotation( n, v ) } ) : AnnotationVisitor = {
    elements.push( usesGenerator( desc, visibleAtRuntime ) )
    new AnnotationVisitor( Opcodes.ASM4 ) {
      override def visitAnnotation( name : String, typeDescriptor : String ) : AnnotationVisitor = pushAnnotationAndReturnANewVisitor( TypeDescriptor( typeDescriptor ), visibleAtRuntime )
      override def visitArray( name : String ) = pushAnnotationAndReturnANewVisitor( TypeDescriptor( "" ), None, { ( _, _ ) => UsesAnnotationArray( name ) } )
      override def visitEnum( name : String, typeDescriptor : String, value : String ) = elements.push( UsesAnnotationEnum( Option( name ), TypeDescriptor( typeDescriptor ), value ) )
    }
  }

  override def visitField( access : Int, name : String, desc : String, signature : String, value : Object ) = {
    val f = ProvidesField( currentClassname, access, InternalName( name ), TypeDescriptor( desc ), OptionalSignature( signature ), Option( value ) )
    elements.push( f )
    new FieldVisitor( Opcodes.ASM4 ) {
      override def visitAnnotation( desc : String, visible : Boolean ) = pushAnnotationAndReturnANewVisitor( TypeDescriptor( desc ), some( visible ) )
    }
  }

  override def visitMethod( access : Int, name : String, desc : String, signature : String, exceptions : Array[ String ] ) = {
    //    currentClassProvider = currentClassProvider.head.method( access, name, desc, signature, exceptions ) :: currentClassProvider.tail
    elements.push( ProvidesMethod( currentClassname, access, JavaIdentifier( name ), MethodDescriptor( desc ), OptionalSignature( signature ), nullToEmptyList( exceptions ) map InternalName ) )

    new MethodVisitor( Opcodes.ASM4 ) {
      override def visitAnnotation( desc : String, visible : Boolean ) =
        pushAnnotationAndReturnANewVisitor( TypeDescriptor( desc ), some( visible ) )

      override def visitParameterAnnotation( parameter : Int, desc : String, visible : Boolean ) =
        pushAnnotationAndReturnANewVisitor( TypeDescriptor( desc ), some( visible ), ( name, _ ) => UsesParameterAnnotation( name ) )

      //     public void visitEnum(String name, String desc, String value) {

      override def visitFieldInsn( opcode : Int, owner : String, name : String, desc : String ) =
        elements.push( UsesField( InternalName( owner ), name, TypeDescriptor( desc ) ) )

      override def visitMethodInsn( opcode : Int, owner : String, name : String, desc : String ) =
        elements.push( UsesMethod( InternalName( owner ), JavaIdentifier( name ), MethodDescriptor( desc ) ) )

      override def visitTryCatchBlock( start : Label, end : Label, handler : Label, catchBlockType : String ) = {
        // catchBlockType is null for finally blocks
        if ( catchBlockType != null ) elements.push( UsesException( InternalName( catchBlockType ) ) )
      }

      override def visitInvokeDynamicInsn( name : String, desc : String, bsm : Handle, bsmArgs : Object* ) =
        null
    }
  }

  override def visitAnnotation( desc : String, visible : Boolean ) = pushAnnotationAndReturnANewVisitor( TypeDescriptor( desc ), some( visible ) )
}

object DependencyClassVisitor {
  case class ClassProvides( targetClass : JavaIdentifier, provides : Set[ ClassfileElement ] )

  def buildProvidedItems( xs : List[ ClassfileElement ] ) : ClassProvides = {
    def is_ProvidesField : PartialFunction[ ClassfileElement, ClassfileElement ] = { case x : ProvidesField => x }
    def is_ProvidesMethod : PartialFunction[ ClassfileElement, ClassfileElement ] = { case x : ProvidesMethod => x }

    val provided = xs collect {
      case x : ProvidesField  => x
      case x : ProvidesMethod => x
    }

    val klass = xs.head.asInstanceOf[ ProvidesClass ]

    ClassProvides( klass.javaIdentifier, provided.toSet )
  }

  def typeDescriptorToUsesClass( descriptor : TypeDescriptor ) : Set[ UsesClass ] = { convert_ParseResult_to_UsesClass( JavaSignatureParser.parse( descriptor.s ).get ) }
  def methodDescriptorToUsesClass( descriptor : MethodDescriptor ) : Set[ UsesClass ] = convert_ParseResult_to_UsesClass( JavaSignatureParser.parseMethod( descriptor.s ).get )
  def convert_identifiers_to_UsesClasses( xs : Iterable[ IdentifierFlavor ] ) : Set[ UsesClass ] = xs map { _.usesClasses } reduce ( _ ++ _ )
  def expand_UsesClasses( xs : Set[ UsesElement ] ) = ( xs flatMap { _.usesClasses } ).toSet ++ xs

  private def convert_ParseResult_to_UsesClass( fn : { def typesUsed : Set[ JavaName ] } ) = fn.typesUsed map { _.toJava } map JavaIdentifier map UsesClass
}
