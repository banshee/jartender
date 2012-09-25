package com.restphone.asm

import java.io.FileInputStream
import java.util.ArrayList
import org.objectweb.asm.AnnotationVisitor
import org.objectweb.asm.FieldVisitor
import org.objectweb.asm.Opcodes
import scala.collection._
import org.objectweb.asm.ClassReader
import java.io.FileInputStream
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import org.scalatest.FunSuite
import scala.collection.mutable.Stack
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scalaz._
import Scalaz._
import org.objectweb.asm.MethodVisitor

sealed abstract class Consumer
case class ConsumesClass( k : String ) extends Consumer
case class ConsumesInterface( i : String ) extends Consumer
case class ConsumesField( f : String ) extends Consumer // only care about what types of fields we define as a user
case class ConsumesMethod( opcode : Int, owner : String, name : String, desc : String ) extends Consumer
case class ConsumesTypeInstruction( opcode : Int, typeString : String ) extends Consumer
case class ConsumesFieldInstruction( opcode : Int, owner : String, name : String, desc : String ) extends Consumer

case class UserFinder extends org.objectweb.asm.ClassVisitor( Opcodes.ASM4 ) {
  val consumes = mutable.ArrayBuffer[ Consumer ]()

  override def visit( version : Int,
                      access : Int, name : String,
                      signature : String,
                      superName : String,
                      interfaces : Array[ String ] ) = {
    consumes += ConsumesClass( superName )
    consumes ++= interfaces map ConsumesInterface
  }

  override def visitField( access : Int, name : String, desc : String, signature : String, value : Object ) = {
    consumes += ConsumesField( desc )
    null
  }

  override def visitMethod( access : Int, name : String, desc : String, signature : String, exceptions : Array[ String ] ) = {
    val p = JavaSignatureParser.parse( desc )

    consumes += ConsumesClass( desc )

    new MethodVisitor( Opcodes.ASM4 ) {
      override def visitMethodInsn( opcode : Int, owner : String, name : String, desc : String ) =
        consumes += ConsumesMethod( opcode, owner, name, desc )

      // Used for new and typeof
      override def visitTypeInsn( opcode : Int, typeString : String ) =
        consumes += ConsumesTypeInstruction( opcode, typeString )

      override def visitFieldInsn( opcode : Int, owner : String, name : String, desc : String ) =
        consumes += ConsumesFieldInstruction( opcode, owner, name, desc )
    }
  }
}

object UserFinder {
}
