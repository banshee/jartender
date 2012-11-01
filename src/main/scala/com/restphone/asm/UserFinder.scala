package com.restphone.jartender

import scala.Array.fallbackCanBuildFrom
import scala.collection.mutable
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes
import com.restphone.parser.JavaSignatureParser

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
