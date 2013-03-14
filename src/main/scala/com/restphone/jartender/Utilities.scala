package com.restphone.jartender

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import scala.annotation.tailrec
import scala.collection.immutable.Stream.consWrapper
import scala.util.control.Exception.catching
import scalaz.Scalaz._
import java.io.ObjectStreamClass

object NotNull {
  val catchNull = catching( classOf[ NullPointerException ] )

  def apply[ T ]( x : => T, msg : String = "must not be null" ) : Option[ T ] = {
    catchNull.opt( x ) match {
      case None | Some( null ) => throw new RuntimeException( msg )
      case x                   => x
    }
  }
}

object RichFile {
  def slurp( f : File ) = {
    val s = scala.io.Source.fromFile( f )
    val result = s.getLines.mkString( "\n" )
    s.close()
    result
  }
  def ensureDirExists( f : File ) =
    if ( !f.exists ) f.mkdir

  def tree( root : File, descendCheck : File => Boolean = { _ => true } ) : Stream[ File ] = {
    require( root != null )
    def directoryEntries( f : File ) = for {
      direntries <- Option( f.list ).toStream
      d <- direntries
    } yield new File( f, d )
    val shouldDescend = root.isDirectory && descendCheck( root )
    ( root.exists, shouldDescend ) match {
      case ( false, _ )    => Stream.Empty
      case ( true, true )  => root #:: ( directoryEntries( root ) flatMap { tree( _, descendCheck ) } )
      case ( true, false ) => Stream( root )
    }
  }

  def splitFile( f : File ) : List[ File ] = {
    @tailrec def splitFileRecursive( f : File, acc : List[ File ] ) : List[ File ] = {
      f.getParentFile match {
        case null => f :: acc
        case p    => splitFileRecursive( p, f :: acc )
      }
    }
    splitFileRecursive( f, List() )
  }
  def joinFile( fs : List[ File ] ) = fs.mkString( File.pathSeparator )

  def treeIgnoringHiddenFilesAndDirectories( root : File ) = tree( root, { !_.isHidden } ) filter { !_.isHidden }

  def stringToFile( s : String ) = new File( s )
}

object SerializableUtilities {
  def convertToByteArray( x : Serializable ) = {
    val out = new ByteArrayOutputStream
    val objout = new ObjectOutputStream( out )
    objout.writeObject( x )
    out.toByteArray
  }

  def byteArrayToObject[ T ]( bytes : Array[ Byte ] ) : Option[ T ] = {
    try {
      // Deserialization in Eclipse was having a problem; it was trying
      // to deserialize with the wrong classloader.  Use a subclass
      // of ObjectInputStream so I can specify the classloader for 
      // the current thread.
      class CustomObjectInputStream( in : java.io.InputStream, classLoader : ClassLoader ) extends ObjectInputStream( in ) {
        override protected def resolveClass( desc : ObjectStreamClass ) : Class[ _ ] = {
          Class.forName( desc.getName(), false, classLoader )
        }
      }

      val input = new ByteArrayInputStream( bytes )
      val readstream = new CustomObjectInputStream( input, Thread.currentThread.getContextClassLoader)
      some( readstream.readObject.asInstanceOf[ T ] )
    } catch {
      case _ : Throwable => None
    }
  }
}
