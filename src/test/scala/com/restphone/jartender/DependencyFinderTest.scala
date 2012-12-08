package com.restphone.jartender

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import scala.annotation.tailrec
import scala.collection._
import scala.collection.mutable.SynchronizedMap
import scala.collection.mutable
import scala.util.Random.shuffle
import scalaz._
import Scalaz._
import java.io.File
import scala.PartialFunction._

class DependencyFinderTest extends FunSuite with ShouldMatchers {
  import DependencyFinder._

  test( "can calculate dependencies" ) {
    val dependencies: ImmediateDependencies[Symbol] = Map( 'a -> List( 'b, 'c ), 'b -> List( 'a ), 'c -> List( 'd ) )
    val result = findDependencies( 'a, dependencies, new mutable.HashMap[Symbol, Set[Symbol]]() with mutable.SynchronizedMap[Symbol, Set[Symbol]] )
    result should be( Set( 'b, 'c, 'a, 'd ) )
    result should be( Set( 'a, 'b, 'c, 'd ) )
  }

  test( "can read a jarfile" ) {
    val jarfile = getResource( "jarfiles/libjar.jar" )
    jarfile should be( 'defined )
    val r1 = DependencyAnalyser.buildItemsFromFile( new File( jarfile.get ) )
    val r2 = r1 match {
      case Success( ( h: DependencyAnalyser.JarfileResult ) :: t ) => some( h )
      case _ => None
    }
    r2 should be( 'defined )
  }

  test( "can gracefully fail when given a nonexistant file" ) {
    val r1 = DependencyAnalyser.buildItemsFromFile( new File( "/should/not/exist.jar" ) )
    r1 should be( 'failure )
  }

  test( "can gracefully fail when given an empty jar file" ) {
    val jarfile = getResource( "jarfiles/empty.jar" )
    jarfile should be( 'defined )
    val r1 = DependencyAnalyser.buildItemsFromFile( new File(jarfile.get) )
    r1 should be( 'failure )
  }

  def getResource( s: String ) = {
    val extractFilePathExpr = """^file:/(.*)""".r
    for {
      root <- Option( Thread.currentThread.getContextClassLoader.getResource( s ) )
      extractFilePathExpr( f ) <- Option(root.toString)
    } yield f
  }
}
