package com.restphone.jartender

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import scala.annotation.tailrec
import scala.collection._
import scala.collection.mutable.SynchronizedMap

class GraphWalkerTest extends FunSuite with ShouldMatchers {
  type ImmediateDependencies[T] = Map[T, List[T]]
  type DependencyCache[T] = scala.collection.mutable.HashMap[T, Set[T]] with SynchronizedMap[T, Set[T]]
  
  val dependencies: ImmediateDependencies[Symbol] = Map('a -> List('b, 'c), 'b -> List('a), 'c -> List('d))

  def findDependencies[T](x: T, d: ImmediateDependencies[T], cache: DependencyCache[T]): Set[T] = {
    def findDependenciesRecursive(x: T, d: ImmediateDependencies[T], parents: Set[T]): Set[T] = {
      cache.get(x) match {
        case None =>
    	  val firstLevelResult = d.get(x) getOrElse Set()
    	  val nextLevelResult = firstLevelResult filter {!parents.contains(_)} flatMap {a => findDependenciesRecursive(a, d, parents + a)}
    	  val result = (firstLevelResult ++ nextLevelResult).toSet
    	  cache.put(x, result)
    	  result
        case Some(value) => value
      }
    }
    findDependenciesRecursive(x, d, Set()).toSet
  }

  test("can calculate 'a dependencies") {
    println(dependencies)
    val result = findDependencies('a, dependencies, new mutable.HashMap[Symbol, Set[Symbol]]() with mutable.SynchronizedMap[Symbol, Set[Symbol]])
    result foreach { println(_) }
    result should be(Set('b, 'c, 'a, 'd))
  }
}
