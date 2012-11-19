package com.restphone.jartender

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import scala.annotation.tailrec
import scala.collection._
import scala.collection.mutable.SynchronizedMap
import scala.collection.mutable
import scala.util.Random.shuffle

class DependencyFinderTest extends FunSuite with ShouldMatchers {
  import DependencyFinder._
  
  test("can calculate dependencies") {
    val dependencies: ImmediateDependencies[Symbol] = Map('a -> List('b, 'c), 'b -> List('a), 'c -> List('d))
    val result = findDependencies('a, dependencies, new mutable.HashMap[Symbol, Set[Symbol]]() with mutable.SynchronizedMap[Symbol, Set[Symbol]])
    result should be(Set('b, 'c, 'a, 'd))
    result should be(Set('a, 'b, 'c, 'd))
  }
}
