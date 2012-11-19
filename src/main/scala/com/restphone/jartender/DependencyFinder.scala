package com.restphone.jartender

import scala.annotation.tailrec
import scala.collection._
import scala.collection.mutable.SynchronizedMap
import scala.collection.mutable
import scala.util.Random.shuffle

object DependencyFinder {
  type ImmediateDependencies[T] = Map[T, List[T]]
  type DependencyCache[T] = mutable.HashMap[T, Set[T]] with SynchronizedMap[T, Set[T]]
  
  def findDependencies[T](x: T, d: ImmediateDependencies[T], cache: DependencyCache[T]): Set[T] = {
    def findDependenciesRecursive(x: T, d1: ImmediateDependencies[T], ancestors: Set[T]): Set[T] = {
      cache.get(x) match {
        case None =>
    	  val firstLevelResult = shuffle(d.get(x) getOrElse Set())
    	  val nextLevelResult = firstLevelResult filter {!ancestors.contains(_)} flatMap {a => findDependenciesRecursive(a, d, ancestors + a)}
    	  val result = (firstLevelResult ++ nextLevelResult).toSet
    	  cache.put(x, result)
    	  result
        case Some(value) => value
      }
    }
    findDependenciesRecursive(x, d, Set()).toSet
  }
}
