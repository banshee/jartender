package com.restphone.jartender

object Completable {
  import scalaz._
  import Scalaz._
  import scala.PartialFunction._

  implicit class Completable[E, A]( v: Validation[E, A] )( implicit isComplete: A => Option[A] ) {
    def completeOrElse( x: => Validation[E, A] ) =
      v.fold( fail = _ => x, succ = isComplete( _ ).fold( x )( _.success ) )
  }
}

