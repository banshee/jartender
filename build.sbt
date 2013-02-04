// use sbt deliver-local to create ivy.xml

name := "Jartender"

organization := "com.restphone"

version := "0.4-SNAPSHOT"

scalaVersion := "2.10.0"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

publishMavenStyle := true

libraryDependencies ++= Seq(
  "com.restphone" %% "javasignatureparser" % "latest.snapshot",
  "com.restphone" %% "scalatestutilities" % "latest.snapshot" % "test",
  "org.ow2.asm" % "asm" % "4.1",
  "org.scala-lang" % "scala-actors" % "2.10.0",
  "org.scalatest" %% "scalatest" % "latest.snapshot" % "test",
  "org.scalaz" %% "scalaz-core" % "latest.snapshot"
)
