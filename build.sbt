// use sbt deliver-local to create ivy.xml

name := "Jartender"

organization := "com.restphone"

version := "0.4-SNAPSHOT"

scalaVersion := "2.10.0"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

publishMavenStyle := true

libraryDependencies ++= Seq(
  "com.restphone" %% "javasignatureparser" % "0.4-SNAPSHOT",
  "org.scalaz" %% "scalaz-core" % "7.0.0-M7",
  "org.scala-lang" % "scala-actors" % "2.10.0",
  "org.ow2.asm" % "asm" % "4.1",
  "com.restphone" %% "scalatestutilities" % "0.4-SNAPSHOT" % "test",
  "org.scalatest" %% "scalatest" % "2.0.M5b" % "test"
)

