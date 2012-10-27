// use sbt deliver-local to create ivy.xml

name := "AsmSample"

organization := "com.restphone"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.0-SNAPSHOT"

// Compile for these Scala versions
crossScalaVersions := Seq("2.9.1", "2.9.2", "2.10.0-RC1")

publishMavenStyle := true

externalResolvers := Seq(
  "RESTPhone Nexus" at "http://git:8081/nexus/content/groups/public"
)

transitiveClassifiers := Seq("sources")

EclipseKeys.withSource := true

libraryDependencies ++= Seq(
  "org.scalaz" % "scalaz-concurrent_2.10.0-M7" % "7.0.0-M3",
  "com.google.guava" % "guava" % "13.0.1",
  "org.ow2.asm" % "asm" % "4.0",
  "org.ow2.asm" % "asm" % "4.0" % "sources"
)

libraryDependencies ++= Seq(
  "com.restphone" % "scalatest" %  "2.0.M5" % "test",
  "junit" % "junit" % "4.8.1" % "test",
  "org.scalacheck" % "scalacheck_2.10" % "1.10.1-SNAPSHOT" % "test"
)

publishTo <<= version { (v: String) =>
  val nexus = "http://git:8081/nexus/content/repositories/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "snapshots")
  else
    Some("releases" at nexus + "releases")
}

credentials += Credentials("Sonatype Nexus Repository Manager", 
                           "git", 
                           "deployment",
                           "deploy")
