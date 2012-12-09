// use sbt deliver-local to create ivy.xml

name := "Jartender"

organization := "com.restphone"

version := "0.2-SNAPSHOT"

scalaVersion := "2.10.0-SNAPSHOT"

// Compile for these Scala versions
crossScalaVersions := Seq("2.10.0-RC1")

publishMavenStyle := true

externalResolvers := Seq(
  "RESTPhone Nexus" at "http://git:8081/nexus/content/groups/public",
  Resolver.sonatypeRepo("snapshots")
)

transitiveClassifiers := Seq("sources")

libraryDependencies ++= Seq(
  "com.restphone" % "javasignatureparser_2.10" % "0.3-SNAPSHOT",
  "org.scala-lang" % "scala-actors" % "2.10.0-RC3",
  "org.ow2.asm" % "asm" % "4.1",
  "org.scalatest" % "scalatest_2.10.0-RC3" % "2.0.M5-B1" % "test"
)

defaultConfigurationMapping in GlobalScope := "compile->default;runtime->default;test->default;provided->default"

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
