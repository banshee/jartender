// use sbt deliver-local to create ivy.xml

name := "Jartender"

organization := "com.restphone"

version := "0.5"

scalaVersion := "2.10.1"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

libraryDependencies ++= Seq(
  "com.restphone" %% "javasignatureparser" % "0.4",
  "org.scalaz" %% "scalaz-core" % "7.0.0-M7",
  "org.scala-lang" % "scala-actors" % "2.10.0",
  "org.ow2.asm" % "asm" % "4.1",
  "com.restphone" %% "scalatestutilities" % "0.4-SNAPSHOT" % "test",
  "org.scalatest" %% "scalatest" % "2.0.M5b" % "test"
)

pomExtra := (
  <url>https://github.com/banshee/JavaSignatureParser</url>
  <licenses>
    <license>
      <name>GPL</name>
      <url>https://github.com/banshee/JavaSignatureParser/blob/master/COPYING</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git:git@github.com:banshee/JavaSignatureParser.git</url>
    <connection>scm:git:git@github.com:banshee/JavaSignatureParser.git</connection>
  </scm>
  <developers>
    <developer>
      <id>jamesmoore</id>
      <name>James Moore</name>
      <organization>RESTPhone</organization>
      <organizationUrl>http://restphone.com</organizationUrl>
    </developer>
  </developers>)
