scalaVersion := "2.10.0-SNAPSHOT"

resolvers += Resolver.sonatypeRepo("snapshots")

unmanagedResourceDirectories in Compile <+= baseDirectory(_ / "libs")
