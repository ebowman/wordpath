name := "wordpath"

version := "0.0.1"

scalaVersion := "2.9.1"

resolvers ++= Seq("external" at "https://nexus.gilt.com/nexus/repositories/content/repositories/external")

libraryDependencies ++= Seq("com.typesafe.akka" % "akka-actor" % "2.0")

scalacOptions ++= Seq("-deprecation", "-unchecked", "-optimize")
