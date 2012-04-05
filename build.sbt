name := "wordpath"

version := "0.0.1"

scalaVersion := "2.9.1"

mainClass in (Compile, packageBin) := Some("Fast") 

resolvers ++= Seq(
    "external" at "https://nexus.gilt.com/nexus/repositories/content/repositories/external",
    "twitter" at "http://maven.twttr.com/")

libraryDependencies ++= Seq("com.typesafe.akka" % "akka-actor" % "2.0")

scalacOptions ++= Seq("-deprecation", "-unchecked", "-optimize")

seq(com.twitter.sbt.StandardProject.newSettings: _*)

