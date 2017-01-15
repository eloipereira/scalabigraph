name := "ScalaBigraph"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.1"

scalacOptions += "-target:jvm-1.8"

resolvers ++= Seq(
  DefaultMavenRepository,
  "My bitbucket maven releases repo" at "https://bitbucket.org/eloipereira/maven-repo-releases/raw/master",
  "My bitbucket maven snapshots repo" at "https://bitbucket.org/eloipereira/maven-repo-snapshots/raw/master",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.8",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

scalacOptions += "-deprecation"

initialCommands in console := "import bigraph._, placeGraph._, linkGraph._, PlaceGraph._, LinkGraph._"

//testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "3")
