name := "ScalaBigraph"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  DefaultMavenRepository,
  "My bitbucket maven releases repo" at "https://bitbucket.org/eloipereira/maven-repo-releases/raw/master",
  "My bitbucket maven snapshots repo" at "https://bitbucket.org/eloipereira/maven-repo-snapshots/raw/master",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.2",
  "org.scalacheck" %% "scalacheck" % "1.13.0" % "test"
)

scalacOptions += "-deprecation"

initialCommands in console := "import bigraph._, PlaceGraph._"
