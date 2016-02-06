name := "ScalaBigraph"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  DefaultMavenRepository, 
  "My bitbucket maven releases repo" at "https://bitbucket.org/eloipereira/maven-repo-releases/raw/master",  
  "My bitbucket maven snapshots repo" at "https://bitbucket.org/eloipereira/maven-repo-snapshots/raw/master",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)


libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.4.1"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.0"

scalacOptions += "-deprecation"


