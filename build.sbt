name := "api"

scalaVersion := "2.11.8"

val scalazVersion = "7.2.7"
val http4sVersion = "0.15.0a-SNAPSHOT"
val circeVersion = "0.6.1"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-concurrent" % scalazVersion,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.pauldijou" %% "jwt-circe" % "0.9.2",
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "io.circe" %% "circe-optics" % circeVersion,
  "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
  "org.slf4j" % "slf4j-simple" % "1.7.21"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-Xlint",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-Ywarn-dead-code",
  "-feature",
  "-language:_"
)

resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
enablePlugins(JavaAppPackaging)
