name := "api"

scalaVersion := "2.11.8"

val scalazVersion = "7.2.7"
val http4sVersion = "0.15.0a-SNAPSHOT"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion
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

enablePlugins(JavaAppPackaging)
