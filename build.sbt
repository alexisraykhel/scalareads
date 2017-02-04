name := "Scalareads"
organization := "io.github.alexisraykhel"
version := "1.0.0"
scalaVersion := "2.11.8"

val http4sVersion = "0.15.2"

libraryDependencies ++=
  List(
    "io.argonaut" %% "argonaut" % "6.1",
    "org.scalaz" %% "scalaz-core" % "7.2.4",
    "org.scalaz" %% "scalaz-concurrent" % "7.2.4",
    "org.scala-lang.modules" %% "scala-xml" % "1.0.2",
    "org.http4s" %% "http4s-dsl" % http4sVersion,
    "org.http4s" %% "http4s-blaze-server" % http4sVersion,
    "org.http4s" %% "http4s-blaze-client" % http4sVersion)
