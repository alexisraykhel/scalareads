name := "Scalareads"
organization := "io.github.alexisraykhel"
version := "1.0.0"
scalaVersion := "2.11.8"

libraryDependencies ++=
  List(
    "org.scalaz" %% "scalaz-core" % "7.2.4",
    "org.scalaz" %% "scalaz-concurrent" % "7.2.4",
//    "io.argonaut" %% "argonaut" % "6.1",
    "org.scala-lang.modules" %% "scala-xml" % "1.0.2")