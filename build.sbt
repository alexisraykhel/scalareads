lazy val root = (project in file(".")).
  settings(
    name := "goodreads",
    version := "1.0",
    scalaVersion := "2.11.8"
  )

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.4"
