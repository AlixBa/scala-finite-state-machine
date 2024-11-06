ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    name := "scala-finite-state-machine",
    libraryDependencies += "org.typelevel" %% "otel4s-core" % "0.10.0",
    libraryDependencies += "org.typelevel" %% "log4cats-core" % "2.7.0" % Compile,
    libraryDependencies += "org.scalameta" %% "munit" % "latest.integration" % Test
  )
