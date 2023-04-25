ThisBuild / scalaVersion := "2.13.8"

libraryDependencies +=
"org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

// Set the Java version for Java compilation
ThisBuild / javacOptions ++= Seq("--release", "17")

// Set the Java version for Scala compilation
ThisBuild / scalacOptions ++= Seq("-release", "17")

// Set the Java version for running
ThisBuild / Compile / run / fork := true
ThisBuild / Compile / run / javaOptions ++= Seq("--enable-preview", "--illegal-access=permit")

// Set the Java version for testing
ThisBuild / Test / fork := true
ThisBuild / Test / javaOptions ++= Seq("--enable-preview", "--illegal-access=permit")
