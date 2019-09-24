organization := "workshop"
name := "day1"

scalaVersion := "2.13.1"
scalacOptions += "-Ymacro-annotations"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"
libraryDependencies += "com.github.mvv.sash" %% "sash-cats" % "0.1-M5"
libraryDependencies += "org.specs2" %% "specs2-core" % "4.7.1" % Test
