name := "Underscore"

version := "0.1"

scalaVersion := "2.13.5"

libraryDependencies += "org.scalameta" %% "munit" % "0.7.25" % Test
testFrameworks += new TestFramework("munit.Framework")
