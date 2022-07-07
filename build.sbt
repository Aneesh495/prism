name := "prism"
version := "0.1.0"
scalaVersion := "3.4.2"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-encoding", "utf8",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials"
)

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "1.0.0" % Test
)

testFrameworks += new TestFramework("munit.Framework")

fork := true
