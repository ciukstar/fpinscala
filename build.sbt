lazy val root = (project in file("."))
.settings(
  organization := "ciukstar.edu",
  name := "fpinscala",
  version := "0.1.0",
  scalaVersion := "2.11.8",
  libraryDependencies ++= Seq(
    "org.scalactic" %% "scalactic" % "3.0.1",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  )
)
