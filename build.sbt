lazy val root = (project in file("."))
.settings(
  organization := "ciukstar.edu",
  name := "fpinscala",
  version := "0.1.0",
  scalaVersion := "2.11.8",
  libraryDependencies ++= Seq(
    "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
    "com.typesafe.akka" %% "akka-actor" % "2.4.16",
    "org.scalactic" %% "scalactic" % "3.0.1",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  )
)
