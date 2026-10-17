ThisBuild / scalaVersion := "3.7.3"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "cashback"

lazy val root = (project in file("."))
  .settings(
    name := "bank-cashback",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.19" % Test
    )
  )
