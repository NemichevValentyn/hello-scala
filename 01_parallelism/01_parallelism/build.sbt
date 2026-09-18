scalaVersion := "3.3.3"
name := "01_parallelism"
version := "1.0"

// Бібліотека для паралельних колекцій (починаючи зі Scala 2.13 вони винесені з stdlib)
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

// Бібліотека для Unit-тестування
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % Test
