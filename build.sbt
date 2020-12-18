lazy val root = (project in file("."))
  .settings(name := "Advent of Code Scala")
  .settings(moduleName := "advent-of-code-scala")
  .settings(version := "0.1")
  .settings(scalaVersion := "2.13.1")
  .settings(libraryDependencies ++= dependencies)

lazy val dependencies = Seq("org.scalatest" %% "scalatest"   % "3.1.0"  % "test")
libraryDependencies += "org.tpolecat" %% "atto-core"    % "0.7.0"
libraryDependencies += "org.tpolecat" %% "atto-refined" % "0.7.0"


scalacOptions ++= Seq("-deprecation", "-feature")
