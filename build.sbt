val junit = "junit" % "junit" % "4.12" % "test"
val scalatest = "org.scalatest" %% "scalatest" % "3.0.3" % "test"

lazy val commonSettings = Seq(
  organization := "org.monkeynuthead",
  version := "0.1.0",
  scalaVersion := "2.12.2",
  sbtVersion := "0.13.15",
  scalacOptions ++= Seq("-feature","-unchecked","-deprecation")
)

lazy val `basic-sbt` = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "basic-sbt",
    libraryDependencies ++= Seq(
      junit, scalatest
    ),
    initialCommands in console :=
      """
      """.stripMargin,
    cleanupCommands in console :=
      """
      """.stripMargin
  )
