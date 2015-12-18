val scalazCore = "org.scalaz" %% "scalaz-core" % "7.2.0"
val junit = "junit" % "junit" % "4.12" % "test"
val scalatest = "org.scalatest" %% "scalatest" % "2.2.5" % "test"

lazy val commonSettings = Seq(
  organization := "org.monkeynuthead",
  version := "0.1.0",
  scalaVersion := "2.11.7",
  sbtVersion := "0.13.9",
  scalacOptions ++= Seq("-feature","-unchecked","-deprecation")
)

lazy val `func-programming-in-scala` = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "func-programming-in-scala",
    libraryDependencies ++= Seq(
      scalazCore,
      junit, scalatest
    ),
    initialCommands in console :=
      """import java.util.concurrent._
        |import com.monkeynuthead.func_prog_in_scala.parallelism.Part2._
        |import com.monkeynuthead.func_prog_in_scala.parallelism.Actor
        |import Par._
        |import com.monkeynuthead.func_prog_in_scala.props._
        |val S = Executors.newFixedThreadPool(2)
        |
      """.stripMargin,
    cleanupCommands in console :=
      """
        |S.shutdownNow
        |
      """.stripMargin
  )
