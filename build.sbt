scalaVersion := "2.13.3"

organization := "com.redbook"
version := "0.1"

lazy val root = (project in file("."))
  .settings(
    name := "redbook-exercises",
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2-core" % dependencies("specs2") % "test"
    ),
    scalacOptions in Test ++= Seq("-Yrangepos")
  )

val dependencies = Map(
  "specs2" -> "4.10.0"
)
