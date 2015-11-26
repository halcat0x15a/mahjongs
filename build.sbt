lazy val commonSettings = Seq(
  organization := "org.halcat",
  version := "0.1.0",
  scalaVersion := "2.11.7",
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.4" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
  ),
  scalacOptions += "-deprecation"
)

lazy val root = project in file(".") aggregate (solver, recognizer)

lazy val solver = project in file("solver") settings (commonSettings: _*)

lazy val recognizer = project in file("recognizer") settings (commonSettings: _*)

lazy val server = project in file("server") settings (commonSettings: _*) settings (
  libraryDependencies += "com.typesafe.akka" %% "akka-http-spray-json-experimental" % "1.0",
  fork in run := true,
  connectInput := true
) dependsOn (solver, recognizer)

lazy val app = project in file("app") settings (commonSettings: _*) settings (android.Plugin.androidBuild: _*) settings (
  platformTarget in Android := "android-19",
  javacOptions ++= Seq("-source", "1.7", "-target", "1.7")
)
