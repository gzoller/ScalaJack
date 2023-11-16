inThisBuild(List(
  organization := "co.blocke",
  homepage := Some(url("https://github.com/gzoller/ScalaJack")),
  licenses := List("MIT" -> url("https://opensource.org/licenses/MIT")),
  developers := List(
    Developer(
      "gzoller",
      "Greg Zoller",
      "gzoller@blocke.co",
      url("http://www.blocke.co")
    )
  )
))

name := "rnd"
ThisBuild / organization := "co.blocke"
ThisBuild / scalaVersion := "3.3.0"

lazy val root = project
  .in(file("."))
  .settings(settings)
  .settings(
    name := "serializer",
    Compile / packageBin / mappings += {
      (baseDirectory.value / "plugin.properties") -> "plugin.properties"
    },
    doc := null,  // disable dottydoc for now
    Compile / doc / sources := Seq(),
    //sources in (Compile, doc) := Seq(),
    Test / parallelExecution := false,
    libraryDependencies ++= Seq(
      "org.scalatest"        %% "scalatest"            % "3.2.17" % Test
    )
  )

//==========================
// Settings
//==========================
lazy val settings = Seq(
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  scalacOptions ++= compilerOptions
)

lazy val compilerOptions = Seq(
  "-unchecked",
  "-feature",
  "-language:implicitConversions",
  "-deprecation",
  // "-explain",
  "-encoding",
  "utf8"
)

