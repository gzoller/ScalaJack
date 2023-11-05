ThisBuild / organization := "co.blocke"

val compilerOptions = Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-unchecked",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Xfuture"
)

val circeVersion = "0.15.0-M1"
val scalaTestVersion = "3.2.11"
ThisBuild / scalaVersion := "3.3.0"

def priorTo2_13(scalaVersion: String): Boolean =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, minor)) if minor < 13 => true
    case _                              => false
  }

val baseSettings = Seq(
  scalacOptions ++= compilerOptions
)

lazy val benchmark = project
  .in(file("."))
  .settings(baseSettings ++ noPublishSettings)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion),
    libraryDependencies ++= Seq(
      "org.playframework" %% "play-json" % "3.0.1",
      "io.argonaut" %% "argonaut" % "6.3.9",
      "co.blocke" %% "scalajack" % "826a30_unknown",
      "co.blocke" %% "scala-reflection" % "sj_fixes_edbef8",
      "dev.zio" %% "zio-json" % "0.6.1",
      // "io.circe" %% "circe-derivation" % "0.15.0-M1",
      // "io.circe" %% "circe-jackson29" % "0.14.0",
      // "org.json4s" %% "json4s-jackson" % "4.0.4",
      // "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.13.17",
      // "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.13.17",
      "org.scalatest" %% "scalatest" % scalaTestVersion % Test
    )
  )
  .enablePlugins(JmhPlugin)

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
)
