import sbt._
import sbt.Keys._
import scoverage.ScoverageKeys._

def compile(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "compile")
def test(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "test")

val mongo_java = "org.mongodb" % "mongodb-driver-sync" % "3.10.2"
val scalatest = "org.scalatest" %% "scalatest" % "3.1.0-RC3"
val slf4j_simple = "org.slf4j" % "slf4j-simple" % "1.7.26"
val dynamo = "com.amazonaws" % "aws-java-sdk-dynamodb" % "1.11.538"
val json4s = "org.json4s" %% "json4s-core" % "3.6.6"
val json4sNative = "org.json4s" %% "json4s-native" % "3.6.6"
val cats = "org.typelevel" %% "cats-core" % "2.0.0"
val snakeyaml = "org.snakeyaml" % "snakeyaml-engine" % "2.0"

lazy val basicSettings = Seq(
  resolvers += Resolver.jcenterRepo,
  organization := "co.blocke",
  startYear := Some(2015),
  publishArtifact in (Compile, packageDoc) := false, // disable scaladoc due to bug handling annotations
  scalaVersion := "2.13.1",
  coverageMinimum := 98, // really this should be 96% but mongo isn't quite up to that yet
  coverageFailOnMinimum := true,
  parallelExecution in ThisBuild := false,
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-Xlint",
    "-encoding",
    "UTF8",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:existentials",
    "-language:reflectiveCalls",
    "-unchecked"
  ),
  testOptions in Test += Tests.Argument("-oDF")
)

// configure prompt to show current project
shellPrompt := { s =>
  Project.extract(s).currentProject.id + " > "
}

lazy val root = (project in file("."))
  .settings(basicSettings: _*)
  .settings(publishArtifact := false)
  .settings(publish := {})
  .settings(crossScalaVersions := Nil)
  .aggregate(scalajack, scalajack_mongo, scalajack_dynamo) //, scalajack_benchmarks)

val pubSettings = Seq(
  publishMavenStyle := true,
  bintrayOrganization := Some("blocke"),
  bintrayReleaseOnPublish in ThisBuild := false,
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  bintrayRepository := "releases",
  bintrayPackageLabels := Seq("scala", "json", "scalajack")
)

lazy val scalajack = project
  .in(file("core"))
  .settings(basicSettings)
  .settings(pubSettings: _*)
  .settings(
    libraryDependencies ++=
      Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value) ++
        Seq("org.apache.commons" % "commons-text" % "1.6") ++
        Seq("commons-codec" % "commons-codec" % "1.12") ++
        Seq(json4s, cats, snakeyaml) ++
        test(scalatest) ++
        test(json4sNative)
  )

lazy val scalajack_mongo = project
  .in(file("mongo"))
  .settings(basicSettings)
  .settings(pubSettings: _*)
  .settings(
    libraryDependencies ++=
      compile(mongo_java) ++
        test(scalatest, slf4j_simple) ++
        test(json4sNative)
  )
  .dependsOn(scalajack)

lazy val scalajack_dynamo = project
  .in(file("dynamodb"))
  .settings(basicSettings)
  .settings(pubSettings: _*)
  .settings(
    libraryDependencies ++=
      compile(dynamo) ++
        test(scalatest) ++
        test(json4sNative)
  )
  .dependsOn(scalajack)

lazy val scalajack_benchmarks = project
  .in(file("benchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(basicSettings: _*)
  .settings(pubSettings: _*)
  .settings(
    libraryDependencies ++=
      test(scalatest, slf4j_simple) ++
        List(
          "net.liftweb" %% "lift-json" % "3.4.0",
          json4sNative,
          "co.blocke" %% "scalajack" % "6.0.4",
          "io.circe" %% "circe-core" % "0.12.3",
          "io.circe" %% "circe-generic" % "0.12.3",
          "io.circe" %% "circe-parser" % "0.12.3"
        ) :+ json4s
  )
  .dependsOn(scalajack)
