import sbt._
import sbt.Keys._
import scoverage.ScoverageKeys._

def compile(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "compile")
def test(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "test")

val mongo_java = "org.mongodb" % "mongodb-driver-sync" % "3.10.2"
val scalatest = "org.scalatest" %% "scalatest" % "3.1.0-SNAP13"
val slf4j_simple = "org.slf4j" % "slf4j-simple" % "1.7.26"
val dynamo = "com.amazonaws" % "aws-java-sdk-dynamodb" % "1.11.538"
val json4s = "org.json4s" %% "json4s-core" % "3.6.6"

def scalacOptionsVersion(scalaVersion: String) = {
  val xver = CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, scalaMajor)) if scalaMajor == 11 =>
      Seq("-language:existentials")
    case _ => Seq("-language:_")
  }

  Seq(
    "-feature",
    "-deprecation",
    "-Xlint",
    "-encoding",
    "UTF8",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked"
  ) ++ xver
}

lazy val crossVersions = crossScalaVersions := Seq("2.12.8", "2.13.0")

lazy val basicSettings = Seq(
  resolvers += Resolver.jcenterRepo,
  organization := "co.blocke",
  startYear := Some(2015),
  publishArtifact in (Compile, packageDoc) := false, // disable scaladoc due to bug handling annotations
  scalaVersion := "2.12.8",
  coverageMinimum := 92, // really this should be 96% but mongo isn't quite up to that yet
  coverageFailOnMinimum := true,
//  parallelExecution in ThisBuild := false,
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  scalacOptions ++= Seq("-target:jvm-1.8", "-language:_"),
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
  .aggregate(scalajack, scalajack_benchmarks) //, scalajack_mongo, scalajack_dynamo) //, scalajack_benchmarks)
// For gpg might need this too:
//publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))

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
  .settings(basicSettings ++ crossVersions: _*)
  .settings(pubSettings: _*)
  .settings(
    libraryDependencies ++=
      Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value) ++
        Seq("org.apache.commons" % "commons-text" % "1.6") ++
        Seq("commons-codec" % "commons-codec" % "1.12") ++
        Seq(json4s) ++
        test(scalatest) ++
        test("org.json4s" %% "json4s-native" % "3.6.6")
  )

/*
lazy val scalajack_mongo = project
  .in(file("mongo"))
  .settings(basicSettings ++ crossVersions: _*)
  .settings(pubSettings: _*)
  .settings(
    libraryDependencies ++=
      compile(mongo_java) ++
        test(scalatest, slf4j_simple)
  )
  .dependsOn(scalajack)

lazy val scalajack_dynamo = project
  .in(file("dynamodb"))
  .settings(basicSettings ++ crossVersions: _*)
  .settings(pubSettings: _*)
  .settings(
    libraryDependencies ++=
      compile(dynamo) ++
        test(scalatest)
  )
  .dependsOn(scalajack)
 */

lazy val scalajack_benchmarks = project
  .in(file("benchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(basicSettings: _*)
  .settings(pubSettings: _*)
  .settings(
    libraryDependencies ++=
//      compile(mongo_scala) ++
      test(scalatest, slf4j_simple) ++
        List(
          "net.liftweb" %% "lift-json" % "3.4.0",
          "org.json4s" %% "json4s-native" % "3.6.6",
          "co.blocke" %% "scalajack" % "6.0.4",
          "io.circe" %% "circe-core" % "0.11.1",
          "io.circe" %% "circe-generic" % "0.11.1",
          "io.circe" %% "circe-parser" % "0.11.1"
        ) :+ json4s
  )
  .dependsOn(scalajack)
