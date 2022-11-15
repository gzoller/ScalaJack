import org.typelevel.sbt.gha.JavaSpec.Distribution.Zulu

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

name := "scalajack"
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec(Zulu, "13"))
ThisBuild / organization := "co.blocke"
val scala3 = "3.2.1"
val reflectionLibVersion = "1.1.11"

lazy val root = (project in file("."))
  .settings(settings)
  .settings(publish / skip := true)
  .settings(
    crossScalaVersions := Nil,
    doc := null,  // disable dottydoc for now
    Compile / doc / sources := Seq()
  )
  .aggregate(scalajack, scalajack_dynamo, scalajack_mongo)

lazy val scalajack = (project in file("core"))
  .settings(settings)
  .settings(
    name := "scalajack",
    doc := null,  // disable dottydoc for now
    Compile / doc / sources := Seq(),
    libraryDependencies ++= commonDependencies,
    Test / parallelExecution := false,
    
    // This messy stuff turns off reflection compiler plugin except for test case code
    addCompilerPlugin("co.blocke" %% "scala-reflection" % reflectionLibVersion),
    autoCompilerPlugins := false,
    ivyConfigurations += Configurations.CompilerPlugin,
    Test / scalacOptions ++= Classpaths.autoPlugins(update.value, Seq(), true)
  )

lazy val scalajack_dynamo = (project in file("dynamodb"))
  .settings(settings)
  .settings(
    doc := null,  // disable dottydoc for now
    Compile / doc / sources := Seq(),
    libraryDependencies ++= commonDependencies ++ Seq("com.amazonaws" % "aws-java-sdk-dynamodb" % "1.11.882" % Compile),
    Test / parallelExecution := false,
    
    // This messy stuff turns off reflection compiler plugin except for test case code
    addCompilerPlugin("co.blocke" %% "scala-reflection" % reflectionLibVersion),
    autoCompilerPlugins := false,
    ivyConfigurations += Configurations.CompilerPlugin,
    Test / scalacOptions ++= Classpaths.autoPlugins(update.value, Seq(), true)
  ).dependsOn(scalajack)

lazy val scalajack_mongo = (project in file("mongo"))
  .settings(settings)
  .settings(
    doc := null,  // disable dottydoc for now
    Compile / doc / sources := Seq(),
    libraryDependencies ++= commonDependencies ++ Seq("org.mongodb" % "mongo-java-driver" % "3.12.7"),
    Test / parallelExecution := false,
    
    // This messy stuff turns off reflection compiler plugin except for test case code
    addCompilerPlugin("co.blocke" %% "scala-reflection" % reflectionLibVersion),
    autoCompilerPlugins := false,
    ivyConfigurations += Configurations.CompilerPlugin,
    Test / scalacOptions ++= Classpaths.autoPlugins(update.value, Seq(), true)
  ).dependsOn(scalajack)

lazy val commonDependencies = Seq(
  "co.blocke"      %% "scala-reflection"      % reflectionLibVersion,
  "commons-codec"  %  "commons-codec"         % "1.15",
  "org.json4s"     %  "json4s-core_2.13"      % "3.6.11",
  "org.snakeyaml"  %  "snakeyaml-engine"      % "2.3",
  "org.json4s"     %  "json4s-native_2.13"    % "3.6.11" % Test,
  "org.scalameta"  %% "munit"                 % "0.7.25" % Test
)

//==========================
// Settings
//==========================
lazy val compilerOptions = Seq(
  "-unchecked",
  "-feature",
  "-language:implicitConversions",
  "-deprecation",
  "-encoding",
  "utf8"
)

lazy val settings = Seq(
  scalacOptions ++= compilerOptions,
  scalaVersion := scala3,
  testFrameworks += new TestFramework("munit.Framework")
)

