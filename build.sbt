name := "scalajack"
organization in ThisBuild := "co.blocke"
val scala3 = "3.0.0-M3"
scalaVersion := scala3
val reflectionLibVersion = "1.0.0-M3"

lazy val root = (project in file("."))
  .settings(settings)
  .settings(publishArtifact := false)
  .settings(publish := {})
  .settings(crossScalaVersions := Nil)
  .aggregate(scalajack, scalajack_dynamo, scalajack_mongo)

lazy val scalajack = (project in file("core"))
  .settings(settings)
  .settings(
    name := "scalajack",
    doc := null,  // disable dottydoc for now
    sources in (Compile, doc) := Seq(),
    libraryDependencies ++= commonDependencies,
    Test / parallelExecution := false,
    
    // This messy stuff turns off reflection compiler plugin except for test case code
    addCompilerPlugin("co.blocke" %% "scala-reflection" % reflectionLibVersion),
    autoCompilerPlugins := false,
    ivyConfigurations += Configurations.CompilerPlugin,
    scalacOptions in Test ++= Classpaths.autoPlugins(update.value, Seq(), true)
  )

lazy val scalajack_dynamo = (project in file("dynamodb"))
  .settings(settings)
  .settings(
    doc := null,  // disable dottydoc for now
    sources in (Compile, doc) := Seq(),
    libraryDependencies ++= commonDependencies ++ Seq("com.amazonaws" % "aws-java-sdk-dynamodb" % "1.11.882" % Compile),
    Test / parallelExecution := false,
    
    // This messy stuff turns off reflection compiler plugin except for test case code
    addCompilerPlugin("co.blocke" %% "scala-reflection" % reflectionLibVersion),
    autoCompilerPlugins := false,
    ivyConfigurations += Configurations.CompilerPlugin,
    scalacOptions in Test ++= Classpaths.autoPlugins(update.value, Seq(), true)
  ).dependsOn(scalajack)

lazy val scalajack_mongo = (project in file("mongo"))
  .settings(settings)
  .settings(
    doc := null,  // disable dottydoc for now
    sources in (Compile, doc) := Seq(),
    libraryDependencies ++= commonDependencies ++ Seq("org.mongodb" % "mongo-java-driver" % "3.12.7"),
    Test / parallelExecution := false,
    
    // This messy stuff turns off reflection compiler plugin except for test case code
    addCompilerPlugin("co.blocke" %% "scala-reflection" % reflectionLibVersion),
    autoCompilerPlugins := false,
    ivyConfigurations += Configurations.CompilerPlugin,
    scalacOptions in Test ++= Classpaths.autoPlugins(update.value, Seq(), true)
  ).dependsOn(scalajack)

lazy val commonDependencies = Seq(
  "co.blocke"      %% "scala-reflection"      % reflectionLibVersion,
  "commons-codec"  %  "commons-codec"         % "1.12",
  "org.json4s"     %  "json4s-core_2.13"      % "3.6.6",
  "org.snakeyaml"  %  "snakeyaml-engine"      % "2.0",
  "org.json4s"     %  "json4s-native_2.13"    % "3.6.6" % Test,
  "org.scalameta"  %% "munit"                 % "0.7.20" % Test
)

//==========================
// Settings
//==========================
lazy val settings = 
  commonSettings ++
  publishSettings

lazy val compilerOptions = Seq(
  "-unchecked",
  "-feature",
  "-language:implicitConversions",
  "-deprecation",
  "-encoding",
  "utf8"
)

lazy val commonSettings = Seq(
  scalacOptions ++= compilerOptions,
  scalaVersion := scala3,
  resolvers += "co.blocke releases buildResolver" at "https://dl.bintray.com/blocke/releases",
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  bintrayOrganization := Some("blocke"),
  bintrayReleaseOnPublish in ThisBuild := true,
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  bintrayRepository := "releases",
  bintrayPackageLabels := Seq("scala", "dotty", "reflection")
)

