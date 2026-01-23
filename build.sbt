import org.typelevel.sbt.gha.JavaSpec.Distribution
import xerial.sbt.Sonatype.sonatypeCentralHost
import scoverage.ScoverageKeys._


disablePlugins(TypelevelMimaPlugin) // we use our own versioning for now via gitflow-packager

lazy val isCI = sys.env.get("CI").contains("true")

inThisBuild(List(
  organization := "co.blocke",
  homepage := Some(url("https://github.com/gzoller/ScalaJack")),
  licenses := List("MIT" -> url("https://opensource.org/licenses/MIT")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/gzoller/ScalaJack"),
      "scm:git:git@github.com:gzoller/ScalaJack.git"
    )
  ),
  developers := List(
    Developer(
      "gzoller",
      "Greg Zoller",
      "gzoller@blocke.co",
      url("http://www.blocke.co")
    )
  )
  //coverageMinimumStmtTotal    := 92,
  //coverageFailOnMinimum       := true
))

name := "scalajack"
ThisBuild / organization := "co.blocke"
ThisBuild / scalaVersion := "3.5.2"
ThisBuild / versionScheme := Some("semver-spec")
ThisBuild / githubWorkflowScalaVersions := Seq("3.5.2")
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec(Distribution.Temurin, "21"))
ThisBuild / githubWorkflowOSes := Seq("ubuntu-latest", "windows-latest")
ThisBuild / sonatypeCredentialHost := sonatypeCentralHost

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
    scalafmtOnCompile := !isCI,
    libraryDependencies ++= Seq(
      "co.blocke"            %% "scala-reflection"     % "2.0.16",
      "org.apache.commons"   % "commons-text"          % "1.13.1",
      "io.github.kitlangton" %% "neotype"              % "0.3.23",
      "com.fasterxml.woodstox" % "woodstox-core"       % "7.1.1",
      "org.scalatest"        %% "scalatest"            % "3.2.19" % Test,
      "org.json4s"           %% "json4s-core"          % "4.0.7" % Test,
      "org.json4s"           %% "json4s-native"        % "4.0.7" % Test
    )
  )

ThisBuild / githubWorkflowPublishTargetBranches := Seq(
  RefPredicate.Equals(Ref.Branch("main")),
  RefPredicate.StartsWith(Ref.Tag("v"))
)

ThisBuild / githubWorkflowJobSetup := Seq(
  WorkflowStep.Run(
    name = Some("Ignore line ending differences in git"),
    cond = Some("contains(runner.os, 'windows')"),
    commands = List("bash -c 'git config --global core.autocrlf false'")
  ),
  WorkflowStep.Use(
    UseRef.Public("actions", "setup-java", "v4"),
    params = Map(
      "distribution" -> "temurin",
      "java-version" -> "21"
    )
  ),
  WorkflowStep.Use(
    UseRef.Public("actions", "checkout", "v4")
  ),
  WorkflowStep.Use(
    UseRef.Public("coursier", "setup-action", "v1")
  ),
  WorkflowStep.Run(
    name = Some("Install sbt"),
    commands = List(
      "cs install sbt",
      "echo \"$HOME/.local/share/coursier/bin\" >> $GITHUB_PATH",
      "sbt sbtVersion"
    )
  )
)

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("ci-release"),
    env = Map(
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}",
      "CI_SNAPSHOT_RELEASE" -> "+publishSigned"
    )
  )
)

//==========================
// Settings
//==========================
lazy val settings = Seq(
  javacOptions ++= Seq("--release", "21"),
  scalacOptions ++= compilerOptions
)

lazy val compilerOptions = Seq(
  "-language:implicitConversions",
  // "-explain",'
  "-coverage-exclude-files",
  ".*SJConfig.*",
  "-coverage-exclude-classlikes",
  ".*internal.*",
  "-coverage-exclude-classlikes",
  ".*AnyWriter"
)

