import org.typelevel.sbt.gha.JavaSpec.Distribution.Zulu
import scoverage.ScoverageKeys._

lazy val isCI = sys.env.get("CI").contains("true")

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
  //coverageMinimumStmtTotal    := 92,
  //coverageFailOnMinimum       := true
))

name := "scalajack"
ThisBuild / organization := "co.blocke"
ThisBuild / scalaVersion := "3.5.2"
ThisBuild / githubWorkflowScalaVersions := Seq("3.5.2")

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
      "co.blocke"            %% "scala-reflection"     % "unique_08c7fd", //2.0.12",
      "org.apache.commons"   % "commons-text"          % "1.11.0",
      "io.github.kitlangton" %% "neotype"              % "0.0.9",
      "org.scalatest"        %% "scalatest"            % "3.2.17" % Test,
      "org.json4s"           %% "json4s-core"          % "4.0.6" % Test,
      "org.json4s"           %% "json4s-native"        % "4.0.6" % Test
    )
  )

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec(Zulu, "21"))
ThisBuild / githubWorkflowOSes := Seq("ubuntu-latest")
ThisBuild / githubWorkflowPublishTargetBranches := Seq(
  RefPredicate.Equals(Ref.Branch("main")),
  RefPredicate.StartsWith(Ref.Tag("v"))
)

ThisBuild / githubWorkflowPublish := Seq(
  // Step 1: Import GPG key into runner's GPG keyring
  WorkflowStep.Run(
    name = Some("Import GPG key"),
    commands = List(
      "echo \"$PGP_SECRET\" | base64 --decode | gpg --batch --import",
      "echo 'allow-loopback-pinentry' >> ~/.gnupg/gpg-agent.conf",
      "echo 'use-agent' >> ~/.gnupg/gpg.conf",
      "gpgconf --kill gpg-agent",
      "gpgconf --launch gpg-agent"
    ),
    env = Map(
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}"
    )
  ),

  // Step 2: Run ci-release as usual
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
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  scalacOptions ++= compilerOptions
)

lazy val compilerOptions = Seq(
  "-unchecked",
  "-feature",
  "-language:implicitConversions",
  "-deprecation",
  // "-explain",'
  "-coverage-exclude-files",
  ".*SJConfig.*",
  "-coverage-exclude-classlikes",
  ".*internal.*",
  "-coverage-exclude-classlikes",
  ".*AnyWriter",
  "-encoding",
  "utf8"
)

