import sbt._
import sbt.Keys._
import pl.project13.scala.sbt.JmhPlugin
import com.typesafe.sbt.SbtScalariform._
import scalariform.formatter.preferences._
import scoverage.ScoverageKeys._

val resolutionRepos = Seq(
  "Typesafe Repo"         at "http://repo.typesafe.com/typesafe/releases/",
  "Typesafe Snapshots"    at "http://repo.typesafe.com/typesafe/snapshots/",
  "OSS"                   at "http://oss.sonatype.org/content/repositories/releases",
  "OSS Staging"           at "http://oss.sonatype.org/content/repositories/staging",
  "PhantomMvn"            at "http://maven.websudos.co.uk/ext-release-local",
  "Mvn"                   at "http://mvnrepository.com/artifact"  // for commons_exec
)

def compile   (deps: ModuleID*): Seq[ModuleID] = deps map (_ % "compile")
def test      (deps: ModuleID*): Seq[ModuleID] = deps map (_ % "test")

val mongo_scala     = "org.mongodb.scala"       %% "mongo-scala-driver"   % "2.1.0"
val scalatest       = "org.scalatest"           %% "scalatest"            % "3.0.1"
val slf4j_simple    = "org.slf4j"               % "slf4j-simple"          % "1.7.25"
val dynamo          = "com.amazonaws"           % "aws-java-sdk-dynamodb" % "1.11.43"

def scalacOptionsVersion(scalaVersion: String) = {
  val xver =  CrossVersion.partialVersion(scalaVersion) match {
         case Some((2, scalaMajor)) if scalaMajor == 11 => Seq("-language:existentials")
         case _ => Seq("-language:_")
      }

  Seq(
    "-feature", 
    "-deprecation", 
    "-Xlint", 
    "-encoding", "UTF8", 
    "-language:higherKinds",
    // "-language:existentials",
    "-language:implicitConversions",
    "-unchecked"
    //"-Xfatal-warnings"
  ) ++ xver
}

lazy val basicSettings = Seq(
  organization                := "co.blocke",
  startYear                   := Some(2015),
  crossScalaVersions          := Seq("2.11.12", "2.12.6"),
  publishArtifact in (Compile, packageDoc) := false,  // disable scaladoc due to bug handling annotations
  scalaVersion                := "2.12.3",
//  resolvers                   ++= resolutionRepos,
  coverageMinimum             := 92,  // really this should be 96% but mongo isn't quite up to that yet
  coverageFailOnMinimum       := true,
  ScalariformKeys.preferences := ScalariformKeys.preferences.value
    .setPreference(AlignArguments, true)
    .setPreference(AlignParameters, true)
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(DoubleIndentConstructorArguments, true),
  // .setPreference(PreserveDanglingCloseParenthesis, true),
  scalacOptions := scalacOptionsVersion(scalaVersion.value),
  testOptions in Test += Tests.Argument("-oDF")
)

// configure prompt to show current project
shellPrompt := { s => Project.extract(s).currentProject.id + " > " }

lazy val root = (project in file("."))
  .settings(basicSettings: _*)
  .settings(publishArtifact := false)
  .settings(publish := { })
  .aggregate(scalajack)//, scalajack_dynamodb, scalajack_mongo)
// For gpg might need this too:
//publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))

val pubSettings = Seq (
  publishMavenStyle := true,
  bintrayOrganization := Some("blocke"),
  bintrayReleaseOnPublish in ThisBuild := false,
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  bintrayRepository := "releases",
  bintrayPackageLabels := Seq("scala", "json", "scalajack")
)

lazy val scalajack = project.in(file("core"))
  .settings(basicSettings: _*)
  .settings(pubSettings: _*)
  .settings(libraryDependencies ++=
    Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value) ++
    Seq("org.scala-lang" % "scala-compiler" % scalaVersion.value) ++
    Seq("org.apache.commons" % "commons-text" % "1.4") ++
    Seq("org.json4s" %% "json4s-core" % "3.5.4") ++
    Seq("org.json4s" %% "json4s-native" % "3.5.4" % Test) ++
      test(scalatest)
  )

//lazy val scalajack_dynamodb = project.in(file("dynamodb"))
//  .settings(basicSettings: _*)
//  .settings(pubSettings: _*)
//  .settings(libraryDependencies ++=
//    compile( dynamo ) ++
//      test( scalatest, slf4j_simple )
//  ).dependsOn( scalajack )
//
//lazy val scalajack_mongo = project.in(file("mongo"))
//  .settings(basicSettings: _*)
//  .settings(pubSettings: _*)
//  .settings(libraryDependencies ++=
//    compile( mongo_scala ) ++
//      test( scalatest, slf4j_simple )
//  ).dependsOn( scalajack )


//lazy val scalajack_benchmarks = project.in(file("benchmarks"))
//  .enablePlugins(JmhPlugin)
//  .settings(basicSettings: _*)
//  .settings(pubSettings: _*)
//  .settings(libraryDependencies ++=
//    compile( mongo_scala ) ++
//      test( scalatest, slf4j_simple ) ++
//      List(
//        "com.typesafe.play" %% "play-json" % "2.6.7",
//        "org.json4s" %% "json4s-native" % "3.5.4",
//        "net.liftweb" %% "lift-json" % "3.3.0",
//        "io.spray" %% "spray-json" % "1.3.2"
//      )
//  ).dependsOn( scalajack )
