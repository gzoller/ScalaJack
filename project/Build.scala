import sbt._
import sbt.Keys._
import bintray.BintrayKeys._
import pl.project13.scala.sbt.JmhPlugin
import com.typesafe.sbt.SbtScalariform._
import scalariform.formatter.preferences._

import scala.Some

object Build extends Build {

	import Dependencies._

	val scalaVer = "2.11.7"

	lazy val basicSettings = Seq(
		organization 				:= "co.blocke",
		startYear 					:= Some(2015),
		scalaVersion 				:= scalaVer,
		resolvers					++= Dependencies.resolutionRepos,
		ScalariformKeys.preferences := ScalariformKeys.preferences.value
			.setPreference(AlignArguments, true)
			.setPreference(AlignParameters, true)
			.setPreference(AlignSingleLineCaseStatements, true)
			.setPreference(DoubleIndentClassDeclaration, true)
			.setPreference(PreserveDanglingCloseParenthesis, true),
		scalacOptions				:= Seq("-feature", "-deprecation", "-Xlint", "-encoding", "UTF8", "-unchecked", "-Xfatal-warnings"),
		testOptions in Test += Tests.Argument("-oDF")
	)

	// configure prompt to show current project
	override lazy val settings = super.settings :+ {
		shellPrompt := { s => Project.extract(s).currentProject.id + " > " }
	}

	lazy val root = (project in file("."))
		.settings(basicSettings: _*)
		.settings(publishArtifact := false)
		.settings(publish := { })
		.aggregate(scalajack, dummy)//, scalajack_mongo)
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
			compile(joda, joda_convert, scala_reflect, asm) ++
			test(scalatest)
		)

	lazy val dummy = project.in(file("dummy"))
		.settings(basicSettings: _*)
		.settings(pubSettings: _*)
		.settings(libraryDependencies ++=
			test( scalatest )
		)

	lazy val scalajack_mongo = project.in(file("mongo"))
		.settings(basicSettings: _*)
		.settings(pubSettings: _*)
		.settings(libraryDependencies ++=
			compile( mongo_scala ) ++
			test( scalatest, slf4j_simple )
		).dependsOn( scalajack )


	lazy val scalajack_benchmarks = project.in(file("benchmarks"))
		.enablePlugins(JmhPlugin)
		.settings(basicSettings: _*)
		.settings(pubSettings: _*)
		.settings(libraryDependencies ++=
			compile( mongo_scala ) ++
				test( scalatest, slf4j_simple )
		).dependsOn( scalajack )
}

object Dependencies {
	val resolutionRepos = Seq(
		"Typesafe Repo"  		at "http://repo.typesafe.com/typesafe/releases/",
		"Typesafe Snapshots"	at "http://repo.typesafe.com/typesafe/snapshots/",
		"OSS"					at "http://oss.sonatype.org/content/repositories/releases",
		"OSS Staging"			at "http://oss.sonatype.org/content/repositories/staging",
		"PhantomMvn"			at "http://maven.websudos.co.uk/ext-release-local",
		"Mvn" 					at "http://mvnrepository.com/artifact"  // for commons_exec
	)

	def compile   (deps: ModuleID*): Seq[ModuleID] = deps map (_ % "compile")
	def test      (deps: ModuleID*): Seq[ModuleID] = deps map (_ % "test") 

	val scala_reflect = "org.scala-lang"     %  "scala-reflect"       % Build.scalaVer
	val mongo_scala   = "org.mongodb.scala"  %% "mongo-scala-driver"  % "1.1.0"
	val joda          = "joda-time"          %  "joda-time"           % "2.3"
	val joda_convert  = "org.joda"           %  "joda-convert"        % "1.7"
	val scalatest     = "org.scalatest"      %% "scalatest"           % "3.0.0"
	val slf4j_simple  = "org.slf4j"          %  "slf4j-simple"        % "1.7.7"
	val asm           = "org.ow2.asm"        %  "asm"                 % "5.1"
}
