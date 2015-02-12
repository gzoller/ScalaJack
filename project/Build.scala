import sbt._
import sbt.Keys._

import scala.Some

object Build extends Build {

	import Dependencies._

	val scalaVer = "2.11.4"

	lazy val basicSettings = Seq(
		organization 				:= "co.blocke",
		startYear 					:= Some(2015),
		scalaVersion 				:= scalaVer,
		resolvers					++= Dependencies.resolutionRepos,
		scalacOptions				:= Seq("-feature", "-deprecation", "-encoding", "UTF8", "-unchecked"),
		testOptions in Test += Tests.Argument("-oDF"),
		version 					:= "4.0-WIP"
	)

	// configure prompt to show current project
	override lazy val settings = super.settings :+ {
		shellPrompt := { s => Project.extract(s).currentProject.id + " > " }
	}

	lazy val root = (project in file("."))
		.settings(publishArtifact := false)
		.aggregate(scalajack)//, scalajack_mongo, scalajack_mysql)
		// For gpg might need this too:
		//publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))

	val pubSettings = Seq (
		publishArtifact in Test := false,
		publishTo := {
			val nexus = "https://oss.sonatype.org/"
			if( version.value.trim.endsWith("SNAPSHOT"))
				Some("snapshots" at nexus + "content/repositories/snapshots")
			else
				Some("releases" at nexus + "service/local/staging/deploy/maven2")
		},
		pomIncludeRepository := { _ => false },
		licenses := Seq("Apache 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
		homepage := Some(url("https://github.com/gzoller/ScalaJack")),
		pomExtra := (
		  <scm>
		    <url>git://github.com/gzoller/ScalaJack.git</url>
		    <connection>scm:git://github.com/gzoller/ScalaJack.git</connection>
		  </scm>
		  <developers>
		    <developer>
		      <id>gzoller</id>
		      <name>Greg Zoller</name>
		      <url>http://github.com/gzoller</url>
		    </developer>
		  </developers>)
	)

  	lazy val scalajack = project.in(file("core"))
		.settings(basicSettings: _*)
		.settings(pubSettings: _*)
		.settings(libraryDependencies ++=
			compile(joda, joda_convert, jackson, scala_reflect) ++
			test(scalatest)
		)

/*
	Don't build DB modules for now; until parse/render bit is done.

	lazy val scalajack_mongo = project.in(file("mongo"))
		.settings(basicSettings: _*)
		.settings(pubSettings: _*)
		.settings(libraryDependencies ++=
			compile( casbah ) ++
			test( scalatest, slf4j_simple )
		).dependsOn( scalajack )

	lazy val scalajack_mysql = project.in(file("mysql"))
		.settings(basicSettings: _*)
		.settings(pubSettings: _*)
		.settings(libraryDependencies ++=
			compile( mysql_jdbc ) ++
			test( scalatest, slf4j_simple )
		).dependsOn( scalajack )
*/
}

object Dependencies {
	val resolutionRepos = Seq(
		"Typesafe Repo"  		at "http://repo.typesafe.com/typesafe/releases/",
		"Typesafe Snapshots"	at "http://repo.typesafe.com/typesafe/snapshots/",
		"OSS"					at "http://oss.sonatype.org/content/repositories/releases",
		"OSS Staging"			at "http://oss.sonatype.org/content/repositories/staging",
		"Spray"					at "http://repo.spray.io",
		"PhantomMvn"			at "http://maven.websudos.co.uk/ext-release-local",
		"Mvn" 					at "http://mvnrepository.com/artifact"  // for commons_exec
	)

	def compile   (deps: ModuleID*): Seq[ModuleID] = deps map (_ % "compile")
	def test      (deps: ModuleID*): Seq[ModuleID] = deps map (_ % "test") 

// TODO: Move each to latest version!
	val jackson         = "com.fasterxml.jackson.core" % "jackson-core"	 	% "2.4.4"
	val scala_reflect 	= "org.scala-lang"			% "scala-reflect"		% Build.scalaVer
	val scala_lib 		= "org.scala-lang"			% "scala-library"		% Build.scalaVer
	val casbah 			= "org.mongodb"				%% "casbah"				% "2.7.1"
	val joda 			= "joda-time"				% "joda-time"			% "2.3"
	val joda_convert    = "org.joda"				% "joda-convert"		% "1.7"
	val scalatest 		= "org.scalatest" 			%% "scalatest"			% "2.2.1"
	val slf4j_simple 	= "org.slf4j" 				% "slf4j-simple" 		% "1.7.7"
	val mysql_jdbc  	= "mysql" 					% "mysql-connector-java" % "5.1.33"
}
