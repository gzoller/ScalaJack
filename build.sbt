name := "ScalaJack"

version := "2.0.5.EXP"

organization := "co.blocke"

scalaVersion := "2.11.2"

scalacOptions += "-feature"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")

javacOptions ++= Seq("-source", "1.6", "-target", "1.6")

resolvers ++= Seq("Typesafe" at "http://repo.typesafe.com/typesafe/releases/",
            "Maven Repo" at "http://mvnrepository.com/artifact",
             "Sonatype releases" at "https://oss.sonatype.org/content/repositories/snapshots"
             )

libraryDependencies ++= Seq("com.fasterxml.jackson.core" % "jackson-core"   % "2.3.2",
                            "org.scala-lang"             % "scala-compiler" % "2.11.2",
                            "org.mongodb"                % "casbah_2.11"    % "2.7.1",
                            "joda-time"                  % "joda-time"      % "2.3",
                            "org.scalatest"              % "scalatest_2.11" % "2.1.3" % "test")

publishArtifact in Test := false

publishTo := {
	val nexus = "https://oss.sonatype.org/"
	if( version.value.trim.endsWith("SNAPSHOT"))
		Some("snapshots" at nexus + "content/repositories/snapshots")
	else
		Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

pomIncludeRepository := { _ => false }

licenses := Seq("Apache 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

homepage := Some(url("https://github.com/gzoller/ScalaJack"))

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
