name := "ScalaJack"

version := "2.0.0"

organization := "co.blocke"

scalaVersion := "2.11.0"

scalacOptions += "-feature"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")

resolvers ++= Seq("Typesafe" at "http://repo.typesafe.com/typesafe/releases/",
            "Maven Repo" at "http://mvnrepository.com/artifact",
             "Sonatype releases" at "https://oss.sonatype.org/content/repositories/snapshots"
             )

libraryDependencies ++= Seq("com.fasterxml.jackson.core" % "jackson-core"   % "2.2.3",
                            "org.scala-lang"             % "scala-compiler" % "2.11.0",
                            "org.mongodb"                %% "casbah"        % "2.7.1-SNAPSHOT",
                            "org.scalatest"              % "scalatest_2.11.0-RC4" % "2.1.3" % "test")
//                            "org.scalatest"              % "scalatest_2.10" % "2.0" % "test")

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
