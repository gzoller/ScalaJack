name := "ScalaJack"

version := "1.0"

scalaVersion := "2.10.1"

resolvers += "Typesafe" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq("com.fasterxml.jackson.core" % "jackson-core"   % "2.2.0",
                            "org.scala-lang"             % "scala-compiler" % "2.10.1",
                            "org.scalatest"              % "scalatest_2.10" % "2.0.M6-SNAP22" % "test")
