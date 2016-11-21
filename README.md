# ScalaJack

[![license](https://img.shields.io/github/license/mashape/apistatus.svg?maxAge=86400)](https://opensource.org/licenses/MIT) [![Bintray](https://img.shields.io/bintray/v/blocke/releases/scalajack.svg?maxAge=360)](https://bintray.com/blocke/releases/scalajack) [![Build Status](https://img.shields.io/travis/gzoller/ScalaJack.svg?branch=master)](https://travis-ci.org/gzoller/ScalaJack) [![Codacy branch grade](https://img.shields.io/codacy/grade/9437bb8b88464096b1a848ba0eed8b7d/master.svg?maxAge=2592000)](https://www.codacy.com/app/gzoller/ScalaJack?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=gzoller/ScalaJack&amp;utm_campaign=Badge_Grade) [![Coveralls branch](https://img.shields.io/coveralls/gzoller/ScalaJack/master.svg?maxAge=360)](https://coveralls.io/github/gzoller/ScalaJack)

ScalaJack is a very fast, seamless JSON serialization engine designed to require the minimum amount of help possible when serializing a class.

Advanced Features:
 - Handles tuples
 - 'Any' support
 - Handles default values for case class fields
 - Rich configuration of trait type hint/value
 - Supports value classes
 - Pluggable reader/render (for non-JSON encodings--CSV, MongoDB, and DynamoDB)

## Use

ScalaJack is extremely simple to use.

Include it in your projects by adding the following to your build.sbt:

	libraryDependencies ++= Seq("co.blocke" %% "scalajack" % "5.0.1")

If you want to use the optional MongoDB serialization support include this as well:

	libraryDependencies ++= Seq("co.blocke" %% "scalajack_mongo" % "5.0.1")

DynamoDB helpers are available here:

	libraryDependencies ++= Seq("co.blocke" %% "scalajack_dynamo" % "5.0.1")

ScalaJack is hosted on Bintray/JCenter now so if you're using sbt v0.13.9+ you should find it with no issues.

If you're on sbt v0.13.11 you may need to enable the bintray resolver in your build.sbt with

``` sbt
useJCenter := true
```

Now you're good to go!  Let's use ScalaJack in your project to serialize/de-serialize a case class object into JSON:

	import co.blocke.scalajack._

	val sj = ScalaJack()
	val js = sj.render( myCaseObj )  // serialization
	val myObj = sj.read[MyCaseClass](js) // deserialization

Couldn't be simpler!

## Features

* [Case Classes and Traits](doc/classesAndTraits.md)
* [Non-Case Classes and Java Class Support](doc/noncase.md)
* [Any Support](doc/any.md)
* [Value Class Support](doc/valueClass.md)
* [Parameterized Classes](doc/parameterized.md)
* [Trait Type Hint Customization](doc/typeHint.md)
* [Custom Type Adapters (custom read/render)](doc/custom.md)
* [Try and Capture](doc/tryAndCapture.md)
* [ParseOrElse](doc/parseOrElse.md)
* [Null and None treatment](doc/nullAndNone.md)
* [Non-Canonical JSON](doc/noncanonical.md)
* [Externalized Type Hints](doc/externalTypes.md)

Non-JSON Formats:
* [MongoDB](doc/mongo.md)
* [CSV](doc/csv.md)
* [DynamoDB](doc/dynamo.md)

## Benchmarks

|Benchmark         |Score      |Error        |Units
|------------------|----------:|------------:|-----|
|Hand-written      |28683.250  |± 3505.351   |ops/s
|**ScalaJack 5.0** |20632.580  |±  306.105   |ops/s
|Spray             |10314.990  |±  120.898   |ops/s
|LiftJson          |9313.326   |±  212.206   |ops/s
|ScalaJack 4.8.3   |6525.699   |±  36.103    |ops/s
|Json4s            |5840.046   |±  201.42    |ops/s

## Series 5

The entire ScalaJack engine and test suite was redesigned for 5.0.  This new design streamlines processing tremendously while maintaining both flexibility and simplicity.  In addition to reaching new levels of performance, a major design goal was to allow a flexible way to implement new serialization targets beyond JSON.

This project welcomes our newest team member and core committer, Adam Paynter!  The design for ScalaJack 5's new engine was made possible by his thought leadership and sense of adventure.

We hope you'll enjoy using the latest ScalaJack!

*Blöcke*
