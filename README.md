# ScalaJack

[![license](https://img.shields.io/github/license/mashape/apistatus.svg?maxAge=86400)](https://opensource.org/licenses/MIT)
[![bintray](https://api.bintray.com/packages/blocke/releases/scalajack/images/download.svg)](https://bintray.com/blocke/releases/scalajack/_latestVersion)
[![Build Status](https://img.shields.io/travis/gzoller/ScalaJack.svg?branch=master)](https://travis-ci.org/gzoller/ScalaJack)
[![Codacy branch grade](https://img.shields.io/codacy/grade/9437bb8b88464096b1a848ba0eed8b7d/master.svg?maxAge=2592000)](https://www.codacy.com/app/gzoller/ScalaJack?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=gzoller/ScalaJack&amp;utm_campaign=Badge_Grade)
[![Coveralls branch](https://img.shields.io/coveralls/gzoller/ScalaJack/master.svg?maxAge=360)](https://coveralls.io/github/gzoller/ScalaJack)

ScalaJack is a very fast, seamless serialization engine for JSON, and other protocols, designed to require the minimum amount of help possible when serializing a class.

Advanced Features:
 - Handles tuples
 - 'Any' support
 - Handles default values for case class fields
 - Rich configuration of trait type hint/value
 - Supports value classes
 - Sealed trait-style enumerations
 - Extensible to other encodings (JSON, CSV, MongoDB, and DynamoDB are provided by ScalaJack, but you can roll your own too!)

> **Note:** 2.13.1 on JDK 13.0.1 or later is strongly recommended!
>Scala has done a lot of very recent work to improve compatibility with later JDKs and it's been a bumpy road.  The combination above has been tested.  
Combinations of earlier versions are known to have compabitility problems.  If you must use earlier Scala or JVM versions then use JDK 1.8.

## Use

ScalaJack is extremely simple to use.

Include it in your projects by adding the following to your build.sbt:

    libraryDependencies ++= Seq("co.blocke" %% "scalajack" % "6.2.0")

If you want to use the optional MongoDB serialization support include this as well:

    libraryDependencies ++= Seq("co.blocke" %% "scalajack_mongo" % "6.2.0")

DynamoDB helpers are available here:

    libraryDependencies ++= Seq("co.blocke" %% "scalajack_dynamo" % "6.2.0")

ScalaJack is hosted on Bintray/JCenter.  If you're using pre-v0.13.9 of SBT you may need to enable the bintray resolver in your build.sbt with

``` sbt
useJCenter := true
```

Now you're good to go!  Let's use ScalaJack in your project to serialize/de-serialize a case class object into JSON:

```scala
import co.blocke.scalajack._

val sj = ScalaJack()
val js = sj.render( myCaseObj )  // serialization
val myObj = sj.read[MyCaseClass](js) // deserialization
```

Couldn't be simpler!

## Features

* [Case Classes and Traits](doc/classesAndTraits.md)
* [Non-Case Classes and Java Class Support](doc/noncase.md)
* [Re-name Case Class Fields](doc/mapname.md)
* [Any Support](doc/any.md)
* [Value Class Support](doc/valueClass.md)
* [Parameterized Classes](doc/parameterized.md)
* [Trait Type Hint Customization](doc/typeHint.md)
* [Custom Type Adapters (custom read/render)](doc/custom.md)
* [Try and Capture](doc/tryAndCapture.md)
* [ParseOrElse and Cascading Fallback Parsing](doc/parseOrElse.md)
* [Null and None treatment](doc/nullAndNone.md)
* [Externalized Type Hints](doc/externalTypes.md)
* [View/SpliceInto](doc/viewSplice.md)
* [Filter](doc/filter.md)
* [Union type](doc/union.md)
* [Converters *(new)*](doc/map.md)
* [ScalaJack Configuration](doc/config.md)
* [Gimme Speed!](doc/speed.md)

Non-JSON Formats:
* [YAML *(new)*](doc/yaml.md)
* [MongoDB](doc/mongo.md)
* [Delimited (e.g. CSV)](doc/delimited.md)
* [DynamoDB](doc/dynamo.md)
* [Json4s](doc/json4s.md)

## Benchmarks

I need to start this section with an apology.  After having benchmarks deactivated for some time I finally wired them up and discovered that
ScalaJack 6.0 series was significantly slower than the 5.x series.  Definitely not desirable!

ScalaJack 6.1 has undergone a significant streamlining and refactoring with beneficial results. Numbers are below.  

*Disclaimer: Benchmarks are very situational.  We didn't hand-pick a senario to favor our product.  It is entirely possible/likely you can find scenarios where your favorite parser beats ScalaJack.*

|Benchmark         |Score      |Error        |Units
|------------------|----------:|------------:|-----|
|**ScalaJack 6.1 (forType[T])** |27729.218  |±  338.995   |ops/s
|Hand-written      |24041.835  |± 1752.455   |ops/s
|**ScalaJack 6.1** |20232.361  |±  736.766   |ops/s
|Circe             |19971.288  |±  986.961   |ops/s
|ScalaJack 5.x     |16349.600  |±  193.090   |ops/s
|ScalaJack 6.0     |11111.795  |±  127.843   |ops/s
|LiftJson          |9313.472   |±  639.219   |ops/s
|Json4s            |3425.144   |±  219.970   |ops/s

The key takeaway here is that for our sample test, ScalaJack was very fast, and most critically, ScalaJack 6.1 is a dramatic improvement over 6.0.

## Series 6

Series 5 introduced a whole new engine for ScalaJack.  For series 6 we went through and streamlined everything internally.  JSON is no longer assumed in the core, allowing for an easier extension to other protocols.  Internally the code is tighter and cleaner, which always makes us feel happy.

A major goal of this release was to clean up the internals to be ready for Dotty/Scala 3.0 when it comes out.

We've got to give our users at least a few new toys, so we've added a filter feature, so you can filter on incoming parsed messages, pulling out those you care about and passively ignoring those you don't.

There have also been improvements to Delimited/CSV handling.  Before, ScalaJack could not handle List in CSV format.  Now it can (for simple Lists), along with Either and Enumeration.

MongoDB support has been strengthened, with some of the limitations of earlier versions removed.  Type hint modifiers (for type members) are now supported.  Support for scalar non-String keys (Int, Boolean, etc) work now too.  Try and SJCapture features are now supported for Mongo as well as JSON.

One feature was removed: non-canonical JSON support.  If you loved this feature, I understand, but its benefits just weren't worth the benefit of continued maintenance.

We hope you'll enjoy using the latest ScalaJack!

*Blöcke*
