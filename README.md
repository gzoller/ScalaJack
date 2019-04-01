
# ScalaJack  
  
[![license](https://img.shields.io/github/license/mashape/apistatus.svg?maxAge=86400)](https://opensource.org/licenses/MIT) [ ![bintray](https://api.bintray.com/packages/blocke/releases/scalajack/images/download.svg) ](https://bintray.com/blocke/releases/scalajack/_latestVersion) [![Build Status](https://img.shields.io/travis/gzoller/ScalaJack.svg?branch=master)](https://travis-ci.org/gzoller/ScalaJack) [![Codacy branch grade](https://img.shields.io/codacy/grade/9437bb8b88464096b1a848ba0eed8b7d/master.svg?maxAge=2592000)](https://www.codacy.com/app/gzoller/ScalaJack?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=gzoller/ScalaJack&amp;utm_campaign=Badge_Grade) [![Coveralls branch](https://img.shields.io/coveralls/gzoller/ScalaJack/master.svg?maxAge=360)](https://coveralls.io/github/gzoller/ScalaJack)  
  
ScalaJack is a very fast, seamless serialization engine for JSON, and other protocols, designed to require the minimum amount of help possible when serializing a class.  
  
Advanced Features:  
 - Handles tuples  
 - 'Any' support  
 - Handles default values for case class fields  
 - Rich configuration of trait type hint/value  
 - Supports value classes  
 - Sealted trait-style enumerations  
 - Extensible to other encodings (JSON, CSV, MongoDB, and DynamoDB are provided by ScalaJack, but you can roll your own too!)  
  
## Use  
  
ScalaJack is extremely simple to use.  
  
Include it in your projects by adding the following to your build.sbt:  
  
   libraryDependencies ++= Seq("co.blocke" %% "scalajack" % "6.0.0")  
  
If you want to use the optional MongoDB serialization support include this as well:  
  
   libraryDependencies ++= Seq("co.blocke" %% "scalajack_mongo" % "6.0.0")  
  
DynamoDB helpers are available here:  
  
   libraryDependencies ++= Seq("co.blocke" %% "scalajack_dynamo" % "6.0.0")  
  
ScalaJack is hosted on Bintray/JCenter.  If you're using pre-v0.13.9 of SBT you may need to enable the bintray resolver in your build.sbt with  
  
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
* [Re-name Case Class Fields](doc/mapname.md)  
* [Any Support](doc/any.md)  
* [Value Class Support](doc/valueClass.md)  
* [Parameterized Classes](doc/parameterized.md)  
* [Trait Type Hint Customization](doc/typeHint.md)  
* [Custom Type Adapters (custom read/render)](doc/custom.md)
* [Try and Capture](doc/tryAndCapture.md)  
* [ParseOrElse](doc/parseOrElse.md)  
* [Null and None treatment](doc/nullAndNone.md)  
* [Externalized Type Hints](doc/externalTypes.md)  
* [View/SpliceInto](doc/viewSplice.md)
* [Filter *(new)*](doc/filter.md)  
* [ScalaJack Configuration](doc/config.md)  
  
Non-JSON Formats:  
* [MongoDB](doc/mongo.md)  
* [CSV](doc/csv.md)  
* [DynamoDB](doc/dynamo.md)  
  
## Benchmarks  
  
 (Older benchmarks--new, more comprehensive ones coming in the future.  For now, expect ScalaJack 6.0 to be pretty close to 5.0, as performance improvements weren't a key goal of this release.)
    
|Benchmark         |Score      |Error        |Units  
|------------------|----------:|------------:|-----|  
|Hand-written      |28683.250  |± 3505.351   |ops/s  
|**ScalaJack 5.0** |20632.580  |±  306.105   |ops/s  
|Spray             |10314.990  |±  120.898   |ops/s  
|LiftJson          |9313.326   |±  212.206   |ops/s  
|ScalaJack 4.8.3   |6525.699   |±  36.103    |ops/s  
|Json4s            |5840.046   |±  201.42    |ops/s  
  
## Series 6  
  
Series 5 introduced a whole new engine for ScalaJack.  For series 6 we went through and streamlined the the everything internally.  JSON is no longer assumed in the core, allowing for an easier extension to other protocols, which we tested especially for MongoDB.  Internally the code is tighter and cleaner, which always makes us feel happy.  

A major goal of this release was to clean up the internals to be ready for Dotty/Scala 3.0 when it comes out.

We've got to give our users at least one new toy, so we've added a filter feature, so you can filter on incoming parsed messages, pulling out those you care about and passively ignoring those you don't. 

One feature was removed: non-canonical JSON support.  If you loved this feature, I understand, but its benefits just weren't worth the benefit of continued maintenacne.
  
We hope you'll enjoy using the latest ScalaJack!  
  
*Blöcke*
