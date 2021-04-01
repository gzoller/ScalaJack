
# ScalaJack

[![license](https://img.shields.io/github/license/mashape/apistatus.svg?maxAge=86400)](https://opensource.org/licenses/MIT)
[![bintray](https://api.bintray.com/packages/blocke/releases/scalajack/images/download.svg)](https://bintray.com/blocke/releases/scalajack/_latestVersion)

ScalaJack 7 is an all-new ScalaJack implmenation built on Scala 3.  For Scala 2.13 ScalaJack, please use (frozen) version 6.2.0.  ScalaJack 7 is built on JDK 13+.

ScalaJack is a very fast, seamless serialization engine for JSON, and other protocols, designed to require the minimum amount of help possible when serializing a class.

Advanced Features:
 - Handles tuples
 - 'Any' support
 - Handles default values for case class fields
 - Rich configuration of trait type hint/value
 - Supports value classes
 - Sealed trait-style enumerations
 - Extensible to other encodings (JSON, CSV, MongoDB, and DynamoDB are provided by ScalaJack, but you can roll your own too!)

## Use

ScalaJack is extremely simple to use.

Include the following in your build.sbt:
```
    libraryDependencies ++= Seq("co.blocke" %% "scalajack" % SJ_VERSION)
```

To use the **highly-recommended** reflection compiler plug-in, add to build.sbt:
```
addCompilerPlugin("co.blocke" %% "scala-reflection" % VERSION)
```
where VERSION is the latest scala-reflection version found by looking at the Download badge here: [www.blocke.co/scala-reflection](http://www.blocke.co/scala-reflection)

If you want to use the optional MongoDB serialization support include this as well:
```
    libraryDependencies ++= Seq("co.blocke" %% "scalajack_mongo" % SJ_VERSION)
```

DynamoDB helpers are available here:
```
    libraryDependencies ++= Seq("co.blocke" %% "scalajack_dynamo" % SJ_VERSION)
```
where SJ_VERSION is this version of ScalaJack.

Now you're good to go!  Let's use ScalaJack in your project to serialize/de-serialize a case class object into JSON:

```scala
import co.blocke.scalajack._

case class Person(name: String, age: Int)

val sj = ScalaJack()
val js = sj.render(Person("Mike",34))  // js == """{"name":"Mike","age":34}"""
val inst = sj.read[Person](js) // re-constitutes original Person
```

Couldn't be simpler!

### A word about performance...
Compared to pre-7.0 ScalaJack, which used Scala 2.x runtime reflection, ScalaJack is up to 30% faster in many cases when used with the highly-recommended scala-reflection compiler plugin.  

### A word about macros...
ScalaJack 7 uses Scala 3 macros to the fullest extent possible to do the hard work of reflecting on types. Macros impact the compile/test cycle in ways that are non-intuitive at first. Think of this example:

```scala
// File1.scala
case  class  Foo(name: String)

// File2.scala
val  js = sj.read[Foo](someJson)
```

In a non-macro implementation (e.g. Scala 2 runtime reflection) if you update Foo in File1.scala you naturally expect sbt to re-compile this file, and anything that depends on Foo, and the changes will be picked up in your program, and all will be well.

That's **not** necessarily what happens with macros! Remember, the macro code is run at compile-time. File2.scala needs to be re-compiled because the macro needs to be re-run to pick up your changes to Foo class in File1.scala. **Unfortunately sbt doesn't pick up this dependency!** If you don't know any better you'll just re-run your program after a change to File1.scala, like normal, and get a **spectacular exception with exotic errors** that won't mean much to you. The solution is you need to also recompile File2.scala.

This means you will be doing more re-compiling with macro-based code than you would without the macros. It's an unfortunate cost of inconvenience and time, but the payoff is a *dramatic* gain in speed at runtime, and in the case of reflection in Scala 3, using macros is really the only way to accomplish reflection.

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
* [Converters](doc/map.md)
* [ScalaJack Configuration](doc/config.md)
* [Gimme Speed!](doc/speed.md)

Non-JSON Formats:
* [YAML](doc/yaml.md)
* [MongoDB](doc/mongo.md)
* [Delimited (e.g. CSV)](doc/delimited.md)
* [DynamoDB](doc/dynamo.md)
* [Json4s](doc/json4s.md)

### Notes:
* 7.0.0-M2 -- Initial release for Scala 3


