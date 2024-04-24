# ScalaJack

[![license](https://img.shields.io/github/license/mashape/apistatus.svg?maxAge=86400)](https://opensource.org/licenses/MIT)

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/co.blocke/scalajack_3/badge.svg)](https://search.maven.org/artifact/co.blocke/scalajack_3/8.0.0/jar)

ScalaJack 8 is an all-new ScalaJack implemenation built on Scala 3. For Scala 2.13 ScalaJack, please use (frozen) version 6.2.0. ScalaJack 8 is built using Scala 3.4.1 on JDK 21 LTS version.

ScalaJack is a very fast, seamless serialization engine for JSON designed to require a bare minimum of extra code to serialize a class.

Advanced Features:

- Handles tuples
- 'Any' support
- Handles default values for case class fields
- Rich configuration of trait type hint/value
- Supports value classes
- Sealed trait-style enumerations

## Use
ScalaJack is extremely simple to use.

Include the following in your build.sbt:
```
libraryDependencies ++= Seq("co.blocke" %% "scalajack" % SJ_VERSION)
```
Now you're good to go! Let's use ScalaJack in your project to serialize/de-serialize a case class object into JSON:
```scala
import co.blocke.scalajack._

case class Person(name: String, age: Int)

given sjPerson: ScalaJack[Person] = sjCodecOf[Person] // create a re-usable Person codec
...
val inst = Person("Mike",34)
val js = sjPerson.toJson(inst) // """{"name":"Mike","age":34}"""
val inst = sjPerson.fromJson(js) // re-constitutes original Person
```
Couldn't be simpler!

### A word about performance...

Compared to pre-8.0 ScalaJack, which used Scala 2.x runtime reflection, ScalaJack is dramatically faster in almost every case. How's this work? ScalaJack 8 uses macros, that at compile-time generate all the serialization code for you (the codecs). It's very much like writing hand-tooled, field-by-field serialization code yourself, except ScalaJack does it at compile-time.  Wherever you see ```sjCodecOf``` is where the compiler will generate all the serialization code.  **(That also means try not to use sjCodecOf more than once for any given class or you'll generate a lot of redundant code!)**

You only need to worry about generating codecs for your top-most level classes.  Some serialization libraries require all classes to be specifically called out.  ScalaJack doesn't require this.  For example:

```scala
case class Dog(name: String, numLegs: Int)
case class Person(name: String, age: Int, dog: Dog)

// create a re-usable Person codec (includes Dog for free!)
given sjPerson: ScalaJack[Person] = sjCodecOf[Person] 
```
In this example, the contained Dog class is automatically detected and genrated by ScalaJack, so if all you care about is Person, that's the only codec you need.

### A word about macros...

ScalaJack 8 uses Scala 3 macros to the fullest extent possible to do the hard work of reflecting on types. Macros impact the compile/test cycle in ways that are non-intuitive at first. Think of this example:

```scala
// File1.scala
case class Foo(name: String)

// File2.scala
given sjFoo: ScalaJack[Foo] = sjCodecOf[Foo] 
val js = sjFoo.fromJson(someJson)
```

In a non-macro program (e.g. something using Scala 2 runtime reflection) if you update Foo in File1.scala you naturally expect sbt to re-compile this file, and anything that depends on Foo, and the changes will be picked up in your program, and all will be well.

That's **not** necessarily what happens with macros! Remember, the macro code is run at compile-time. File2.scala needs to be re-compiled because the macro needs to be re-run to pick up your changes to Foo class in File1.scala. **Unfortunately sbt doesn't pick up this dependency!** If you don't know any better you'll just re-run your program after a change to File1.scala, like normal, and get a **spectacular exception with exotic errors** that won't mean much to you. The simple, but non-intuitive, solution is you need to also recompile File2.scala.

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

### Notes:

* 8.0.0 -- Rebuild on Scala 3.4.1 and major refactor of ScalaJack 7.0
* 7.0.3 -- Rebuild on Scala 3.2.1
* 7.0.1 -- GA release of ScalaJack 7 for Scala 3.
* 7.0.0-M2 -- Initial release for Scala 3