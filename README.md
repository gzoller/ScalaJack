# ScalaJack

[![license](https://img.shields.io/github/license/mashape/apistatus.svg?maxAge=86400)](https://opensource.org/licenses/MIT)

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/co.blocke/scalajack_3/badge.svg)](https://search.maven.org/artifact/co.blocke/scalajack_3/8.0.0/jar)

ScalaJack 8 is an all-new ScalaJack serializer implemenation built on Scala 3. For Scala 2.13 ScalaJack, please use the frozen version 6.2.0. ScalaJack 8 is built 
using Scala 3.4.1 on JDK 21 LTS version.

ScalaJack is a very fast, seamless serialization engine for unstructured data types designed to require a bare minimum of extra code 
to serialize a class. ScalaJack supports JSON in its focus on over-the-wire data and message/event transport. (We looked at offering MsgPack support, but to our surprise benchmarks
showed that MsgPack serialization had about 25% slower write performance and 45% slower read performance than JSON, so for now we're sticking with just JSON support.)

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
// File1.scala
case class Person(name: String, age: Int)

// File2.scala
import co.blocke.scalajack.*

given sjPerson: ScalaJack[Person] = sjCodecOf[Person] // create a re-usable Person codec
...
val inst = Person("Mike",34)
val js = sjPerson.toJson(inst) // """{"name":"Mike","age":34}"""
sjPerson.fromJson(js) // re-constitutes original Person
```
Couldn't be simpler!

| **NOTE:** Classes must be defined in a different file from where ScalaJack is called.
| This is a Scala macro requirement, not a ScalaJack "ism"

### A word about performance...

Compared to pre-8.0 ScalaJack, which used Scala 2.x runtime reflection, ScalaJack is dramatically faster in almost every case. How's this work? ScalaJack 8 uses macros, that at compile-time generate all the serialization code for you (the codecs). It's very much like writing hand-tooled, field-by-field serialization code yourself, except ScalaJack does it at compile-time.  Wherever you see ```sjCodecOf``` is where the compiler will generate all the serialization code.  **(That also means try not to use sjCodecOf more than once for any given class or you'll generate a lot of redundant code!)**

### Easy codecs
You only need to worry about generating codecs for your top-most level classes.  Some serialization libraries require all nested classes in an object hierarchy to be 
specifically called out for code generation, which can get pretty burdensome.  ScalaJack doesn't require this.  For example:

```scala
case class Dog(name: String, numLegs: Int)
case class Person(name: String, age: Int, dog: Dog)

// create a re-usable Person codec (includes Dog for free!)
given sjPerson: ScalaJack[Person] = sjCodecOf[Person] 
```
In this example, the contained Dog class is automatically detected and genrated by ScalaJack, so if all you care about is Person, and would never serialize a Dog as a top-level value, then Persion is the only codec you need.

### A word about macros...

ScalaJack 8 uses Scala 3 macros to the fullest extent possible to do the hard work of reflecting on types. Macros impact the compile/test cycle in ways that are non-intuitive at first. Think of this example:

```scala
// File1.scala
case class Foo(name: String)

// File2.scala
given sjFoo: ScalaJack[Foo] = sjCodecOf[Foo] 
val js = sjFoo.fromJson(someJson)
```

In a non-macro program (e.g. something using Scala 2 runtime reflection) let's say you add a new field to class Foo in File1.scala. You naturally expect sbt to re-compile this file, and anything that depends on Foo, and the changes will be picked up in your program, and all will be well.

That's **not** necessarily what happens with macros! Remember, the macro code is run/expnded at compile-time. File2.scala needs to be re-compiled because the macro that gets expanded at sjCodecOf[Foo] needs to be re-generated to pick up your changes to Foo class in File1.scala. **Unfortunately sbt cna't detect up this dependency!** If you don't know any better you'll just re-run your program after a change to File1.scala, like normal, and you'll get a spectacular exception with exotic errors that won't mean much to you. The simple, but non-intuitive, solution is you need to also recompile File2.scala.

This means you will be doing more re-compiling with macro-based code than you would without the macros. It's an unfortunate cost of inconvenience, but the payoff is a *dramatic* gain in speed at runtime, and in the case of reflection in Scala 3, using macros is the only way to accomplish reflection, so there really isn't an alternative.

## Features
* [Case Classes and Traits](doc/classesAndTraits.md)
* [Non-Case Classes and Java Class Support](doc/noncase.md)
* [Re-name Case Class Fields](doc/mapname.md)
* [Any Support](doc/any.md)
* [Value Class Support](doc/valueClass.md)
* [Parameterized Classes](doc/parameterized.md)
* [Trait Type Hint Customization](doc/typeHint.md)
* [Null and None treatment](doc/nullAndNone.md)
* [NeoType Support](doc/neotype.md)
* [Union type](doc/union.md)
* [Gimme Speed!](benchmark/README.md)

### Notes:

* 8.0.0 -- Rebuild on Scala 3.4.1 and major refactor of ScalaJack 7.0
* 7.0.3 -- Rebuild on Scala 3.2.1
* 7.0.1 -- GA release of ScalaJack 7 for Scala 3.
* 7.0.0-M2 -- Initial release for Scala 3