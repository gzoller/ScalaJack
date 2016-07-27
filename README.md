CI service | Status | Description
-----------|--------|------------
Travis | [![Build Status](https://travis-ci.org/gzoller/ScalaJack.png?branch=master)](https://travis-ci.org/gzoller/ScalaJack) | Linux container tests

# ScalaJack

Very fast JSON parser/generator for Scala case classes using Jackson that requires virtually no annotations
or "helper" code.

Advanced Features:
 - Handles tuples
 - Limited 'any' support
 - Handles default values for case class fields
 - Rich configuration of trait type hint/value
 - Supports value classes
 - Pluggable reader/render (for non-JSON encodings in the future)

# Use

ScalaJack is extremely simple to use.

Include it in your projects by adding the following to your build.sbt:

	libraryDependencies ++= Seq("co.blocke" %% "scalajack" % "4.8.2")

If you want to use the optional MongoDB serialization support include this as well:

	libraryDependencies ++= Seq("co.blocke" %% "scalajack_mongo" % "4.8.2")

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

Couldn't be simpler.

### Trait support
ScalaJack can handle traits too.  To do this you'll need the help of a type hint in the JSON.  This tells ScalaJack what actual class to create in support of the trait.

The default type hint is "_hint" but you can set whatever you want (very powerful for 3rd party JSON!) You set your own type hint with a VisitorContext object as shown below.   Don't forget to use the same type hint name for render and read.

```scala
package com.myproj

trait Pet { val name:String }
case class Dog( name:String ) extends Pet
case class Cat( name:String ) extends Pet

val p : Pet = Dog("Fido")
val sj = ScalaJack()
val js = sj.render(p)  // {"_hint":"com.myproj.Dog","name":"Fido"}
val vc = VisitorContext(hintMap=Map("default"->"kind"))
val js2 = sj.render(p,vc) // {"kind":"com.myproj.Dog","name":"Fido"}

// Be sure to match type hints when reading!
val d1 = sj.read[Pet](js)
val d2 = sj.read[Pet](js2,vc)
```
There's one more cool trick we can do with traits.  Imagine you're parsing JSON from a 3rd party (i.e. you don't control its structure) and you're modeling nested traits but want the type hint field to be different.  ScalaJack's VisitorContext object allows you specify per-trait hints like so:

```scala
package com.myproj

trait Animal {
	val name:String
}
case class Dog(name:String) extends Animal
case class Cat(name:String) extends Animal
trait Pet {
	val kind:Animal
	val food:String
}
case class NicePet(kind:Animal, food:String) extends Pet
case class GrumpyPet(kind:Animal, food:String) extends Pet

val pets = List(NicePet(Dog("Fido"),"kibbles"),GrumpyPet(Cat("Meow"),"fish"))
val sj = ScalaJack()
val vc = VisitorContext(hintMap = Map(
		"default"->"_hint",
		"com.myproj.Pet"->"_happy",
		"com.myproj.Animal"->"_kind"))
val js = sj.render(pets,vc)
// produces: [{"_happy":"com.myproj.NicePet","kind":{"_kind":"com.myproj.Dog","name":"Fido"},"food":"kibbles"},{"_happy":"com.myproj.GrumpyPet","kind":{"_kind":"com.myproj.Cat","name":"Meow"},"food":"fish"}]
```
Note how you get different type hints for specific traits.  This can be invaluable for advanced JSON parsing of 3rd party data.

# Custom Value Class JSON

**NOTE:**  Custom value class JSON handling has changed since ScalaJack 4.7.  We no longer use the VisitorContext.

Let's imagine we have a value class PosixDate that has a long as its single value type, holding a Unix timestamp.  Let's further assume we use this class in a case class for server stats like this:

```scala
class PosixDate( val ts:Long = (new Date()).getTime ) extends AnyVal {
	def toDate : Date = new Date(ts)
	def isBefore( pd:PosixDate ) = this.ts < pd.ts
	def isAfter( pd:PosixDate )  = this.ts > pd.ts
	override def toString() = ts.toString
}

case class ServerStats( instanceName:String, upSince:PosixDate )
```
Good enough.  Now let's render some JSON:

```scala
val ss = ServerStats( "admin", new PosixDate() )
val js = sj.render( ss )  // Output: {"instanceName":"admin","upSince":1383317215}
```

Hmm... For my use I might want something different than that Long numeric output.  I may want a specific format for my timestamp in the JSON.  I can accomplish this by providing a companion object to my value class that extends a special trait, **ValueClassCustom**.  This trait defines read and render functions as PartialFunctions, which allows you to provide whatever reading or rendering logic you need for a given "flavor" of parsing. 

The two flavors supported to day are:  **JsonKind** and **MongoKind**


```scala
object PosixDate extends ValueClassCustom {
  def read:PartialFunction[(KindMarker,_), Long] = {
	  case (j:JsonKind,js:String) => 
		  DateTimeFormat.forPattern("MMMM, yyyy").parseDateTime(js).toDate.getTime
  }
  def render:PartialFunction[(KindMarker,_),Any] = {
	  case (j:JsonKind,pd:Long) => 
		  '"'+DateTimeFormat.forPattern("MMMM, yyyy").print( new DateTime( new java.util.Date(pd)) )+'"'
  }
}
```
Admittedly, the date formatting code here is a bit tortured, but to prove a point.  You'd use it like this:
```scala
  val sj = ScalaJack()
  val one = ServerStats("Admin", new PosixDate)
  println("1: "+one)
  val js = sj.render(one)  // outputs {"instanceName":"Admin","upSince":"July, 2016"}
  println("2: "+js)
  println("3: "+sj.read[ServerStats](js))
```
The Long (the internal representation of your PosixDate value class) is successfully read and rendered using whatever formatting rules you specify.

Some tips:

 - For JSON parsing the first element of the tuple in read is JsonKind,
   and the second is a String (the raw JSON value).
 - For rendering JSON, the second tuple element of render is the internal data type, or Long
   in our example. 
 - For rendering JSON note that quotes are included in    the result. 
   If your value was numeric or boolean you wouldn't need    these.

# Custom Renderings for simple Java objects and Primitives

This feature is similar to the previous section, Custom Value Class JSON, however it doesn't require you to wrap your objects in a value class.  Neato!

Let's look at a similar example using two types: org.joda.time.DateTime, and java.nio.charset.Charset.  The first is a built-in ScalaJack primitive and the second is a simple Java object.

**Note**: *"Simple" here means a class can be rendered and re-read with just one field.  ScalaJack does not (yet) support a more sophisticated multi-field rendering of a Java class.*

You're already familiar with the use case from the last section, so let's go right to code:
```scala
	// We're going to override the DateTime default rendering
	object Overrides {
		type SpecialTime = DateTime
	}
	import Overrides._
	
	case class ShowMe( when:SpecialTime, again:DateTime, charset:Charset )

	val specialTimeHandler = CustomReadRender(
		{
			case (j:JsonKind,js:String) => ISODateTimeFormat.dateTime().parseDateTime(js)
		},
		{
			case (j:JsonKind,thing:DateTime) => '"'+ISODateTimeFormat.dateTime().withZoneUTC().print(thing)+'"'
		}
	)
	val charsetHandler = CustomReadRender(
		{
			case (j:JsonKind,js:String) => Charset.forName(js)
		},
		{
			case (j:JsonKind,thing:Charset) => '"'+thing.toString+'"'
		}
	)

	val handlerMap = Map(
		"com.my.path.Overrides.SpecialTime"->specialTimeHandler,
		"java.nio.charset.Charset"->charsetHandler
		)

	val sjJS  = ScalaJack()
	val vc = VisitorContext().copy(customHandlers = handlerMap)

	val showMe = ShowMe( new DateTime, new DateTime, Charset.defaultCharset )
	val js = sjJS.render(showMe,vc) // Don't forget the vc!!
```

This will produce some JSON looking like this:
```JSON
{"when":"2016-07-27T00:35:51.977Z","again":1469579752025,"charset":"UTF-8"}
```
Notice something cool... The fields when and again are both basically DateTime values.  We set when to a custom type then mapped that type to a custom handler, so the when value got custom read/render treatment, while the vanilla DateTime variant was handled in the default way.

There is no default handling for a Java class like Charset!  You must always provide a CustomHandler for a Java type or ScalaJack will give you an error.


# MongoDB Persistence

**NOTE:**  As of version 4.5.1 ScalaJack supports the new asynchronous MongoDB drivers for Scala, deprecating Casbah support.  If you need Casbah, version 4.4.6 is the last version supporting Casbah.

ScalaJack doesn't wrap the MongoDB persistence libraries--that's not its mission.  It does provide a way to convert case classes (and traits)to/from Documents.  You'll need to include the mongo support package:

```scala
import co.blocke.scalajack._
import mongo._

val sjMongo = ScalaJack(MongoFlavor()) // produce a Mongo-flavored ScalaJack
val mydbo  = sjMongo.render( myCaseClass )
val myCC   = sjMongo.read[MyClass]( mydbo )
```

The VisitorContext modifications work here too, as before with JSON.

You can (and should) specify the key field(s) for your classes with the @DBKey annotation as shown here:

```scala
case class Sample( @DBKey lastName:String, birthDate:Long, hobbies:List[String] )
```

Compound keys are also supported:

```scala
case class Sample( @DBKey lastName:String, @DBKey birthDate:Long, hobbies:List[String] )
```

Support has been added for Mongo's ObjectId type if you wish to use this directly.

```scala
case class Sample( @DBKey _id:ObjectId, stuff:Int )
```

Once you have your Document, use MongoDB's native Scala API as you normally would.


# CSV Support
CSV support is provided but by necessity is very limited.  CSV itself is not sufficiently expressive to handle complex data structures such as fields n a class that are themselves classes, collections, etc.  Therefore the following rules apply:

* Case class only support
* Classes must be "flat"
	* no List/Array, Map, or collection fields
	* no fields that are classes
* Field ordering in the CSV must match the declared field order of the class
* Optional fields of simple type are OK, for example Option[String] (these will be read/rendered as empty csv: foo,,bar)
* Classes must be concrete. Traits are not allowed. (How would you supply the type hint?)
* Parameterized classes are OK as long as the parameters conform to the above rules

During a read of a CSV record, if a field is empty "foo,,bar" it may mean the empty value is optional, which will be read in as None.  It may also mean that a field in the class has a default value on the constructor, in which case the default value is applied.  If the class field corresponding to the empty spot in the CSV is neither Option nor a default-value field then it is viewed as an empty value.

### Usage
CSV rendering is an inexact science, so here's how this works in ScalaJack.  First you'll need to instantiate a ScalaJack instance with CSVFlavor as shown in the code below.
Null values are represented as null in the CSV.  null (unquoted) read in is a null value in the object.  "null" (quoted) is a String with value "null" (not a real null value).  For example "foo,null,bar" contains a null.  "foo,"null",bar" shows a string having value "null".  Confused yet?

Optional None is rendered as blank in the CSV and when read in, like "foo,,bar".

As per the (unofficial) CSV spec:

* Fields may be enclosed in double-quotes
* String values containing double quotes *must* be enclosed by double quotes and the double-quote char in the string must be escaped by another double quote like this:  "I read ""Lord of the Rings"" last year"
* String values that contain a field separator (comma) must be enclosed in double quotes.

Use CSV support like this:
```scala
case class Foo(name:String, age:Int)
val sjCSV  = ScalaJack(CSVFlavor())
sjCSV.render( Foo("Fred", 32) ) // Fred,32
sjCSV.read[Foo]( "Fred,32" ) // Foo("Fred",32)
```

# MySQL Support
The MySQL support provided in the ScalaJack 3.x series has been removed for the time being.  I wasn't entirely happy with it.  It may be back in a future release if there is a swell of people interested in it.

# VisitorContext
ScalaJack uses an optional VisitorContext object you can pass into read/render to control certain aspects of how data is processed.  Looking at the definition of VisitorContext is a good starting point:

```scala
case class VisitorContext(
	isCanonical    : Boolean = true,    // allow non-string keys in Maps--not part of JSON spec
	isValidating   : Boolean = false,
	estFieldsInObj : Int     = 128,
	customHandlers : Map[String,CustomReadRender] = Map.empty[String,CustomReadRender],
	hintMap        : Map[String,String] = Map("default" -> "_hint"),  // per-class type hints (for nested classes)
	hintValueRead   : Map[String,(String)=>String] = Map.empty[String,(String)=>String], // per-class type hint value -> class name
	hintValueRender : Map[String,(String)=>String] = Map.empty[String,(String)=>String]  // per-class type class name -> hint value
	)
```
Let's look at these fields one-by-one.

**isCanonical**=true is standard JSON.  In some strange situations you may wish JSON-like notation that does not use strings as keys.  You would set this field to false to allow that.  Note that this is *not* really JSON and won't many libraries (like Mongo) assume and require string-based keys for JSON objects.

**isValidating** controls which of ScalaJack's 2 parsers is used.  The non-validating parser (isValidating=false) is a bit faster but doesn't make much effort in telling you why JSON parsing failed.  The validating parser is a little slower but has better error reporting.

**estFieldsInObj** is also something you'll likely want to set for non-validating parsing.  Part of its speed is pre-allocated buffers, so you'll need to guess a reasonable maximum field count for the largest expected object in your data.  If you use the validating parser you can ignore this field as another reason the validating parser is slower is that it can auto-scale its buffers without your help.

**customHandlers** is a map of class name to CustomReadRender object, which provides PartialFunctions for read and render operations by kind (JsonKind, MongoKind, etc.)  This is how you implement custom renderings for either simple Java types and for overriding standard renderings for primitives.

**hintMap** is a map of class name (fully-qualified) to trait type hint string.  Note there must always be a "default" entry in your map or you risk breaking.

**hintValueRead/hintValueRender** maps allow you to use strings other than fully-qualified class names as a hint value.  This can be very valuable if the "discriminator" field for your JSON is provided by a 3rd party, or you'd like to hide the internal details of hint-handling from an external system.  They key to both maps is the fully-qualified trait name.  The value of hintValueRead is a function that accepts a String (the "friendly" hint value) and emits a fully-qualified class name.  In a simple implementation this may just prepend a package hierarchy, a la "com.foo.something", but it can be whatever you want.  hintValueRender goes the other way.  It's values are functions accepting a String (a fully-qualified class name) and emits a friendly hint value.

# View/SpliceInto Feature

If you've ever had the need to support view "projections" to/from a large master object, these
functions will help.  Below is a contrived example of a User object that has some protected fields we don't want to easily project out (to make it save for a UI, for example):

```scala
case class User(
   name:String,
   age:Int,
   password:String,  // don't want password or ssn going up to the UI
   ssn:String
)

case class SafeUser(
   name:String,
   age:Int
)

val safe = ScalaJack.view[SafeUser]( fullUser ) // fullUser is of type User, safe will be SafeUser
```

Note that the field names and types of the view class (SafeUser above) must be exactly the same as the
corresponding fields of the master class, and you shouldn't have extra fields that aren't
present somewhere in the master class.  Field order, however, is not important.

You can also go the other way...incorporating data from a view object back into a master object.

```scala
val updatedUser = ScalaJack.spliceInto( user, newSafeUser ) // updatedUser will be of type User
```

# Assumptions

- Case classes (or traits for case classes) only
- Options of value None are removed from generated JSON (e.g. from List or Map)
- Default parameters are not supported at this time
- Primitive/Simple Data types supported:
	- Int
	- Boolean
	- Long
	- Char
	- Double
	- Float
	- String
	- Byte
	- Short
	- java.util.UUID
	- org.joda.time.DateTime
	- Enumeration.Value
	- Value Class
- Collections supported:
	- scala.Option
	- scala.collection.immutable.List
	- scala.collection.immutable.Map
	- scala.collection.immutable.Set
	- scala.collection.immutable.HashMap
	- scala.collection.immutable.HashSet
	- scala.collection.immutable.ListMap
	- scala.collection.immutable.ListSet
	- scala.collection.immutable.Queue
	- scala.collection.immutable.Seq
	- scala.collection.immutable.Vector
	- scala.collection.mutable.ArrayBuffer
	- scala.collection.mutable.ArraySeq
	- scala.collection.mutable.HashMap
	- scala.collection.mutable.HashSet
	- scala.collection.mutable.IndexedSeq
	- scala.collection.mutable.LinearSeq
	- scala.collection.mutable.LinkedHashMap
	- scala.collection.mutable.LinkedHashSeq
	- scala.collection.mutable.ListBuffer
	- scala.collection.mutable.ListMap
	- scala.collection.mutable.Map
	- scala.collection.mutable.MutableList
	- scala.collection.mutable.OpenHashMap
	- scala.collection.mutable.Queue
	- scala.collection.mutable.ResizableArray
	- scala.collection.mutable.Seq
	- scala.collection.mutable.Set
	- scala.collection.mutable.Stack
	- scala.collection.mutable.WeakHashMap

**New for v4.3!** Limited support of Any type is supported.  You can use primitives, Lists[Any], Maps[String,Any], and nested List/Map with Any.  Don't get too cute, though.  Type inference is pretty primal and limited to a few basic types: String, Int, Double, Boolean, and null.  See v4/AnyTests.scala for examples.

# Why?

The natual and expected question when developing a library for a function that already exists in the marketplace is "Why?".  Jackson has its own Scala module, and there is also a wonderful library called Salat that I've been using for years that does JSON parsing for Scala.  What does ScalaJack offer these two don't?

Salat is very full-featured.  It gives you a high level of control over the parsing process including custom serializers for non-standard types.  Unlike a lot of JSON parsers that require "helper" code, and/or lots of annotations, Salat introspects Scala case classes and does it all almost completely automatically.

After using Salat for a long time I began to be curious how its performance stacked up against other JSON parsers.  (In complete fairness, Salat's JSON handling features evolved some time after its primary mission of MongoDB DAO access.)  I discovered Jackson's relatively new Scala module and found it blazing fast, but...  I didn't like the way Enumeration and Option types were handled.  It also didn't handle traits that I could see (serializing Dog and Cat, where both are a case classes extending trait Animal, and the parser can sort them out). It was configurable enough--but required a lot of manual fidgeting with annotations and such.

ScalaJack aimed for Jackson's speed and at least the key parts of Salat's seamless case class handling.  ScalaJack is indeed faster than Salat (about twice as fast!) but losing nearly all of Salat's configurability.  It does handle traits w/type hints seamlessly.  Unlike Salat (at the time of this writing) ScalaJack also supports arbitrary nesting of data structures to allow you to construct sophisticated data structures with ease.

If you're OK with gatining lots of speed at the price of my assumptions, ScalaJack is a great thing!

Blöcke
