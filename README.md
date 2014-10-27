# ScalaJack

Very fast JSON parser/generator for Scala case classes using Jackson that requires virtually no annotations
or "helper" code.

# Use

ScalaJack is extremely simple to use.

Include it in your projects by adding the following to your build.sbt:

	libraryDependencies ++= Seq("co.blocke" %% "scalajack" % "3.0.0")
    
If you want to use the optional MongoDB serialization support include this as well:

	libraryDependencies ++= Seq("co.blocke" %% "scalajack_mongo" % "3.0.0")

You may need to  add the OSS repo to your resolvers if its not already present:

	resolvers ++= Seq("OSS" at "http://oss.sonatype.org/content/repositories/releases")

Now you're good to go!  Let's use ScalaJack in your project to serialize/de-serialize a case class object into JSON:

	import co.blocke.scalajack._

	val js = ScalaJack.render( myCaseObj )  // serialization
	val myObj = ScalaJack.read[MyCaseClass](js) // deserialization

Couldn't be simpler.  

### Trait support
ScalaJack can handle traits too.  To do this you'll need the help of a type hint in the JSON.  This tells ScalaJack what actual class to create in support of the trait.  The default type hint is "_hint" but you can set whatever you want (very powerful for 3rd party JSON!)Don't forget to use the same type hint name for render and read!

```scala
package com.myproj

trait Pet { val name:String }
case class Dog( name:String ) extends Pet
case class Cat( name:String ) extends Pet

val p : Pet = Dog("Fido")
val js = ScalaJack.render(p)  // {"_hint":"com.myproj.Dog","name":"Fido"}
val js2 = ScalaJack.render(p,"kind") // {"kind":"com.myproj.Dog","name":"Fido"}

// Be sure to match type hints when reading!
val d1 = ScalaJack.render(js)
val d2 = ScalaJack.render(js2,"kind")
```

# Custom Value Class JSON

Many JSON parsers have the ability to perform custom generations/parsings for fields or data types.  Adding 
this feature in the conventional way would slow down the parser quite a lot, so ScalaJack does something a little
different using an optional "mode" for value classes.  Let's demonstrate via an example.

Let's imagine we have a value class PosixDate that has a long as its single value type, holding a Unix timestamp.
Let's further assume we use this class in a case class for server stats like this:

```scala
class PosixDate( val ts:Long = (new Date()).getTime/1000 ) extends AnyVal {
	def toDate : Date = new Date(ts*1000)
	def isBefore( pd:PosixDate ) = this.ts < pd.ts
	def isAfter( pd:PosixDate )  = this.ts > pd.ts
	override def toString() = ts.toString
}

case class ServerStats( instanceName:String, upSince:PosixDate )
```
Good enough.  Now let's render some JSON:

```scala
val ss = ServerStats( "admin", new PosixDate() )
val js = ScalaJack.render( ss )  // Output: {"instanceName":"admin","upSince":1383317215}
```

For my use, though, I might want a human-readable timestamp too, like "5 hours ago". There are libraries that do
this sort of thing but I want to extend my rendered JSON to incorporate it.  This is possible using an extension
trait on a companion object to your case class like this:  (Sounds worse than it is!)

```scala
object PosixDate extends ExtJson {
	override def toJson( obj:Long ) : String = "{\"rawTS\":" 
		+ obj.asInstanceOf[Long]
		+ ",\"human\":\""+ getHumanReadable(obj.asInstanceOf[Long]) 
		+ "\"}"
	override def fromJson( valueType:Field, jp:JsonParser, ext:Boolean, hint:String ) : Any = {
		jp.nextToken // consume '{'
		jp.getCurrentName // consume 'rawTS' label
		jp.nextToken // scan to value
		val v = jp.getValueAsLong // consume 'rawTS' value
		while( jp.getCurrentToken != JsonToken.END_OBJECT ) { // skip the rest
			jp.nextToken
		}
		jp.nextToken // consume '}'...ready for next parsed field
		v  // return created value
	}

val js2 = ScalaJack.render( ss, true )  // Output: {"instanceName":"admin","upSince":{"rawTS":1383317215,"human":"5 hours ago"}}
```

The ExtJson trait defines toJson (for render) and fromJson (for read).  Note that all the types defined are the
data type of your value class' data element, Long in this case.  The toJson function simply renders whatever JSON you want.
If you need the ability to parse it back in, it would be necessary to render enough information so that you can 
reconstitute the object from JSON, which is exactly what fromJson does.  fromJson uses Jackson parser calls to step
through your custom JSON and do "whatever ya gotta do" to build an instance of your object.  The only hard rule of thumb
for fromJson is that you must leave the parser at a clean point--ready to parse the next field's tokens, i.e. don't stop
consuming tokens half way through your custom JSON object!

If you only need custom rendering (i.e. you have no intention of ever reading your custom-rendered JSON) you may simply
omit the fromJson function in your object.

Seem like a strange feature?  This facility gives you the possibility of dynamic, on-the-fly json creation/type conversion
in a lightweight/fast manner.

# MongoDB (Casbah) Persistence

ScalaJack doesn't wrap MongoDB or Casbah persistence libraries--that's not its mission.  It does provide a way to convert case classes (and traits)to/from DBObjects.  You'll need to include the mongo support package:

```scala
import co.blocke.scalajack._
import mongo._

val mydbo  = ScalaJack.renderDB( myCaseClass )         // default type hint for traits
val mydbo2 = ScalaJack.renderDB( myCaseClass, "_dt" )  // custom type hint for traits
val myCC   = ScalaJack.readDB( mydbo ) 
val myCC2  = ScalaJack.readDB( mydbo, "_dt" ) 
```

There is also a way to specify the MongoDB key field (_id) via an annotation:

```scala
case class Sample( @MongoKey lastName:String, birthDate:Long, hobbies:List[String] )
```

Compound keys are also supported:

```scala
case class Sample( @MongoKey lastName:String, @MongoKey birthDate:Long, hobbies:List[String] )
```

Support has been added for Mongo's ObjectId type if you wish to use this directly.

```scala
case class Sample( _id:ObjectId, stuff:Int )
```

Once you have your DBObject, use Casbah or MongoDB's native Java APIs as you normally would.  At this time there's no fancy
JodaTime support as found in other libraries, although this may be considered for a future release.

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
- Data types supported: Int, Boolean, Long, Char, Double, Float, String, Enumeration.Value, Value Class
- Collections supported: List (immutable), Map (immutable), Option

# Why?

The natual and expected question when developing a library for a function that already exists in the marketplace 
is "Why?".  Jackson has its own Scala module, and there is also a wonderful library called Salat that I've been 
using for years that does JSON parsing for Scala.  What does ScalaJack offer these two don't?

Salat is very full-featured.  It gives you a high level of control over the parsing process including
custom serializers for non-standard types.  Unlike a lot of JSON parsers that require "helper" code, and/or lots
of annotations, Salat introspects Scala case classes and does it all almost completely automatically.

After using Salat for a long time I began to be curious how its performance stacked up against other JSON 
parsers.  (In complete fairness, Salat's JSON handling features evolved some time after its primary mission of 
MongoDB DAO access.)  I discovered Jackson's relatively new Scala module and found it blazing fast, but...  I 
didn't like the way Enumeration and Option types were handled.  It also didn't handle traits that I could see 
(serializing Dog and Cat, where both are a case classes extending trait Animal, and the parser can sort them out).
It was configurable enough--but required a lot of manual fidgeting with annotations and such.  

ScalaJack aimed for Jackson's speed and at least the key parts of Salat's seamless case class handling.  ScalaJack 
is indeed faster than Salat (about twice as fast!) but losing nearly all of Salat's configurability and suffering 
a couple losses of supported datatypes.  It does handle traits w/type hints seamlessly.  Unlike Salat (at the time 
of this writing) ScalaJack also supports arbitrary nesting of data structures, Map, List, Option, to allow you to 
construct sophisticated data structures with ease.

If you're OK with gatining lots of speed at the price of my assumptions, ScalaJack is a great thing!

Blöcke
