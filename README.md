# ScalaJack

Very fast JSON parser/generator for Scala case classes using Jackson that requires virtually no annotations
or "helper" code.

# Use

ScalaJack is extremely simple to use.

	import co.blocke.scalajack._

	val js = ScalaJack.render( myCaseObj )  // serialization
	val myObj = ScalaJack.read[MyCaseClass](js) // deserialization

That's about it.  Couldn't be simpler.  There is an optional hint (String) parameter for both render() and read()
that changes the default \_hint field name for trait type hints, if you'd prefer something else.  Don't forget
to use the same type hint name for render and read!

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
object PosixDate extends ExtJson[Long] {
	override def toJson( obj:Long ) : String = "{\"rawTS\":"+obj+",\"human\":\""+ getHumanReadable(obj) +"\"}"
	override def fromJson( valueType:Field, jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[Long]) : Any = {
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


# Assumptions

- Case classes only
- Options of value None are removed from generated JSON
- Default parameters are not supported at this time
- Data types supported: Int, Boolean, Long, Char, Double, Float, String, Enumeration.Value, Value Class
- Collections/"containers" supported: List (immutable), Map (immutable), Option

# Why?

The natual and expected question when developing a library for a function that already exists in the marketplace 
is "Why?".  Jackson has its own Scala module, and there is also a wonderful library called Salat that I've been 
using for years that does JSON parsing for Scala.  What does ScalaJack offer these two don't?

Salat is very full-featured.  It gives you a high level of control over the parsing process including
custom serializers for non-standard types.  Unlike a lot of JSON parsers that require "helper" code, and/or lots
of annotations, Salat introspects Scala case classes and does it all almost completely automatically.

After using Salat for a couple of years I began to be curious how its performance stacked up against other JSON 
parsers.  (In complete fairness, Salat's JSON handling features evolved some time after its primary mission of 
MongoDB DAO access.)  I discovered Jackson's relatively new Scala module and found it blazing fast, but...  I 
didn't like the way Enumeration and Option types were handled.  It also didn't handle traits that I could see 
(serializing Foo, where Foo is a trait member of a case class, and at runtime an object implementing the trait 
is given).  It was configurable enough--but required a lot of manual fidgeting with annotations and such.  

ScalaJack aimed for Jackson's speed and at least the best parts of Salat's behavior.  ScalaJack is indeed faster
than Salat (about twice as fast!) but losing nearly all of Salat's configurability and suffering a couple losses of 
supported datatypes.  It does handle traits w/type hints seamlessly.  Unlike Salat (at the time of this writing)
ScalaJack also supports arbitrary nesting of data structures, Map, List, Option, to allow you to construct
sophisticated data structures with ease.

If you're OK with speed at the price of my assumptions, ScalaJack is a great thing!

Blöcke
