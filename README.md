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
to use the same type hint name for render/read!

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
