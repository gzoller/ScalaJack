# ScalaJack

Fast JSON parser/generator for Scala using Jackson

The 1-liner description is what it is. 

# Use

ScalaJack is very simple to use.

	import co.nubilus.scalajack._

	val js = ScalaJack.render( myCaseObj )  // serialization
	val myObj = ScalaJack.read[MyCaseClass](js) // deserialization

That's about it.  There is an included annotation for MongoDB JSON.  Consider a case class:

	case class Person(
		@MongoKey name : String,
		age : Int
		)

If I'm sending a Person object to, say, a RESTful service, I want the name and age fields labeled.

	{"name":"Fred","age":28}

But, if I'm sending this object to Mongo, and if name is the primary key, I want to transform 'name' into
'_id', Mongo's default id key.  Adding a MongoKey annotation on the object's key field does this transformation
if I also pass true as a second parameter (forMongo) to render:

	val js = ScalaJack.render( myCaseObj, true )

# Assumptions

- Case classes only
- Options of value None are removed from generated JSON
- Default parameters are not supported at this time
- Simple types supported: Int, Boolean, Long, String, Enumeration.Value
- Collections/"containers" supported: List (immutable), Map (immutable), Option
- MongoKey-decorated fields must be type String and map to "_id" (Mongo's default)

# Why?

The natual and expected question when developing a library that already exists in the marketplace is "Why?".  
Jackson has its own Scala module, and there is also a wonderful library called Salat that I've been using for
some time that does JSON parsing for Scala.  What does ScalaJack offer these two don't?

Salat is very full-featured.  It gives you a high level of control over the parsing process including
custom serializers for non-standard types.  After using it for a couple of years I began to wonder if it might
be a bit slow for high-throughput needs.  In fairness, Salat's JSON handling features evolved after its primary
mission of MongoDB DAO access.

I discovered Jackson's relatively new Scala module and found it blazing fast, but...  I didn't like the way
Enumeration and Option types were handled.  It also didn't handle traits that I could see (serializing Foo, where 
Foo is a trait member of a case class, and at runtime an object implementing the trait is given).  Salat does
this very well, and seamlessly, via an embedded type hint.

ScalaJack aimed for Jackson's speed and at least the best parts of Salat's behavior.  ScalaJack is indeed faster
than Salat but losing nearly all of Salat's configurability and suffering a couple losses of datatypes supported.
It does handle traits w/type hints nicely.