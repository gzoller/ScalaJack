## Try and Capture

There are times when you just don't care.  Try and Capture support is designed to let you control how much you care about the input you parse.

### Case 1: I care about some fields and not at all about the rest!
In this case, simply don't represent JSON fields you never care about in your class.  Parsing will harmlessly ignore them.  This also has the benefit of being very flexible if the source JSON changes--as long as they don't change the fields you care about nothing in your system breaks.
```scala
val js = """{"one":"Thing","two":"Another thing","three":"Last thing"}"""
case class ThingsICareAbout(one:String, three:String)  // field two is ignored
```
Note that fields ignored on read, because they're not present in your class, will not be output upon render.  (If you don't care about certain fields, but don't want to lose them on render ScalaJack has a mechanism to preserve this knowledge we'll see shortly.)

### Case 2: I have an optional field that I care about if it exists.
In this case, represent optionally-present fields as Option type in your class.  If they're present in the JSON, they'll be materialized as Some of something, and None if not.
```scala
val js = """{"one":"Thing","three":"Last thing"}"""
case class ThingsICareAbout(one:String, two:Option[String], three:String)
// materializes: ThingsICareAbout("Thing",None,"LastThing")
```
In this case Option fields who's value is None will not be rendered, but if their value is Some(something) then output will be rendered.

### Case 3: There's a field that may or may not parse but I want to keep it
This case is for when you get unreliable JSON, likely from a 3rd party.  In that JSON is a field you expect and require to be present (i.e. it's not optional) but you're not 100% sure of its content or proper formatting.  If it can parse you want the value, but if not... you want to keep the original JSON intact as-given, because you have a requirement to re-generate the original message format.  

An example of this case is if your code is some kind of proxy that receives JSON, makes certain changes and re-emits a modified version of the original JSON.

In this instance we support a Try:

```scala
val js = """{"one":"Thing","two":"another thing","three":"Last thing"}"""

case class ThingsICareAbout(one:String, two:Try[String], three:String) // <-- Note the Try!

// materializes: ThingsICareAbout("Thing",Success("another thing"),"LastThing")

val js2 = """{"one":"Thing","two":true,"three":"Last thing"}"""
val myObj = sj.read[ThingsICareAbout](js2)

/*
Oops!  Materializes:
ThingsICareAbout(Thing,Failure(co.blocke.scalajack.typeadapter.ValueBackedException: [$.two]: Expected String here but found Boolean
{"one":"Thing","two":true,"three":"Last thing"}
------------------------^),Last thing)
*/

//
// but...
val rerendered = sj.render(myObj)
// emits: {"one":"Thing","two":true,"three":"Last thing"}
```

Wow!  Did you catch that?  Parsing a Try captured, but didn't throw, the ValueBackedException (we fed a boolean value into a String).  Behind the scenes, ScalaJack also captured the original JSON, so that even though we couldn't parse it into our class, when the object was rendered, the original JSON was put back into place!

The assumption here is that we're getting 3rd party JSON.  Maybe types change unexpectedly and we can't read them but we don't want to screw things up for others in the ecosystem that perhaps do expect the given type.

### Case 4: I don't care about any "extra" fields in the JSON, but please don't lose them!
This case is along the lines of Case 3 except that here we don't care about any fields we're not going to use.  For the same reasons (maybe our code is a proxy or pass-through) we may need to re-render all those "don't-care" fields in their original format.

We accomplish this behavior like this:

```scala
case class ThingsICareAbout(one:String) extends SJCapture  // <-- Note the extends SJCapture

val js = """{"one":"thing","id":1234,"isOK":true}"""
val myObj = sj.read[ThingsICareAbout](js)
// myObj = ThingsICareAbout("thing")

println(sj.render(myObj))
// prints: {"one":"thing","id":1234,"isOK":true}
```

Because your class extends SJCapture, all the "extra" incoming JSON fields are quietly captured and stored until the object is rendered.

This is ideal for pass-through applications where the JSON formats are unstable or you are only concerned about a subset of fields as you pass through the object.  Your code won't break if changes happen to any of the extra, captured fields.
