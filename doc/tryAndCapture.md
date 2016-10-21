## Try and Capture

There are times when you just don't care.  Try and Capture support is designed to let you control how much you care about the JSON you parse.

#### Case 1: I don't care about some fields and not at all about the rest!
In this case, simply don't represent JSON fields you never care about in your class.  Parsing will harmlessly ignore them.  This also has the benefit of being very flexible if the source JSON changes--as long as they don't change the fields you care about nothing in your system breaks.
```scala
val js = """{"one":"Thing","two":"Another thing","three":"Last thing"}"""
case class ThingsICareAbout(one:String, three:String)
```

#### Case 2: I have an optional field that I care about if it exists.
In this case, represent optionally-present fields as Option type in your class.  If they're present in the JSON, they'll be materialized as Some of something, and None if not.
```scala
val js = """{"one":"Thing","three":"Last thing"}"""
case class ThingsICareAbout(one:String, two:Option[String], three:String)
// materializes: ThingsICareAbout("Thing",None,"LastThing")
```

#### Case 3: There's a field that may or may not parse but I want to keep it
This case is for when you get unreliable or not totally known JSON, perhaps from a 3rd party.  In that JSON is a field you expect to be present (i.e. it's not optional) but you're not 100% sure of its content.  If it can parse you want the value, but if not... you want to keep the original JSON intact as-given, likely because you have a requirement to re-generate the original message format.  

A good use case for this example is if your code is some kind of proxy that receives JSON, makes certain changes and re-emits a modified version of the original JSON.

In this instance we support a Try:

```scala
val js = """{"one":"Thing","two":"another thing","three":"Last thing"}"""
case class ThingsICareAbout(one:String, two:Try[String], three:String)
// materializes: ThingsICareAbout("Thing",Success("another thing","LastThing")

val js2 = """{"one":"Thing","two":true,"three":"Last thing"}"""
val myObj = sj.read[ThingsICareAbout](js)
// materializes: ThingsICareAbout("Thing",Failure("ThingsICareAbout(Thing,Failure(co.blocke.scalajack.UnreadableException: java.lang.IllegalStateException: Expected value token of type String, not True when reading String value.
{"one":"Thing","two":true,"three":"Last thing"}
---------------------^),Last thing)","LastThing")
//
// but...
val rerendered = sj.render(myObj)
// emits: {"one":"Thing","two":true,"three":"Last thing"}
```

Wow!  Did you catch that?  Parsing a Try captured, but didn't throw, the exception (we fed a boolean value into a String).  Behind the scenes, ScalaJack also captured the original JSON, so that even though we couldn't parse it into our class, when the object was rendered, the original JSON was put back into place!

The assumption here is that we're getting 3rd party JSON.  Maybe types change and we can't read them, but we don't want to screw things up for others in the ecosystem that perhaps do expect the given type.

#### Case 4: I don't care about any "extra" fields in the JSON, but don't lose them!
This case is along the lines of Case 3 except that here we don't care about any fields we're not going to use.  For the same reasons (maybe our code is a proxy or pass-through) we may need to re-render all those "don't-care" fields in their original format.

We accomplish this behavior like this:

```scala
case class ThingsICareAbout(one:String) extends SJCapture
val js = """{"one":"thing","id":1234,"isOK":true}"""
val myObj = sj.read[ThingsICareAbout](js)
// myObj = ThingsICareAbout("thing")
println(sj.render(myObj))
// prints: {"one":"thing","id":1234,"isOK":true}
```

Because your class extends SJCapture, all the "extra" incoming JSON fields are quietly captured verbatim and stored until such time as the object is rendered. 

This is ideal for pass-through applications where the JSON formats are unstable.  Your code won't break if changes happen to any of the extra, captured fields.