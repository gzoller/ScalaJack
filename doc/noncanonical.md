## Non-Canonical JSON Support
ScalaJack supports a form of "non-canonical" (non-standard) JSON.  In fact, because it doesn't follow the JSON spec, it isn't really JSON at all, but rather a JSON-like format.

**WARNING:** Use of non-canonical JSON will not be standard or likely even work outside a ScalaJack-to-ScalaJack enivonrment!  You have been warned.

The reason for this feature is purely aesthetic and has to do with JSON keys.  Per the JSON spec, JSON object (aka Map) keys must be Strings, so non-String values are re-worked so that they become Strings.

This is of little consequence for primitive types:

* true becomes "true"
* 123 becomes "123"

And so on.  The aesthetic problems occur if your Map key is a collection or object.  Consciously avoiding the subject of whether using complex objects as Map keys is a wise idea or not, the fact remains that Scala can do it, so ScalaJack needs to support it if it can.

Here's a (canonical) rendering of a Map[List[String],String]:
```scala
val inst = Map(List("a", "b") -> "one", List("c", "d") -> "two")
println(sj.render(inst))
// {"[\"a\",\"b\"]":"one","[\"c\",\"d\"]":"two"}
```
While the output is technically correct, the escaped quotes are a little annoying.  This problem becomes more acute for more complex structures:

```scala
object Food extends Enumeration {
  val Seeds, Meat, Pellets, Veggies = Value
}

val m1 = List(Food.Meat, Food.Veggies)
val m2 = List(Food.Seeds, Food.Pellets)
val inst = Map(Map(m1 -> m2) -> Map(m2 -> m1))
val js = sj.render(inst)
// {"{\"[\\\"Meat\\\",\\\"Veggies\\\"]\":[\"Seeds\",\"Pellets\"]}":{"[\"Seeds\",\"Pellets\"]":["Meat","Veggies"]}}
```

Yikes!  That's a whole lot of escaping going on!  Sadly it's all entirely necessary to properly unwrap these serialized complex objects.

**PAUSE!** Before continuing, consider whether you care.  Really this ugliness only matters if this JSON will be read by people, right?

Consider the same example with the non-canonical feature enabled:

```scala
val sj = ScalaJack().isCanonical(false)
val m1 = List(Food.Meat, Food.Veggies)
val m2 = List(Food.Seeds, Food.Pellets)
val inst = Map(Map(m1 -> m2) -> Map(m2 -> m1))
val js = sj.render(inst)
// {{["Meat","Veggies"]:["Seeds","Pellets"]}:{["Seeds","Pellets"]:["Meat","Veggies"]}}
```

Sweet, right?  Basically non-canonical JSON elevates non-String objects to first-class key values, so wrapping (and escaping) as String is unnecessary.

If JSON had been conceived as a true object serialization specification, perhaps this behavior would have been standard, but that's not what we have.  ScalaJack's non-canonical feature allows you to clean up your JSON if you're prepared to accept that it is now no longer really JSON, and will most certainly be considered invalid by any other JSON parser except for ScalaJack.