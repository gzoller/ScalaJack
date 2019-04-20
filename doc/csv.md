## Delimited Format (e.g. CSV)

ScalaJack offers support for delimited serialization (e.g. CSV).  It must be said clearly that there are structural limitations to the support available because delimited format is "thin", not supporting advanced nested structures.  Here are some of the features and limitations imposed by ScalaJack on delimited-format serialization:

* Each input string (presumably a line of a larger input file) shall be parsed to one given object
* Map structures are not supported (how would you do key-value in a flat format?)
* Lists are implemented by a nested field, escaped in quotes.  This is handy for very simple lists, but can get messy quickly.  Use with caution!
* Scala Either is supported
* Enumerations are supported
* Traits are **not** supported (where would you put the type hint in CSV format?)
* You must enclose a field with double-qutoes if it contains double quotes, the delimiter character ('comma), or line breaks.
* Double quotes within a string/field must be escaped using double-double quotes, "" (not the more JSON style \")
```scala
val sj = ScalaJack(DelimitedFlavor)  // CSV default
// val sj = ScalaJack(DelimitedFlavor('|'))  <-- to set a non-comma delimiter character
val inst = StringHolder("Hey, you", "This \"life\"", "And now, \"Mike\" will sing.")
val csv = sj.render(inst)
// renders: "Hey, you","This ""life""","And now, ""Mike"" will sing."
```

### Lists
Simple list support is provided.
```scala
val sj = ScalaJack(DelimitedFlavor)
case class Foo(a:Int, b:List[String])
val f = Foo(1,List("a","b","c"))
sj.render(f)
// renders 1,"a,b,c"
```
Note that while clever, list representation can get ugly fast.  Consider the case where the strings in the list need to themselves be escaped because they contain a delimiter character.
```scala
val f = Foo(1,List("a","b,x","c"))
sj.render(f)
// renders 1,"a,""b,x"",c"
```
Not awful, but you can see where the double-quotes could multiply very quickly for any more sophisticated structure.

### Handling None and Null
If an object has a field with value None, then ScalaJack will renders an empty field.  Note this is different than an empty String.  

Nulls are generally not supported.  A null value in your Scala object (please tell me you never have these, right?) will render as an empty field but unless that field is Optional, it will explode with an exception if you try to read it back in.  An empty non-Optionl field in delimited format is considered a missing field and will be treated as such.  The only exception is if the object's field has a default value.  Let's see an example:

Consider the following CSV creation:

```scala
case class Maybe(one:String, two:Option[String], three:Boolean = true)

val sj = ScalaJack(DelimitedFlavor)

val d1 = "foo,bar,false"
sj.read[Maybe](d1) // works fine

val d2 = ",bar,false"
sj.read[Maybe](d2) // explodes with a missing field error--field one is required

val d3 = "foo,,false"
sj.read[Maybe](d3) // works--two is set to None

val d4 = "foo,bar,"
sj.read[Maybe](d4) // works--three set to default value true
```
### Takeaway
Generally it's best to treat delimited format as "fragile", meaning don't get too clever or cute with it.  Use simple, flat or nearly-flat objects and you'll be fine.
