## Any Support

![Maven Central](https://img.shields.io/maven-central/v/co.blocke/scalajack)
![Maven Central](https://maven-badges.herokuapp.com/maven-central/co.blocke/scalajack_3.0.0-RC1/badge.svg)

Scala has the concept of Any.  ScalaJack supports Any but you should be aware of its special needs and limitations.

A value of type Any means ScalaJack has no specific idea what the type should be, and must therefore infer the type as best it can.  This is necessarily an imperfect process but we can describe the process it uses here.  The main takeaway is this:  Virtually everywhere else, if you render an object with ScalaJack to JSON, then read that object back in, you should get the original object.  This isn't often true with Any-typed data, and we'll see examples of this.

Let's use a simple sample:

```scala
package com.me

case class Amorphous(thing: Any)
case class Small(num:Int)

val all = List(
  Amorphous(true),
  Amorphous("blather"),
  Amorphous(1.234),
  Amorphous(List(1,2,3)),
  Amorphous(Map("a"->1,"b"->2)),
  Amorphous(null),
  Amorphous(Small(99))
  )

sj.render(all)
```

This renders:

```JSON
[
  {"thing":true},
  {"thing":"blather"},
  {"thing":1.234},
  {"thing":[1,2,3]},
  {"thing":{"a":1,"b":2}},
  {"thing":null},
  {"thing":{"_hint":"com.me.Small","num":99}}
]
```
So far, so good, right?  It gets a bit more complicated... 

### Classes and Maps
You can see from the sample above that when an Any-typed value is populated with an object, it is rendered like a trait, with its type hint (only the default type hint "_hint" is supported for now).  This is so ScalaJack knows what class to materialize upon reading this JSON.

Without a type hint, the JSON object will be inferred to be just a key/value Map in Scala.

**Note:** Option[] values cannot be inferred as an Any value.  Rendering Some(thing) would always be read as thing.  ScalaJack would never be able to infer Some(thing) vs thing from JSON.

### Numbers
When reading a numerical value, ScalaJack must infer what kind of numerical type to use.  There's no right/wrong answer here, so ScalaJack uses a simple fitting mechanism.  The fittings are shown in the table below, but one important thing to keep in mind: If you render an Any numerical value and read it back in, the value read in may be a different type than you rendered!  ScalaJack takes great pains to try to ensure a read object matches the rendered original, but for Any this promise is not always possible to keep.

|Scala render() Type for Any|ScalaJack read() Type for Any|
|-------|-------|
|Byte |Long
|Short |Long
|Int |Long
|Long |Long
|BigInt |Long if it fits, else BigInt
|Float |Double
|Double |Double
|BigDecimal |Double if it fits, else BigDecimal

>**Remember that when processing Any, there is no wrong answer--any returned value, in any type, is an Any!  There's just expected and unexpected on your part.**
