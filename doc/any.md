
## Any Support

Scala has the Any type. ScalaJack supports Any but you should be aware of its special needs and limitations.

A value of type Any means ScalaJack has no specific idea what the type should be, and must therefore infer the type as best it can. This is necessarily an imperfect process, but we can describe the assumptions it uses here. The main takeaway is this: Virtually everywhere else, if you render an object with ScalaJack to JSON, then read that object back in, you should get the original object. This is often untrue with Any-typed data, and we'll see examples of this.

Let's use a simple sample:
**File1.scala** (Remember: Class definitions must be in a separate file than where the ScalaJack macros are called!)
```scala
package com.me

case  class  Amorphous(thing: Any)
case  class  Small(num:Int)
```
**File2.scala**
```scala
given sjAmorphousList: ScalaJack[List[Amorphous]] = sjCodecOf[List[Amorphous]]

val  all = List(
  Amorphous(true),
  Amorphous("blather"),
  Amorphous(1.234),
  Amorphous(List(1,2,3)),
  Amorphous(Map("a"->1,"b"->2)),
  Amorphous(null),
  Amorphous(Small(99))
)

sjAmorphousList.toJson(all)
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
  {"thing":{"num":99}}
]
```
So far, so good, right? All the values seem to be rendered well.  Now it gets a bit more complicated...

### Classes and Maps

You can see from the sample above that when an Any-typed value is populated with an object, it is rendered like any class.  However there's no way for ScalaJack to know what class to reconstruct when parsing an Any value.  Any JSON value in braces '{ }' will be presumed to be a Map, regardless of what they actually were when the JSON was rendered.

### Options

Option[] values cannot be inferred as an Any value. Rendering Some(thing) would always be read as simply 'thing'. ScalaJack has no information this value was originally an Option, in order to wrap it in Some().
  

### Numbers

When reading a numerical value, ScalaJack must infer what kind of numerical type to use. There's no right/wrong answer here, so ScalaJack uses a simple fitting mechanism. If ScalaJack's parser detects a numerical value for Any, it reads it in as a BigDecimal value, then attempts to "fit" the number in this order:

1. Int if value is a valid Int
2. Long if value is a valid Long
3. Double if value is a valid Double
4. BigInt if value is valid BigInt
5. otherwise leave it as BigDecimal

>**Remember that when processing Any, there is no wrong answer--any returned value, in any type, is a valid Any value. There's just expected and unexpected on your part.**