## Union Types
JSON can be messy.  Very messy.  Take Avro schema file definitions, for example.  An Avro "type" field can have 1 of 4 possible values:
1) An ennumeration value of a simple type
2) A symbol (string) of a named complex type
3) An embedded complex type (JSON object)
4) A union of a list of any of the above

Great, eh?  Definitely not something easy to deserialize.  Ideally, Scala would have built-in support for union types, or a type that can have 1 of several types of values.  Several 3rd party libraries, e.g. Cats, glom this feature into Scala with varying degrees of success.  The problem is that because these are not language-native ScalaJack's reflection engine doesn't know how to unpack them.

The solution is the ScalaJack Union type, which is a caveman-primitive implementation consisting of several typed fields, all of which are optional, and only one specified.  Yuck, right?

Looks like this:

```scala
import co.blocke.scalajack.Union3
case class Multi3(one: Union3[List[String], List[Int], Boolean])
val m3 = Multi3(Union3(None,Some(List(1,2,3)),None))
```
In this case we have a Union3, which can be of type List[String], List[Int], or Boolean.

ScalaJack defines 3 Union types: Union2, Union3, and Union4.

Not an elegant solution at all, but it does work if you have 3rd party JSON that may be well-formed, but that doesn't follow the strict rules of typed object serialization.

> Call For Proposal: If anyone can suggest or contribute a more elegant way of accomplishing this goal, please submit a PR!
