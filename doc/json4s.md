## Json4s Support
Json4s is a common and convenient way to query and manipulate raw JSON elements.  ScalaJack includes support for Json4s by creating a flavor for it, meaning Json4s is a target serialization format.

```scala
import co.blocke.scalajack.json4s._
val sj = ScalaJack(Json4sFlavor())
```
Then you can use all the usual ScalaJack capabilities.

Json4sFlavor is very handy if you want to do more than straight object  serialization/deserialization, for instance re-process parsed JSON before materializing it into a Scala object.
