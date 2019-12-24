## map and Conversions

### map Wire Formats
ScalaJack supports a number of serialization wire formats (JSON, YAML, Mongo, etc.).  There may be times you wish to map a serialized object from one format to another.  The map feature allows this to be done conveniently in a Scala style.

```scala
import co.blocke.scalajack.yaml._

  case class Person(name: String, age: Int)

  val sjJson = ScalaJack()
  val sjYaml = ScalaJack(YamlFlavor())

  val js = """{"name":"Greg","age":53}"""

  println(sjJson.map[Person, YAML](js, sjYaml)(_.copy(age = 25)))
```
Usage is pretty simple.  You provite 2 type parameters: the type of the object to be serialized, and the target type (JSON, YAML, DELIMITED, JValue, BsonValue).  You pass the serialized object in the original format and the target flavor of ScalaJack then in the curried function you pass a map function, allowing you to modify the serialized object in-flight.

### Conversions

A set of convenience functions is supplied in the Conversions object.  They are wrappers around the map feature described above, and can be used when you don't want to modify your serialized object in-flight, but merely convert from one format to another.

Note that only core formats have converters (so no mongo for now, although map works with mongo).

```scala
import co.blocke.scalajack.yaml._
import co.blocke.scalajack.Converters._

  case class Person(name: String, age: Int)

  val js = """{"name":"Greg","age":53}"""

  println(js.json2Yaml)
```

Also in the Converters object is an alternate way of serializing using implicits, in case you'd like a different feel than the usual read/render functions.

```scala
import co.blocke.scalajack.Converters._

  trait Human
  case class Person(name: String, age: Int) extends Human

  val js = """{"name":"Greg","age":53}"""

  val person = js.fromJson[Person]
  val yamlWithHint = person.toYaml[Human]
  val yamlWithoutHint = person.toYaml[Person]
```

For these convenience functions you always need to supply the type of the serialized object.

#### Configuration

The Converters can be configured just like the normal ScalaJack flavors:
```scala
import co.blocke.scalajack.Converters._

  trait Human
  case class Person(name: String, age: Int) extends Human

  val js = """{"name":"Greg","age":53}"""

  // Chain together configuration directives like any other flavor
  val config = Configuration().withDefaultHint("kind").enumsAsInt()
  withConfig(config)

  // Now any subsequent Converters activity will use your provided configuration
```


