## Converters

As for version 6.2.0, ScalaJack includes a Converters package to add some syntax sugar making it easier to move between wire formats.

### Case 1: Simple conversion between two wire formats
```scala
import co.blocke.scalajack.Converters._
case class Person(name: String, age: Int)

  val js = """{"name":"Greg","age":53}"""
  println(js.jsonToYaml)
```

Note: For Delimited converers (delimitedToJson, delimitedToJson4s, delimitedToYaml, jsonToDelimited, json4sToDelimted, yamltoDelimited), you must specify a type parameter.
This is because Delimited format is so representation-poor, it can't represent a Map required for the conversion.  An example:

```scala
import co.blocke.scalajack.Converters._
case class Person(name: String, age: Int)

  val js = """{"name":"Greg","age":53}"""
  println(js.jsonToDelimited[Person])
```

### Case 2: Map serialized object (same wire format)
```scala
import co.blocke.scalajack.Converters._
case class Person(name: String, age: Int)

  val js = """{"name":"Greg","age":53}"""
  println(js.mapJson[Person](person => person.copy(age=35)))
```

### Case 3: Convert between wire formats while modifying serialized object
```scala
import co.blocke.scalajack.Converters._
case class Person(name: String, age: Int)

  val sjYaml = ScalaJack(YamlFlavor())
  val js = """{"name":"Greg","age":53}"""
  println(js.mapJsonTo[Person,YAML](sjYaml)(person => person.copy(age=35)))
```

### *Bonus:* New to/from convenience implicits

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


