## YAML Support

YAML is now supported as a flavor for ScalaJack, so you can serialize your Scala artifacts directly to/from YAML documents.

All the normal trait-handling, type hints, modifiers, etc. that apply in JSON apply in YAML too, so you lose no flexibility.

With YAML you gain the ability to natively support non-String (i.e. complex) Map keys.  ScalaJack's JSON flavor allowed this too, but required serializing the non-String key into a String, which frankly is a bit messy.  No need for that anymore with YAML!

One thing that is different for YAML is there are no permissive primitives settings (.withPermissivePrimitives), for example "true" = true, "123" = 123.  In YAML, permissive primitive behavior is default unless you quote your values.

Usage is as you'd expect:
```scala
import co.blocke.scalajack.yaml._

case class Person(...)

val sj = ScalaJack(YamlFlavor())
sj.render(Person("Fred",34))
val myPerson = sj.read[Person](someYaml)
```
One limitation:  ScalaJack's YAML parser does not handle multi-document YAML input, so basically one top-level class is parsed.  If that is a huge need, open an issue and we'll see what we can do.