
## Trait Hint Type Customization

By default, ScalaJack uses a simple type hint strategy to record concrete types for sealed traits: it inserts a type hint into the rendered JSON object with key "_hint" and value equal to the class simple class name.

This is a pretty good and straightforward way to handle type hints, but there are occasions when you may want something else. Here are several ways you can customize trait type hint handling in ScalaJack.

### Change the Default Hint Label

You can change the global default hint label from "_hint" to whatever else you want using a ScalaJack configuration.

**File1.scala**
```scala
sealed trait Foo{ val bar:Int }
case class Blather(bar:Int) extends  Foo
```

**File2.scala**
```scala
given sjFoo: ScalaJack[Foo] = sjCodecOf[Foo](SJConfig().withTypeHintLabel("kind"))
println(sjFoo.toJson(Blather(2)))
// {"kind":"Blather","bar":2} //<<-- Type hint is now "kind" and not "_hint"
```

### Type Hint Value Customization

We've seen how to change the type hint label, but what if you want something different than the class' simple name as the value? There are two possible use cases for wanting to change the hint value. First, having the class name in the JSON message might be more information than you'd like to share.  ScalaJack provides a way to encode the class name into a value that's not so obvious. This isn't security, by any means, but it is obfuscation and should at least deter curious idiots.  (If what you need is true security, you should be encrypting the entire JSON message anyway.). 

**File1.scala**
```scala
sealed trait Foo{ val bar:Int }
case class Blather(bar:Int) extends  Foo
```

**File2.scala**
```scala
given sjFoo: ScalaJack[Foo] = sjCodecOf[Foo](
  SJConfig().withTypeHintLabel("ref").withTypeHintPolicy(TypeHintPolicy.SCRAMBLE_CLASSNAME))
println(sjFoo.toJson(Blather(2)))
// {"ref":"86999-847-46A","bar":2} //<<-- Type hint is now obfuscated--looks like some kind of id
```

The other use case for modifying type hint values is to support 3rd party JSON that has its own discriminator values that may be different from your class names.  To support that, ScalaJack uses annotations on the class.

**File1.scala**
```scala
sealed trait Foo{ val bar:Int }
@TypeHint(hintValue = "yammer")
case class Blather(bar:Int) extends  Foo
```

**File2.scala**
```scala
given sjFoo: ScalaJack[Foo] = sjCodecOf[Foo](SJConfig().withTypeHintPolicy(TypeHintPolicy.USE_ANNOTATION))
println(sjFoo.toJson(Blather(2)))
// {"_hint":"yammer","bar":2} //<<-- Type hint for Blather is now "yammer", by annotation
```
When using TypeHintPolicy.USE_ANNOTATION, any classes that do not have a @TypeHint annotation will simply revert to the simple class name scheme, so you can mix and match as needed.