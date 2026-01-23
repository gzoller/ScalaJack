
## JSON Defaults

ScalaJack supports the idea of JSON default values. Imagine you had a class:

```scala
case class Foo(x: Int)
```

Later you need to modify this class to:

```scala
case class Foo(x: Int, y: String)
```

Imagine you have a body of stored JSON objects representing serailized instances of your original Foo. If you try to parse them
with the new Foo, ScalaJack will crash because required field y is not present.

To solve this problem you *could* just make any new fields Optional, but that's messy. Instead, ScalaJack
provides the ability to choose harmless defaults:

```scala
given sjFoo: ScalaJack[Foo] = ScalaJack.sjCodecOf[Foo]
given JsonDefault[String] = JsonDefault("")

val js = """{"x":5}"""
val inst = sjFoo.fromJson(js)
// inst == Foo(5, "")
```