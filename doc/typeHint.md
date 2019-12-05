## Trait Hint Type Customization

By default, ScalaJack uses a simple type hint strategy to record concrete types for traits: it inserts a type hint into the rendered JSON object with key "_hint" and value equal to the fully-qualified class name.

This is a pretty good and straightforward way to handle type hints, but there are occasions when you may want something else.  For example when you're dealing with 3rd party JSON.  In those cases you may not control, or wish to share, the full class name of your objects.

Here are 3 ways you can customize trait type hint handling in ScalaJack.

### Change the Default Hint Label
You can change the global default hint label from "_hint" to whatever else you want using a ScalaJack configuration.

```scala
package com.me

trait Foo{ val bar:Int }
case class Blather(bar:Int) extends Foo

val sj = ScalaJack().withDefaultHint("kind")  //<<-- Note the config here

println(sj.render[Foo](Blather(2)))
// {"kind":"com.me.Blather","bar":2}   //<<-- Type hint is now "kind"
```
You'll see the normal _hint is replaced by "kind".

### Class-Specific Hint Label Customization
This is a more granular control of the first case, which allows you to change the type hint on a class-by-class basis (you can still override the default case for classes not mapped using this method):

```scala
package com.me
import scala.reflect.runtime.universe.typeOf

trait Foo{ val a:Int }
trait Bar{ val b:Int; val c:Foo }
case class One(a:Int) extends Foo
case class Two(b:Int, c:Foo) extends Bar

val sj = ScalaJack().withHints((typeOf[Foo] -> "mark")).withDefaultHint("kind")

val inst:Bar = Two(3, One(2))
println(sj.render(inst))
// {"kind":"com.me.Two","b":3,"c":{"mark":"com.me.One","a":2}}
```
Here we've shown a type-specific change to the type hint and a general override of the default type hint.

(Hint: typeOf[SomeClass] is an easy way to get the type of something.  This works for parameterized types too, e.g. typeOf[Vehicle[Person]].)

### Type Hint Value Customization
We've seen how to change the type hint label, but what if you want to modify the behavior of using the fully-qualified class name to use another value instead?  This is ideal when you don't want to expose the class name in JSON or 3rd party consumers of your output expect something else.  ScalaJack has the concept of a HintValueModifier for this purpose.

ScalaJack suppies two pre-built HintValueModifiers out of-the-box:  ClassNameHintModifier and StringMatchHintModifier.  (You are certainly not limited to just these two--you can write your own HintValueModifiers to do whatever you want, but these cover some common use cases.)

The ClassNameHintModifier re-writes the class name itself.  You pass in 2 functions: the first accepts a simple hint string and your function produces a fully-qualified class name, and the second accepts the fully-qualified class name and produces the simple hint string.

```scala
val prependHintMod = model.ClassNameHintModifier((hint: String) => "com.me." + hint, (cname: String) => cname.split('.').last)
val sj = ScalaJack().withHintModifiers((typeOf[Foo], prependHintMod))
val inst:Bar = Two(3, One(2))
println(sj.render(inst))
// {"_hint":"com.me.Two","b":3,"c":{"_hint":"One","a":2}}
```

We've first used ClassNameHintModifier to specify 2 functions:  value->classname, classname->value.  In our case it adds/strips the packages from the classname leaving just the last part of the class path, or "One" in this case.  Reading re-inserts the path: "One" -> "com.me.One".  Then we've associated that modifier to a particular type, Foo in our case.

The other provided HintValueModifier is StringMatchHintModifier:

```scala
val strMatchHintMod = model.StringMatchHintModifier(Map("Yes" -> typeOf[One]))
val sj = ScalaJack().withHintModifiers((typeOf[Foo], strMatchHintMod))
val inst:Bar = Two(3, One(2))
println(sj.render(inst))
// {"_hint":"com.me.Two","b":3,"c":{"_hint":"Yes","a":2}}
```
In this example we mapped the type of the concrete class to some string value (vs modifying the text of the class name itself).  Note a key point here--The StringMatchHindModifier is a map of String->*concrete class type*.  


### Pulling it Together
Using these hint modification methods together is where the power becomes apparent.  A great case for this is when you're parsing 3rd party JSON you don't control.  If there's a "variant" object that could take one of several concrete forms there's bound to be some field that behaves as a discriminator.  That field will likely not be labeled "_hint", and the value will not be a class name.

In that case a StringMatchHintModifier is a great choice, along with a .withHints mapping so you can wrangle what you're given into what you need to read the correct concrete class in your code.

```scala
val cardHints = StringMatchHintModifier(Map("ace" -> typeOf[PlayerAce], "king" -> typeOf[PlayerKing]))
val sizeHints = StringMatchHintModifier(Map("small" -> typeOf[Ant], "big" -> typeOf[Elephant]))
val sj = ScalaJack()
  .withHintModifiers((typeOf[Card] -> cardHints), (typeOf[Animal] -> sizeHints))
  .withHints((typeOf[Card] -> "card"),(typeOf[Animal] -> "size"))
val inst:Animal = Elephant(3, PlayerAce(2))
println(sj.render(inst))
// {"size":"big","b":3,"c":{"card":"ace","a":2}}
```
Wow!  Now there's nothing left in the rendered (and read) JSON that remotely looks like a normal ScalaJack type hint.  Both the labels and the values have been modified, which is entirely suitable for 3rd party JSON.
