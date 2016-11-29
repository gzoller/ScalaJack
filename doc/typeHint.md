## Trait Hint Type Customization

By default, ScalaJack uses a simple type hint strategy to record concrete types for traits: it inserts a type hint into the rendered JSON object with key "_hint" and value equal to the fully-qualified class name.

This is a pretty good and straightforward way to handle type hints, but there are occasions when you may want something else.  For example when you're dealing with 3rd party JSON.  In those cases you may not control, or wish to share, the full class name of your objects.

Here are 3 ways you can customize trait type hint handling in ScalaJack.

#### Change the Default Hint Label
This is how you can change the hint label from _hint to whatever else you want:

```scala
package com.me

trait Foo{ val bar:Int }
case class Blather(bar:Int) extends Foo

val sj = ScalaJack().withDefaultHint("kind")
println(sj.render[Foo](Blather(2)))
// {"kind":"com.me.Blather","bar":2}
```
You'll see the normal _hint is replaced by "kind".

#### Class-Specific Hint Label Customization
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

#### Type Hint Value Customization
This feature let's you modify the fully-qualified class name, by some function of your own design, to another value.  This is ideal when you don't want to expose the class name in JSON.

There are two common cases ScalaJack abstracts for your ease of use:  ClassNameHintModifier and StringMatchHintModifier.  (You are certainly not limited to just these two approaches--you can use any logic you want--but these cover common use cases.)

The ClassNameHintModifier re-writes the class name itself.  You pass in a function that accepts the fully-qualified classname; your logic does whatever it needs to do; and the output is a new value for the type hint.

```scala
val prependHintMod = ClassNameHintModifier((hint: String) => "com.me." + hint, (cname: String) => cname.split('.').last)
val sj = ScalaJack().withHintModifiers((typeOf[Foo], prependHintMod))
val inst:Bar = Two(3, One(2))
println(sj.render(inst))
// {"_hint":"com.me.Two","b":3,"c":{"_hint":"One","a":2}}
```

We've first used ClassNameHintModifier to specify 2 functions:  value->classname, classname->value.  In our case it adds or strips the packages from the classname leaving just the last part of the class path, or "One" in this case.  Reading re-inserts the path: "One" -> "com.me.One".

Then we've hooked that modifier to a particular type, Foo in our case.

The other hint value modifier is StringMatchHintModifier:

```scala
val strMatchHintMod = StringMatchHintModifier(Map("Yes" -> typeOf[One]))
val sj = ScalaJack().withHintModifiers((typeOf[Foo], strMatchHintMod))
val inst:Bar = Two(3, One(2))
println(sj.render(inst))
// {"_hint":"com.me.Two","b":3,"c":{"_hint":"Yes","a":2}}
```
In this example we just mapped the type of the concrete class to some string value (vs modifying the text of the class name itself).  Note a key point here--The StringMatchHindModifier is a map of String->*concrete class type*!  When hooking the adapter into ScalaJack you use the trait type (Foo).

#### Pulling it Together
Using these hint modification methods together is where the power really comes through.  A great case for this is when you're parsing 3rd party JSON you don't control.  If there's a "variant" object that could take one of several concrete forms there's bound to be some field that behaves as a discriminator.  That field will likely not be labeled "_hint", and the value will not be a class name!

In that case a StringMatchHintModifier is a great choice, along with a .withHints mapping so you can wrangle what you're given into what you need to read the correct concrete class in your code.

```scala
val strMatchHintMod = StringMatchHintModifier(Map("ace" -> typeOf[One], "big" -> typeOf[Two]))
val sj = ScalaJack()
  .withHintModifiers((typeOf[Foo] -> strMatchHintMod), (typeOf[Bar] -> strMatchHintMod))
  .withHints((typeOf[Foo] -> "mark"),(typeOf[Bar] -> "size"))
val inst:Bar = Two(3, One(2))
println(sj.render(inst))
// {"size":"big","b":3,"c":{"mark":"ace","a":2}}
```
Wow!  Now there's nothing left in the rendered (and read) JSON that remotely looks like a normal ScalaJack type hint.  Both the labels and the values have been modified, which is entirely suitable for 3rd party JSON.
