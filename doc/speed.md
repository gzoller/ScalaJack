## Gimme Speed

ScalaJack heavily uses reflection to provide seamless and easy serialization.  This is great, but reflection isn't known for speed.

Oftentimes programs are serializing the same type of objects again and again.  If this is your use case then ScalaJack has an optimization that can boost your speed 20+%.

```scala
case class Person(...)

val sj = ScalaJack()

// You can now read/render different types.  But...

val forPerson = sj.forType[Person]
forPerson.read(someJson) // returns Person.  Note no need to do sj.read[Person](json)
forPerson.render(Person(...))
```

forType returns a type-locked flavor of ScalaJack, wired to just the one type you specify.  Then when you call read or render you won't need to specify the type.

This looks like a minor syntatic clean-up but what this is doing is avoiding repeated calls to reflection code, achieving a performance boost.

Use this approach whenever you're reading/rendering the same type of object again and again.  (Of course the usual "unlocked" read/render work as usual as well if you are serializing many types of objects.)