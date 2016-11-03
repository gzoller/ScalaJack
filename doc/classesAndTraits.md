## Case Classes and Traits

Parsing and serializing case classes is as easy as it gets with ScalaJack.  Serializing is simply a call to render():

```scala
case class Person(name:String, age:Int)
val sj = ScalaJack()
val js = sj.render(Person("Mike",32))
// renders {"name":"Mike","age":32}
```

All basic Scala primitive data types are support in addition to Java primitives and Java 8's java.time classes.  Collections are supported as well.

Of course you can nest collections and case classes as you like:

```scala
case class Hobby(desc:String, cost:Double)
case class Person(name:String, age:Int, hobbies:List[Hobby])
sj.render(Person("Mike",32,List(Hobby("surfing",1000.0))))
// renders {"name":"Mike","age":32,"hobbies":[{"desc":"surfing","cost":1000.0}]}
```

Parsing JSON into a case class is similarly easy, with the addition of a given type parameter to tell ScalaJack what kind of class you're trying to construct:

```scala
val js = """{"name":"Mike","age":32}"""
val person = sj.read[Person](js)
```

This works for collections too:

```scala
val js = """[
{"name":"Mike","age":32},
{"name":"Sarah","age":29}
]"""
val person = sj.read[List[Person]](js)

val js2 = """{
"surfers":[{"name":"Mike","age":32}]
}"""
val byHobby = sj.read[Map[String,List[Person]]](js)
```
So you can see the combinations can be as complex as you need.

Traits work the same way but with the small addition of a type hint.  This is to tell ScalaJack what concrete class to construct from a given trait type.  Consider this example:

```scala
package com.me
trait Pet{ val name:String; val numLegs:Int }
case class Dog(name:String, numLegs:Int) extends Pet
val inst:Pet = Dog("Fido",4)
sj.render[Pet](inst)
// renders {"_hint":"com.me.Dog","name":"Fido","numLegs":4}
```
Ok, there's a few things going on here.  Let's work backwards.  Notice the rendered JSON has a type hint.  That's because you rendered a Pet, not a Dog.  Without the hint, ScalaJack has no idea that this Pet should, in fact, be a Dog when read back in, so we have to leave it a hint.

Notice too that when rendering we now specify expressly the [Pet] trait type.  We didn't do that when rendering a concrete class.  This is to prompt ScalaJack to generate the type hint it will need later.

Reading a properly serialized trait back in is simple:
```scala
val js = """{"_hint":"com.me.Dog","name":"Fido","numLegs":4}"""
sj.read[Pet](js)
```
The given type hint will allow ScalaJack to construct a Dog object and return it as a Pet, which is what you want.

We'll see in a later section how you can customize both the type hint label and the value in your JSON.