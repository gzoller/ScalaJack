
## Parameterized Classes

ScalaJack is able to handle parameterized classes and traits seamlessly:

**File1.scala**
```scala
package com.me
sealed trait Vehicle[T]{ val  transported:T }
case class Car(transported:Person) extends Vehicle[Person]
```

**File2.scala**
```scala
given sjVehicle: ScalaJack[Vehicle[Person]] = sjCodecOf[Vehicle[Person]]
println(sjVehicle.toJson(Car(Person("Fred",25))))
// {"_hint":"Car","transported":{"name":"Fred","age":25}}
```

Ok, now let's make it interesting..

**File1.scala**
```scala
object VehicleClass extends Enumeration {
  type VehicleClass = Value
  val Land, Air, Sea = Value
}
import VehicleClass.*

sealed trait Vehicle { val kind: VehicleClass }
case class Car(passengers: Int) extends Vehicle { val kind: Land.type = Land }

sealed trait Hobby[X, Y] { val thing1: X; val thing2: Y }
sealed trait Artist[W, Z] { val instrument: W; val effort: Z }
sealed trait Person[X, Y] { val who: X; val org: Y }

case class Sports[A, B](thing1: A, thing2: B) extends Hobby[A, B]
case class Painter[A, B](instrument: A, effort: B) extends Artist[A, B]
case class Employee[A, B, C, D](who: Artist[C, Hobby[D, A]], org: B) extends Person[Artist[C, Hobby[D, A]], B]
type ComplexPerson = Person[Artist[Int, Hobby[Double, Char]], Vehicle]
```

**File2.scala**
```scala

given sjPerson: ScalaJack[ComplexPerson] = sjCodecOf[ComplexPerson]
val  inst: ComplexPerson = Employee(Painter(5, Sports(1.2, 'Z')), Car(4))
val  js = sjPerson.toJson(inst)

// {"_hint":"Employee","who":{"_hint":"Painter","instrument":5,"effort":{"_hint":"Sports","thing1":1.2,"thing2":"Z"}},"org":{"_hint":"Car","passengers":4}}

sjPerson.fromJson(js) // re-constitutes inst
```

Clearly this is a contrived example, but it does show that complex nesting relationships between parameterized types work just fine, even when we threw in some traits and an Enumeration for good measure. Remember that you always have control of ScalaJack's reading and rendering. You can force a more general trait (will generate type hints) or use a specific concrete class (won't need type hints).