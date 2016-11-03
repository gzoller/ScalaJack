## Parameterized Classes

ScalaJack is able to handle parameterized classes and traits:

```scala
package com.me

trait Vehicle[T]{ val transported:T }
case class Car(transported:Person) extends Vehicle[Person]

println(sj.render[Vehicle[Person]](Car(Person("Fred",25))))
// {"_hint":"com.me.Car","transported":{"name":"Fred","age":25}}
```

Cool, eh?  Ok...let's make it interesting!

```scala
package com.me

object VehicleClass extends Enumeration {
  type VehicleClass = Value
  val Land, Air, Sea = Value
}
import VehicleClass._

trait Vehicle[K <: VehicleClass] { val kind: K }
case class Car(passengers: Int) extends Vehicle[Land.type] { val kind: Land.type = Land }

trait Hobby[X, Y] { val thing1: X; val thing2: Y }
trait Artist[W, Z] { val instrument: W; val effort: Z }
trait Person[X, Y] { val who: X; val org: Y }

case class Sports[A, B](thing1: A, thing2: B) extends Hobby[A, B]
case class Painter[A, B](instrument: A, effort: B) extends Artist[A, B]
case class Employee[A, B, C, D](who: Artist[C, Hobby[D, A]], org: B) extends Person[Artist[C, Hobby[D, A]], B]

val inst: Person[Artist[Int, Hobby[Double, Char]], Vehicle[_ <: VehicleClass]] = Employee(Painter(5, Sports(1.2, 'Z')), "wow")
val js = sj.render(inst)
// {"_hint":"com.me.Employee","who":{"_hint":"com.me.Painter","instrument":5,"effort":{"_hint":"com.me.Sports","thing1":1.2,"thing2":"Z"}},"org":{"_hint":"com.me.Car","passengers":4}}

val orig = sj.read[Person[Artist[Int, Hobby[Double, Char]], Vehicle[_ <: VehicleClass]]](js)
```
Clearly this is a contrived (and forced) example, but it does show that complex nesting relationships between parameterized types work just fine, even when we threw in some traits and an Enumeration for good measure.