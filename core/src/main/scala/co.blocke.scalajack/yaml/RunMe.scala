package co.blocke.scalajack
package yaml

import org.snakeyaml.engine.v2.api.{DumpSettings, LoadSettings}
import org.snakeyaml.engine.v2.api.lowlevel._
import org.snakeyaml.engine.v2.nodes._
import org.snakeyaml.engine.v2.common._
import org.snakeyaml.engine.v2.parser.ParserImpl
import org.snakeyaml.engine.v2.scanner.StreamReader

import scala.jdk.CollectionConverters._

case class Person(name: String, age: Int) extends SJCapture
object Size extends Enumeration {
  val Small, Medium, Large = Value
}
object Food extends Enumeration {
  val Seeds, Meat, Pellets, Veggies = Value
}
trait Pet {
  val name: String
  val food: Food.Value
}
case class FishPet(name: String, food: Food.Value, waterTemp: Double) extends Pet
case class DogPet(name: String, food: Food.Value, numLegs: Int)       extends Pet

case class AnyShell(a: Any)

object RunMe extends App {

  val sj = ScalaJack(YamlFlavor())

  val inst = AnyShell(
    Map(
      List(1, 2, 3)                -> List("a", "b", "c"),
      DogPet("Fido", Food.Meat, 4) -> DogPet("Fifi", Food.Meat, 4),
      Size.Small                   -> "ok",
      123.456                      -> true,
      293845                       -> "Greg",
      false                        -> "16",
      "Fred"                       -> "Wilma",
      16.toByte                    -> null
    )
  )

  println(sj.render(inst))
}
