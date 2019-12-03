package co.blocke.scalajack
package mongo

import org.bson._
import scala.collection.JavaConverters._

case class Person(name: String, age: Int)
case class Two(foo: String, bar: Boolean)
case class Five(@DBKey name: String, two: Two)

object Hello extends App {

  val sj = ScalaJack(MongoFlavor())

  val five = Five("Fred", Two("blah", true))
  val dbo = sj.render(five)
  println(dbo)
  println(sj.read[Five](dbo))
}
