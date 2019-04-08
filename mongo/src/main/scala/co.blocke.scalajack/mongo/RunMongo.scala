package co.blocke.scalajack
package mongo

import org.mongodb.scala.bson._
import java.time._

case class Person(id: ObjectId, name: String, age: Int, stuff: Map[Int, Int], t: ZonedDateTime) extends SJCapture

//---------------------------------------------
trait FlavMaker {
  type W
  def make(): Flav[W]
}
case class StringFlavMaker() extends FlavMaker {
  type W = String
  def make(): Flav[String] = StringFlav()
}
trait Flav[W] {
  def write(s: String): W
}
case class StringFlav() extends Flav[String] {
  def write(s: String): String = "worked"
}
object Maker {
  def apply(maker: FlavMaker): Flav[maker.W] = maker.make()
}
//---------------------------------------------

object RunMongo extends App {

  implicit def BsonDocument2Document(x: BsonValue) = new Document(x.asInstanceOf[BsonDocument])

  //  def resolve[T, W](t: T)(implicit tt: TypeTag[T]): W = {
  //    println(tt.tpe)
  //    null.asInstanceOf[W]
  //  }
  //
  //  val greg = Maker(StringFlavMaker())
  //  val zoller = resolve(greg.write("ppp"))
  //  println(zoller)

  val sj = ScalaJack(delimited.DelimitedFlavor(';'))

  val s = Person(new ObjectId(), "Greg", 52, Map(5 -> 1, 6 -> 2), ZonedDateTime.now())
  val m = sj.render(s)
  m.asDocument.append("extra", BsonString("hey"))
  val d: Document = m
  println(m)
  println(d)
  val readIn = sj.read[Person](m)
  println(readIn)
  println(s)

  println("With extra:")
  println(sj.render(readIn))
}
