package co.blocke.scalajack
package mongo

import org.mongodb.scala.bson._

case class Person(id: ObjectId, name: String, age: Int, stuff: Map[Int, Int])

object RunMongo extends App {

  implicit def BsonDocument2Document(x: BsonValue) = new Document(x.asInstanceOf[BsonDocument])

  val sj = ScalaJack(MongoFlavor)

  val s = Person(new ObjectId(), "Greg", 52, Map(5 -> 1, 6 -> 2))
  val m = sj.render(s)
  val d: Document = m
  println(m)
  println(d)
  println(sj.read[Person](m))
}
