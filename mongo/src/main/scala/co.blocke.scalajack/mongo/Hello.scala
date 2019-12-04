package co.blocke.scalajack
package mongo

import org.bson._
import scala.collection.JavaConverters._

import org.bson.types.ObjectId
case class PersonCapture(
    id:    ObjectId,
    name:  String,
    age:   Int,
    stuff: Map[Int, Int])
  extends SJCapture

object Hello extends App {

  val sj = ScalaJack(MongoFlavor())

  val d = new BsonDocument(
    List(
      new BsonElement("num", new BsonInt32(3)),
      new BsonElement(
        "s",
        new BsonDocument(
          List(
            new BsonElement("_hint", new BsonInt32(45)),
            new BsonElement("size", new BsonInt32(34))
          ).asJava
        )
      )
    ).asJava
  )

  val s = PersonCapture(new ObjectId(), "Fred", 52, Map(5 -> 1, 6 -> 2))
  val m = sj.render(s)
  m.asDocument.append("extra", new BsonString("hey"))
  println(m)
  val readIn = sj.read[PersonCapture](m)
  println(readIn)

}
