package co.blocke.scalajack
package mongo

import org.mongodb.scala.bson._
import java.time._

case class Person(id: ObjectId, name: String, age: Int, stuff: Map[Int, Int], t: ZonedDateTime) extends SJCapture

object RunMongo extends App {

  implicit def BsonDocument2Document(x: BsonValue) = new Document(x.asInstanceOf[BsonDocument])

  val sj = ScalaJack(MongoFlavor())

  /* -- Good SJCapture example for Mongo
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
   */

  //        out += BsonDateTime(t.toInstant.toEpochMilli).asInstanceOf[WIRE]
  // ZonedDateTime.ofInstant(Instant.ofEpochMilli(dateTimeLong), ZoneId.systemDefault)

  val dt = java.time.ZonedDateTime.now().withZoneSameInstant(ZoneId.of("UTC"))
  //  println(dt)
  println("UTC: " + dt)
  val md = new BsonDateTime(dt.toInstant.toEpochMilli)
  println(md)
  println("---------------")
  val z = ZonedDateTime.ofInstant(Instant.ofEpochMilli(md.getValue), ZoneId.of("UTC"))
  println("UTC: " + z)
}
