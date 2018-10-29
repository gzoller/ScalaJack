package co.blocke.scalajack
package mongo

import org.mongodb.scala.bson._

object MRun extends App {

  val sj = ScalaJack(MongoFlavor())

  val d = BsonDocument("foo" -> BsonString("blah"), "bar" -> BsonBoolean(true))
  println(d)

  val z = sj.emit(d)
  println(z)

  val t = sj.parse(z)
  println(t)
}

