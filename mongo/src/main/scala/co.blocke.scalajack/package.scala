package co.blocke.scalajack

// This is needed in case someone reads a class from Bson --> Scala which contains an object id (BsonObjectId).
// If we wanted to then do something with this scala object in JSON, the JSON flavor of ScalaJack has no clue what
// a BsonObjectId is, so we just type it as String so it won't go boom.
package object mongo {
  type ObjectId = String
  type MongoTime = Long
}