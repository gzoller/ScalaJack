## MongoDB Support

ScalaJack doesn't wrap the MongoDB persistence libraries--that's not its mission. It does provide a way to convert classes to/from Mongo Documents. You'll need to include the mongo support package as shown in the Use section of the main Readme.  Then you need to create a Mongo-specific ScalaJack object:

```scala
    import co.blocke.scalajack._
    import mongo._
    
    val sjMongo = ScalaJack(MongoFlavor()) // produce a Mongo-flavored ScalaJack
    val mydbo         = sjMongo.render( myCaseClass )
    val myCaseClass   = sjMongo.read[MyClass]( mydbo )
```

The trait type hint label and value modifiers (.withAdapters, .withDefaultHint, .withHints) work like they do for JSON documents.

**Note:** At present, Try and SJCapture are not supported for Mongo.

### Keys

You must specify your Mongo collection's keys using annotations in your Scala classes.  Both single and compound keys are supported.

```scala
    case class OneKey(
    	@DBKey customerNum: String,
    	address: Address
    )
    
    case class TwoKeys(
    	@DBKey customerNum: String,
    	@DBKey countyCode: Int,
    	address: Address
    )
    
    case class UsingOID(
      @DBKey myKey: ObjectID,
      otherInfo: String
    )
```

ScalaJack will generate the appropriate key field(s) for Mongo using the DBKey annotation.  Note also you can use ObjectID for a Mongo-compatible key.  (ObjectID is a value class wrapping Mongo's BsonObjectId).

### Documents

ScalaJack's MongoDB module produces Document objects upon render -- specifically org.mongodb.scala.bson.collection.immutable.Document, which is the Scala driver's Document object, not the Java driver's.  This Document is also expected for read operations.

Please note that MongoDB's internal representation is in BSON, so while your "outer" object will read/render to a Document, any internal objects will become BsonDocuments!  Use of Document on the outer layer is a conveninece to make use a little more friendly.