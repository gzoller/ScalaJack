## MongoDB Support

ScalaJack doesn't wrap the MongoDB persistence libraries--that's not its mission. It does provide a way to convert classes to/from Mongo BsonDocuments. You'll need to include the mongo support package as shown in the Use section of the main Readme.  Then you need to create a Mongo-specific ScalaJack object:

```scala
    import co.blocke.scalajack._
    import mongo._

    val sjMongo = ScalaJack(MongoFlavor()) // produce a Mongo-flavored ScalaJack
    val mydbo         = sjMongo.render( myCaseClass )
    val myCaseClass   = sjMongo.read[MyClass]( mydbo )
```

The trait type hint label and value modifiers (.withAdapters, .withDefaultHint, .withHints) work like they do for JSON documents.

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

ScalaJack will generate the appropriate key field(s) for Mongo using the DBKey annotation. 

### Documents

ScalaJack's MongoDB module produces BsonDocument objects upon render -- specifically org.bson.BsonDocument.  This BsonDocument type is also expected for read operations.

> **Note:** Previous versions of ScalaJack produced the Scala driver's Document class, not the BsonDocument class.  If you prefer, you can implement an implicit conversion like this:
> ```scala
> implicit def BsonDocument2Document(x: BsonValue) = new Document(x.asInstanceOf[BsonDocument])
> ```
