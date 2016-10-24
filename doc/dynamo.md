## DynamoDB Support
ScalaJack provides some limited support for DynamoDB.  To be very clear, ScalaJack is a serialization product, and does not involve itself in database CRUD calls, i.e. it isn't a Dynamo library or wrapper.  What ScalaJack does do is try to make the process of calling Dynamo easier and contain less boilerplate by using its powerful reflection abilities to create appropriate objects for Dynamo.

First of all you'll need to include a separate scalajack_dynamo library as shown in the main README file.  (We separated it so non-Dynamo users won't have to include Dynamo library dependencies.)

#### Table Creation
DynamoDB table creation is accomplished (in Dynamo's native Java API) via method calls accepting a CreateTableRequest object.  ScalaJack provides a feature to create a CreateTableRequest.

```scala
case class Misc(wow: Double, bing: String)

@Collection(name = "people")
case class Person(
  @DBKey(index = 1) name:String,
  @DBKey(index = 0) age:Int,
  likes:                List[String],
  stuff:                Misc,
  foo:                  Option[Boolean] = None
) 

val sj = ScalaJack(DynamoFlavor())
val req = sj.asInstanceOf[DynamoFlavor].createTableRequest[PersonOneKey](new ProvisionedThroughput(12L, 5L))
```
First let's look at the Person class.  Notice the annotations.  The @Collection annotation marks the class with a given table name in Dynamo.  The two @DBKey annotations mark the primary key and sort key.  Notice the index values in the @DBKey annotation.  Index 0 is the primary key and index 1 is the sort key.  Just like in Dynamo, the sort key is optional.

Note that Dynamo's rules apply, meaning a key must be either String or Number.

Once constructed, you can use the CreateTableRequest as you usually would, adding more to it via its built-in methods, then using it to actually create the table.

**NOTE:** Do you see the .asInstanceOf in the code above?  We need that.  createTableRequest is not a method on stock ScalaJack--only the Dynamo flavor so we need to cast it first.

#### Item
The other feature ScalaJack is very helpful for in Dynamo is Item.  Rendering an Item (or reading from it) is exactly like ScalaJack for JSON, except instead of JSON you have a Dynamo Item object:

```scala
val inst: Person = Person("Greg", 50, List("Woodworking", "Diet Coke"), Misc(1.23, "boom"))
val item:Item = sj.render(inst)
// { Item: {name=Greg, age=50, likes=[Woodworking, Diet Coke], stuff={wow=1.23, bing=boom}} }

sj.read[Person](item)  // == original Person object
```
