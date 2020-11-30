package co.blocke.scalajack
package dynamodb

import TestUtil._
import munit._
import munit.internal.console
import com.amazonaws.services.dynamodbv2.model.ProvisionedThroughput

class CreateTableRequest extends FunSuite:

  val sj = ScalaJack(DynamoFlavor()).asInstanceOf[DynamoFlavor]

  test("Single primary key") {
    describe(
      "-----------------------------------------\n:  DynamoDB Create Table Request Tests  :\n-----------------------------------------", Console.BLUE
    )
    val req = sj.createTableRequest[PersonOneKey](new ProvisionedThroughput(12L, 5L))
    assertEquals(
      """{AttributeDefinitions: [{AttributeName: name,AttributeType: S}],TableName: people2,KeySchema: [{AttributeName: name,KeyType: HASH}],ProvisionedThroughput: {ReadCapacityUnits: 12,WriteCapacityUnits: 5},}""",
      req.toString)
  }

  test("Primary key with sorting key") {
    val req =
      sj.createTableRequest[Person](new ProvisionedThroughput(12L, 5L))
    assertEquals(
      """{AttributeDefinitions: [{AttributeName: age,AttributeType: N}, {AttributeName: name,AttributeType: S}],TableName: people,KeySchema: [{AttributeName: age,KeyType: HASH}, {AttributeName: name,KeyType: RANGE}],ProvisionedThroughput: {ReadCapacityUnits: 12,WriteCapacityUnits: 5},}""",
      req.toString)
  }

  test("Error - no key specified") {
    interceptMessage[java.lang.IllegalStateException]("Class co.blocke.scalajack.dynamodb.ErrorNoKey must define at least a primary key with @DBKey."){
      sj.createTableRequest[ErrorNoKey](new ProvisionedThroughput(12L, 5L))
    }
  }

  test("Error - no table specified") {
    interceptMessage[java.lang.IllegalStateException]("Class co.blocke.scalajack.dynamodb.ErrorNoTable must be annotated with @Collection to specify a table name."){
      sj.createTableRequest[ErrorNoTable](new ProvisionedThroughput(12L, 5L))
    }
  }
