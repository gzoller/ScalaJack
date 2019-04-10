package co.blocke.scalajack
package dynamodb

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import com.amazonaws.services.dynamodbv2.model.ProvisionedThroughput

class CreateTableRequest extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val sj = ScalaJack(DynamoFlavor()).asInstanceOf[DynamoFlavor]

  describe("---------------------------------------\n:  Dynamo Create Table Request Tests  :\n---------------------------------------") {
    it("Single primary key") {
      val req = sj.createTableRequest[PersonOneKey](new ProvisionedThroughput(12L, 5L))
      assertResult("""{AttributeDefinitions: [{AttributeName: name,AttributeType: S}],TableName: people2,KeySchema: [{AttributeName: name,KeyType: HASH}],ProvisionedThroughput: {ReadCapacityUnits: 12,WriteCapacityUnits: 5},}""") { req.toString }
    }
    it("Primary key with sorting key") {
      val req = sj.createTableRequest[Person](new ProvisionedThroughput(12L, 5L))
      assertResult("""{AttributeDefinitions: [{AttributeName: age,AttributeType: N}, {AttributeName: name,AttributeType: S}],TableName: people,KeySchema: [{AttributeName: age,KeyType: HASH}, {AttributeName: name,KeyType: RANGE}],ProvisionedThroughput: {ReadCapacityUnits: 12,WriteCapacityUnits: 5},}""") { req.toString }
    }
    it("Error - no key specified") {
      the[java.lang.IllegalStateException] thrownBy
        sj.createTableRequest[ErrorNoKey](new ProvisionedThroughput(12L, 5L)) should have message "Class co.blocke.scalajack.dynamodb.ErrorNoKey must define at least a primary key with @DBKey."
    }
    it("Error - no table specified") {
      the[java.lang.IllegalStateException] thrownBy
        sj.createTableRequest[ErrorNoTable](new ProvisionedThroughput(12L, 5L)) should have message "Class co.blocke.scalajack.dynamodb.ErrorNoTable must be annotated with @Collection to specify a table name."
    }
  }
}
