package co.blocke.scalajack
package dynamodb
package test

import scala.reflect.runtime.universe.typeOf
import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.util.Try
import java.util.UUID

import com.amazonaws.services.dynamodbv2.document.Item
import com.amazonaws.services.dynamodbv2.model.ProvisionedThroughput

class Plain extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val sj = ScalaJack(DynamoFlavor())

  describe("-----------------------\n:  Plain Class Tests  :\n-----------------------") {
    describe("Items:") {
      it("All val constructor") {
        val inst = new PersonPlain1("Greg", 50, List("Woodworking", "Diet Coke"), Misc(1.23, "boom"), Some(true))
        val item = sj.render(inst)
        assertResult("""{ Item: {name=Greg, age=50, likes=[Woodworking, Diet Coke], stuff={wow=1.23, bing=boom}, foo=true} }""") { item.toString }
        assertResult(true) {
          val inst2 = sj.read[PersonPlain1](item)
          inst.name == inst2.name && inst.age == inst2.age && inst.stuff.wow == inst2.stuff.wow
        }
      }
      it("Zero-arg constructor with var members") {
        val inst = new PersonPlain2()
        inst.name = "Greg"
        inst.age = 50
        inst.likes = List("Woodworking", "Diet Coke")
        inst.stuff = Misc(1.23, "boom")
        val item = sj.render(inst)
        assertResult("""{ Item: {stuff={wow=1.23, bing=boom}, likes=[Woodworking, Diet Coke], age=50, name=Greg} }""") { item.toString }
        assertResult(true) {
          val inst2 = sj.read[PersonPlain2](item)
          inst.name == inst2.name && inst.age == inst2.age && inst.stuff.wow == inst2.stuff.wow
        }
      }
    }
    describe("Creation:") {
      it("All val constructor") {
        val req = sj.asInstanceOf[DynamoFlavor].createTableRequest[PersonPlain1](new ProvisionedThroughput(12L, 5L))
        assertResult("""{AttributeDefinitions: [{AttributeName: age,AttributeType: N}, {AttributeName: name,AttributeType: S}],TableName: people,KeySchema: [{AttributeName: age,KeyType: HASH}, {AttributeName: name,KeyType: RANGE}],ProvisionedThroughput: {ReadCapacityUnits: 12,WriteCapacityUnits: 5},}""") { req.toString }
      }
      it("Zero-arg constructor with var members") {
        val req = sj.asInstanceOf[DynamoFlavor].createTableRequest[PersonPlain2](new ProvisionedThroughput(12L, 5L))
        assertResult("""{AttributeDefinitions: [{AttributeName: age,AttributeType: N}, {AttributeName: name,AttributeType: S}],TableName: people,KeySchema: [{AttributeName: age,KeyType: HASH}, {AttributeName: name,KeyType: RANGE}],ProvisionedThroughput: {ReadCapacityUnits: 12,WriteCapacityUnits: 5},}""") { req.toString }
      }
    }
  }
}
