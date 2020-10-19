package co.blocke.scalajack
package dynamodb

import TestUtil._
import munit._
import munit.internal.console
import JsonMatcher._

import com.amazonaws.services.dynamodbv2.model.ProvisionedThroughput

class Plain extends FunSuite:

  val sj = ScalaJack(DynamoFlavor())

  test("All val constructor") {
    describe(
      "--------------------------------\n:  DynamoDB Plain Class Tests  :\n--------------------------------", Console.BLUE
    )
    describe("Items:")
    val inst = new PersonPlain1(
      "Greg",
      50,
      List("Woodworking", "Diet Coke"),
      Misc(1.23, "boom"),
      Some(true)
    )
    val item = sj.render(inst)
    assertEquals(true, jsonMatches(
      """{"name":"Greg","age":50,"likes":["Woodworking","Diet Coke"],"stuff":{"wow":1.23,"bing":"boom"},"foo":true}""".asInstanceOf[json.JSON],
      item.toJSON.asInstanceOf[json.JSON]
    ))
    assertEquals(true, {
      val inst2 = sj.read[PersonPlain1](item)
      inst.name == inst2.name && inst.age == inst2.age && inst.stuff.wow == inst2.stuff.wow
    })
  }

  test("Zero-arg constructor with var members") {
    val inst = new PersonPlain2()
    inst.name = "Greg"
    inst.age = 50
    inst.likes = List("Woodworking", "Diet Coke")
    inst.stuff = Misc(1.23, "boom")
    val item = sj.render(inst)
    assertEquals(true, jsonMatches(
      """{"name":"Greg","age":50,"likes":["Woodworking","Diet Coke"],"stuff":{"wow":1.23,"bing":"boom"}}""".asInstanceOf[json.JSON],
      item.toJSON.asInstanceOf[json.JSON]
    ))
    assertEquals(true, {
      val inst2 = sj.read[PersonPlain2](item)
      inst.name == inst2.name && inst.age == inst2.age && inst.stuff.wow == inst2.stuff.wow
    })
  }

  test("All val constructor") {
    describe("Creation:")
    val req = sj
      .asInstanceOf[DynamoFlavor]
      .createTableRequest[PersonPlain1](new ProvisionedThroughput(12L, 5L))
    assertEquals(
      """{AttributeDefinitions: [{AttributeName: age,AttributeType: N}, {AttributeName: name,AttributeType: S}],TableName: people,KeySchema: [{AttributeName: age,KeyType: HASH}, {AttributeName: name,KeyType: RANGE}],ProvisionedThroughput: {ReadCapacityUnits: 12,WriteCapacityUnits: 5},}""",
      req.toString)
  }

  test("Zero-arg constructor with var members") {
    val req = sj
      .asInstanceOf[DynamoFlavor]
      .createTableRequest[PersonPlain2](new ProvisionedThroughput(12L, 5L))
    assertEquals(
      """{AttributeDefinitions: [{AttributeName: age,AttributeType: N}, {AttributeName: name,AttributeType: S}],TableName: people,KeySchema: [{AttributeName: age,KeyType: HASH}, {AttributeName: name,KeyType: RANGE}],ProvisionedThroughput: {ReadCapacityUnits: 12,WriteCapacityUnits: 5},}""",
      req.toString)
  }
