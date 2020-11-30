package co.blocke.scalajack
package json.collections

import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON
import scala.collection.immutable._

class Tuples() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()

  test("null tuples work") {
    describe("-----------------\n:  Tuple Tests  :\n-----------------", Console.BLUE) 
    val jsNull = "null".asInstanceOf[JSON]
    assert(sj.read[(Int, Boolean)](jsNull) == null)
    assert(sj.render[(Int,Boolean)](null) == jsNull)
  }

  test("missing start bracken") {
    val js = """12,5""".asInstanceOf[JSON]
    val msg =
      """Expected start of tuple here
        |12,5
        |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[(Int, Int)](js)
    }
  }

  test("missing comma") {
    val js = """[12""".asInstanceOf[JSON]
    val msg =
      """Expected comma here
      |[12
      |---^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[(Int, Int)](js)
    }
  }

  test("no closing bracket") {
    val js = """[12,5""".asInstanceOf[JSON]
    val msg =
      """Expected end of tuple here
        |[12,5
        |-----^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[(Int, Int)](js)
    }
  }
