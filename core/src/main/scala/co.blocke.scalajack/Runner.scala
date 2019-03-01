package co.blocke.scalajack

import scala.util._
import model.SJCapture

case class VCDouble(vc: Double) extends AnyVal
class PlayerMix() {
  def someConfusingThing() = true
  var name: String = "" // public var member
  var maybe: Option[Int] = Some(1) // optional member

  @Ignore var bogus: String = ""

  private var _age: VCDouble = VCDouble(0.0)
  def age: VCDouble = _age // getter/setter member
  def age_=(a: VCDouble) = _age = a
}

class BigPlayer() extends PlayerMix {
  var more: Int = 0
}

class AllVals(val a: Int, val b: Option[Int], val c: Int) {
  private var _age: VCDouble = VCDouble(0.0)
  def age: VCDouble = _age // getter/setter member
  def age_=(a: VCDouble) = _age = a
}
class NotAllVals(val a: Int, b: Int, val c: Int)

class Embed() {
  var stuff: List[String] = List.empty[String]
  var num: Int = 0
}
class Boom() {
  var name: String = ""
  var other: Try[Embed] = Success(null)
}

class Cap() extends SJCapture {
  var name: String = ""
}

case class CaseCap(name: String) extends SJCapture

// Default takes precidence over non-existence (None) in JSON
class Perhaps(
    val a:                        Int,
    val c:                        Int,
    @MapName(name = "bogus") val b:Option[Int]= Some(5)
)

/*

==== Modes of Classes:

1) Pure val constructor:  class Foo(val a:Int, val b:Int)

2) Mixed val constructor and getter/setter, possibly also with public var:
     class Foo(val a:Int, val b:Int) {
       private var _c: Int = 0
       def c: Int = _c
       def c_= (i:Int) = _c = i

       var d: Int = 0
     }

Annotations:
  @Ignore  -- completely ignore this field (public var or getter/setter)
  @Maybe   -- always write, but no error if missing on read (set to input value if present, unlike @Ignore)

 */

@Collection(name = "Stuff")
class MapFactorPlain(val t: Option[Int] = Some(15)) {
  var fooBar: String = ""
  @Maybe var thingy: Long = 1L
  var count: Option[Int] = Some(1)

  private var _age: VCDouble = VCDouble(0.3)
  @Maybe def age: VCDouble = _age // getter/setter member
  def age_=(a: VCDouble) = _age = a
}

object Runner extends App {

  val sj = ScalaJack()

  //  println(sj.read[Perhaps]("""{"a":5,"c":2}""").b)

  val js = """{"t":19,"fooBar":"Mike","count":7,"thingy":123}"""
  //  val js = """{"name":"Bob","id":123,"count":3}"""

  //  try {
  //
  //  println(sj.read[JavaTestClass2](js).name)
  //
  val x = sj.read[MapFactorPlain](js)
  println(x.age)
  //
  //  } catch {
  //    case t: Throwable => println("Boom " + t.getMessage())
  //  }

}

// TODO:
//  1) Handle Options
//  2) Handle @Ignore (only getter/setter + var fields, not constructor fields!)
//  3) Determine semantics of Option
//  4) Think about if we need @Maybe --> Yes... it's like a default value.  If not present on read then do nothing.  Always write.
