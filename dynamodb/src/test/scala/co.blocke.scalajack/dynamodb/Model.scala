package co.blocke.scalajack
package dynamodb

import model._
import scala.collection.mutable
import co.blocke.scala_reflection.RType
import co.blocke.scala_reflection.info.AliasInfo

opaque type Phone >: Null = String

// Override just Phone
object PhoneAdapter extends TypeAdapterFactory with TypeAdapter[Phone]:
  def matches(concrete: RType): Boolean = 
    concrete match {
      case a: AliasInfo if a.name == "Phone" => true
      case _ => false
    }
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[Phone] = this
  val info = RType.of[Phone]
  override def isStringish: Boolean = true
  
  def read(parser: Parser): Phone =
    parser.expectString() match {
      case null      => null.asInstanceOf[Phone]
      case s: String => s.replaceAll("-", "").asInstanceOf[Phone]
    }

  def write[WIRE](
      t:      Phone,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = t match {
    case null => writer.writeNull(out)
    case _ =>
      writer.writeString(
        "%s-%s-%s".format(t.toString.substring(0, 3), t.toString.substring(3, 6), t.toString.substring(6)),
        out
      )
  }

trait Human { val name: String; val age: Int }
case class Misc(wow: Double, bing: String)

@Collection(name = "people")
case class Person(
    @DBKey(index = 1) name:String,
    @DBKey(index = 0) age:Int,
    likes:                List[String],
    stuff:                Misc,
    foo:                  Option[Boolean] = None)
  extends Human

@Collection(name = "people2")
case class PersonOneKey(
    @DBKey(index = 0) name:String,
    age:                  Int,
    likes:                List[String],
    stuff:                Misc,
    foo:                  Option[Boolean] = None)
  extends Human

@Collection(name = "bogus")
case class ErrorNoKey(
    name:  String,
    age:   Int,
    likes: List[String],
    stuff: Misc,
    foo:   Option[Boolean] = None)
  extends Human

case class ErrorNoTable(
    @DBKey(index = 0) name:String,
    age:                  Int,
    likes:                List[String],
    stuff:                Misc,
    foo:                  Option[Boolean] = None)
  extends Human

case class PersonWithPhone(name: String, phone: Phone)
trait Address { val postalCode: String }
case class DefaultAddress(postalCode: String) extends Address
case class USAddress(
    street:     String,
    city:       String,
    state:      String,
    postalCode: String)
  extends Address

@Collection(name = "people")
class PersonPlain1(
    @DBKey(index = 1) val name:String,
    @DBKey(index = 0) val age:Int,
    val likes:                List[String],
    val stuff:                Misc,
    val foo:                  Option[Boolean] = None)

@Collection(name = "people")
class PersonPlain2() {
  @DBKey(index = 1) var name: String = ""
  @DBKey(index = 0) var age: Int = 0
  var likes: List[String] = List.empty[String]
  var stuff: Misc = _
  var foo: Option[Boolean] = None
}

trait Body
case class FancyBody(message: String) extends Body

case class Envelope[T <: Body](id: String, body: T) {
  type Giraffe = T
}