package co.blocke.scalajack.benchmarks

import co.blocke.scalajack._
import co.blocke.scalajack.typeadapter._

import scala.language.{existentials, reflectiveCalls}
import scala.reflect.runtime.universe.{ClassSymbol, TypeTag, typeOf}

object PersonTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](classSymbol: ClassSymbol, context: Context, next: TypeAdapterFactory)(implicit tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe == typeOf[Person]) {
      println("YES!  PersonTypeAdapter")
      PersonTypeAdapter(context.typeAdapterOf[String], context.typeAdapterOf[Long]).asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T](context)
    }

}

case class PersonTypeAdapter(stringTA: TypeAdapter[String], longTA: TypeAdapter[Long]) extends TypeAdapter[Person] {

  override def read(reader: Reader): Person = {
    reader.beginObject()

    var id: Long = 0L
    var firstName: String = ""
    var lastName: String = ""
    var email: String = ""
    var gender: String = ""
    var ipAddress: String = ""

    while (reader.hasMoreMembers) {
      stringTA.read(reader) match {
        case "id"         => id = longTA.read(reader) //reader.readLong
        case "first_name" => firstName = stringTA.read(reader) //reader.readString
        case "last_name"  => lastName = stringTA.read(reader) //reader.readString
        case "email"      => email = stringTA.read(reader) //reader.readString
        case "gender"     => gender = stringTA.read(reader) //reader.readString
        case "ip_address" => ipAddress = stringTA.read(reader) //reader.readString
      }
    }
    reader.endObject()
    Person(id, firstName, lastName, email, gender, ipAddress)
  }

  override def write(value: Person, writer: Writer): Unit = {}

}
