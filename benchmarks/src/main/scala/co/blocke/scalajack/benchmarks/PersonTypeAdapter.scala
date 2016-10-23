package co.blocke.scalajack.benchmarks

import java.lang.reflect.Method

import co.blocke.scalajack._
import typeadapter._

import scala.collection.mutable
import scala.language.{ existentials, reflectiveCalls }
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ ClassSymbol, MethodMirror, MethodSymbol, NoType, TermName, Type, typeOf }

object PersonTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context, next: TypeAdapterFactory): Option[TypeAdapter[_]] =
    if (tpe == typeOf[Person]) {
      println("YES!  PersonTypeAdapter")
      Some(PersonTypeAdapter(context.typeAdapterOf[String], context.typeAdapterOf[Long]))
    } else {
      next.typeAdapter(tpe, context)
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
