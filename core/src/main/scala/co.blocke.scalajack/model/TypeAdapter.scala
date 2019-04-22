package co.blocke.scalajack
package model

import util.Path
import scala.reflect.ClassTag
import scala.collection.mutable.Builder

/**
 * TypeAdapter includes two matching patterns you can use when you extend trait TypeAdapter for your
 * custom adapters.  The two matching behaviors are '===' and '=:='.
 *
 * This difference is because =:= matches children.  Consider:
 *
 *    type Phone = String
 *    case class( name:String, phone:Phone )
 *
 * With =:= both name and phone (String and Phone) will match a TypeAdapter derived from =:=.
 * This is actually what you want if you haven't overridden Phone with its own TypeAdapter... it should default
 * to the TypeAdapter of its base type.
 *
 * But... if you did provide an override PhoneTypeAdapter you want the matching to be strict, so we use  ===
 * in this case.  With strict matching String != Phone.
 *
 */
object TypeAdapter {

  abstract class ===[X](implicit ttFactory: TypeTag[X]) extends TypeAdapterFactory.===[X] with ScalarTypeAdapter[X] {
    val scalarType = ttFactory.tpe
    override def create(next: TypeAdapterFactory)(implicit tt: TypeTag[X]): TypeAdapter[X] = this
  }

  abstract class =:=[X](implicit ttFactory: TypeTag[X]) extends TypeAdapterFactory.=:=[X] with ScalarTypeAdapter[X] {
    val scalarType = ttFactory.tpe
    override def create(next: TypeAdapterFactory)(implicit tt: TypeTag[X]): TypeAdapter[X] = this
  }

}

trait TypeAdapter[T] {

  self =>

  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean = false): T
  def write[WIRE](t: T, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit

  def defaultValue: Option[T] = None
  def resolved: TypeAdapter[T] = this // Might be something else during Lazy construction

  def as[U <: TypeAdapter[_]: ClassTag]: U = {
    maybeAs[U].getOrElse(throw new RuntimeException(s"$self is not an instance of ${implicitly[ClassTag[U]].runtimeClass}"))
  }

  def maybeAs[U <: TypeAdapter[_]: ClassTag]: Option[U] = {
    val runtimeClass = implicitly[ClassTag[U]].runtimeClass
    try {
      Some(runtimeClass.cast(self).asInstanceOf[U])
    } catch {
      case _: ClassCastException =>
        None
    }
  }
}

trait ScalarTypeAdapter[T] extends TypeAdapter[T] {
  val scalarType: Type
}

// Marker trait for anything that boils down to String, e.g. Char, UUID, etc.
trait Stringish {
  this: TypeAdapter[_] =>
}

// Marker trait for collections
trait Collectionish

// Marker trait for classes
trait Classish
