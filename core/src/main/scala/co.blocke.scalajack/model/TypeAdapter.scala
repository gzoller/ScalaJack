package co.blocke.scalajack
package model

import scala.collection.mutable
import scala.reflect.ClassTag
import co.blocke.scala_reflection._
import co.blocke.scala_reflection.info._
import co.blocke.scala_reflection.impl.Clazzes._


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
 trait TypeAdapter[T] {
  self =>

  type tpe = T

  val info: RType
  def resolved: TypeAdapter[T] = this // Might be something else during Lazy construction
  
  def defaultValue: Option[T] = None

  // Used to determine if we need to wrap Map keys in quotes (no, if isStringish == true -- the quotes are automatic in this case)
  def isStringish: Boolean = false
  def maybeStringish: Boolean = false // means we don't know for sure if something is Stringish until read/render time (not in Factory), e.g. Option or Union
  
  def read(parser: Parser): T
  def write[WIRE](
      t:      T,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit

  def as[U <: TypeAdapter[_]: ClassTag]: U = {
    val runtimeClass = implicitly[ClassTag[U]].runtimeClass
    try {
      runtimeClass.cast(self).asInstanceOf[U]
    } catch {
      case _: ClassCastException =>
        throw new RuntimeException(
          s"$self is not an instance of ${implicitly[ClassTag[U]].runtimeClass}"
        )
    }
  }

  // Used to correctly-type tuple fields, which each have separate types that are unknown at write, but...
  // each field's TypeAdapter *does* know its type so it can be correctly cast inside the TypeAdapter.
  inline def castAndWrite[WIRE]( v: Any, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit = 
    write(v.asInstanceOf[tpe], writer, out)
}

trait ScalarTypeAdapter[T] extends TypeAdapter[T] 

// Marker trait for collections
trait Collectionish

// Marker trait for classes
trait Classish