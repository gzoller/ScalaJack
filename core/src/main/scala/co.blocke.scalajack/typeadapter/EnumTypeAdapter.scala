package co.blocke.scalajack
package typeadapter

import model._

import scala.collection.mutable
import scala.util.{Try, Success, Failure}
import java.lang.reflect.Method
import co.blocke.scala_reflection._
import co.blocke.scala_reflection.info._

object EnumTypeAdapterFactory extends TypeAdapterFactory:
  def matches(concrete: RType): Boolean =
    concrete match {
      case _: ScalaEnumInfo => true
      case _: ScalaEnumerationInfo => true
      case _: JavaEnumInfo => true
      case _ => false
    }

  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[_] =
    val enumsAsInt = taCache.jackFlavor.enumsAsInt
    concrete match {
      // Scala 2.x Enumeration support
      case scalaOld: ScalaEnumerationInfo => 
        val erasedEnumClassName = scalaOld.name + "$"
        val enumInstance = Class
          .forName(erasedEnumClassName)
          .getField(scala.reflect.NameTransformer.MODULE_INSTANCE_NAME)
          .get(null)
          .asInstanceOf[Enumeration]
        ScalaEnumerationTypeAdapter(enumInstance, concrete, enumsAsInt)

      // Scala 3.x Enum support
      case scalaNew: ScalaEnumInfo => 
        ScalaEnumTypeAdapter(concrete, enumsAsInt)

      // Java Enum support
      case javaEnum: JavaEnumInfo => 
        JavaEnumTypeAdapter(concrete, enumsAsInt)
    }


case class ScalaEnumerationTypeAdapter[E <: Enumeration](
    e:           E,
    info:        RType,
    enumsAsInt:  Boolean
  ) extends TypeAdapter[e.Value]:
  override def isStringish: Boolean = !enumsAsInt

  def read(parser: Parser): e.Value = 
    if (parser.nextIsNumber) {
      val en = parser.expectNumber()
      Try(e(en.toInt)) match {
        case Success(u) => u
        case Failure(u) =>
          parser.backspace()
          throw new ScalaJackError(
            parser.showError(
              s"No value found in enumeration ${e.getClass.getName} for $en"
            )
          )
      }
    } else if (parser.nextIsString) {
      val es = parser.expectString()
      if (es == null)
        null
      else
        Try(e.withName(es)) match {
          case Success(u) => u
          case Failure(u) =>
            parser.backspace()
            throw new ScalaJackError(
              parser.showError(
                s"No value found in enumeration ${e.getClass.getName} for $es"
              )
            )
        }
    } else if (parser.peekForNull)
      null
    else
      throw new ScalaJackError(
        parser.showError(s"Expected a Number or String here")
      )

  def write[WIRE](
      t:      e.Value,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null            => writer.writeNull(out)
      case v if enumsAsInt => writer.writeInt(v.id, out)
      case v               => writer.writeString(v.toString, out)
    }


case class ScalaEnumTypeAdapter[E <: scala.reflect.Enum](
    info:        RType,
    enumsAsInt:  Boolean
  ) extends TypeAdapter[E]:

  val scalaEnum = info.asInstanceOf[ScalaEnumInfo]
  override def isStringish: Boolean = !enumsAsInt

  def read(parser: Parser): E = 
    if (parser.nextIsNumber) {
      val en = parser.expectNumber().toInt
      Try(scalaEnum.valueOf(en)) match {
        case Success(u) => u.asInstanceOf[E]
        case Failure(u) =>
          parser.backspace()
          throw new ScalaJackError(
            parser.showError(
              s"No value found in enumeration ${info.name} for $en"
            )
          )
      }
    } else if (parser.nextIsString) {
      val es = parser.expectString()
      if (es == null)
        null.asInstanceOf[E]
      else
        Try(scalaEnum.valueOf(es).asInstanceOf[E]) match {
          case Success(u) => u
          case Failure(u) =>
            parser.backspace()
            throw new ScalaJackError(
              parser.showError(
                s"No value found in enumeration ${info.name} for $es"
              )
            )
        }
    } else if (parser.peekForNull)
      null.asInstanceOf[E]
    else
      throw new ScalaJackError(
        parser.showError(s"Expected a Number or String here")
      )

  def write[WIRE](
      t:      E,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = 
    t match {
      case null            => writer.writeNull(out)
      case _ if enumsAsInt => writer.writeInt(t.ordinal, out)
      case _               => writer.writeString(t.toString, out)
    }
  

case class JavaEnumTypeAdapter[E <: java.lang.Enum[_]](
    info:        RType,
    enumsAsInt:  Boolean
  ) extends TypeAdapter[E]:

  val javaEnum = info.asInstanceOf[JavaEnumInfo]
  override def isStringish: Boolean = !enumsAsInt

  def read(parser: Parser): E = 
    if (parser.peekForNull) then
      null.asInstanceOf[E]
    else
      val valueOf = info.infoClass.getDeclaredMethod("valueOf", classOf[String])
      try {
        valueOf.invoke(info.infoClass, parser.expectString()).asInstanceOf[E]
      } catch {
        case ex: java.lang.reflect.InvocationTargetException => throw ex.getCause
        case t: Throwable => throw t
      }
    
  def write[WIRE](
      t:      E,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = 
    t match {
      case null            => writer.writeNull(out)
      case _               => writer.writeString(t.toString, out)
    }