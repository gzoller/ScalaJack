package co.blocke.scalajack
package typeadapter

import model._

import scala.collection.mutable
import co.blocke.scala_reflection._
import co.blocke.scala_reflection.impl.Clazzes._
import co.blocke.scala_reflection.info._

object SealedTraitTypeAdapterFactory extends TypeAdapterFactory:
  def matches(concrete: RType): Boolean = 
    concrete match {
      case _: SealedTraitInfo => true
      case _ => false
    }
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[_] = 
    if concrete.asInstanceOf[SealedTraitInfo].children.head.isInstanceOf[ObjectInfo] then
      CaseObjectTypeAdapter(
        concrete, 
        concrete.asInstanceOf[SealedTraitInfo].children.map(_.asInstanceOf[ObjectInfo].infoClass.getSimpleName).toList)
    else
      val typeAdapters = concrete.asInstanceOf[SealedTraitInfo].children.map(c => c -> taCache.typeAdapterOf(c)).toMap
      SealedTraitTypeAdapter(taCache.jackFlavor, concrete, typeAdapters)


case class SealedTraitTypeAdapter[T](
    jackFlavor:       JackFlavor[_],
    info:             RType,
    typeAdapters:     Map[RType, TypeAdapter[_]]
  ) extends TypeAdapter[T]:

  val sealedInfo = info.asInstanceOf[SealedTraitInfo]

  def read(parser: Parser): T =
    val savedReader = parser.mark()
    if (parser.peekForNull)
      null.asInstanceOf[T]
    else {
      val readFieldNames = parser.expectMap[String, Any, Map[String, Any]](
          jackFlavor.stringTypeAdapter,
          jackFlavor.anyTypeAdapter,
          Map.newBuilder[String,Any]
        ).keySet
        sealedInfo.children.filter(
          _.asInstanceOf[ScalaCaseClassInfo].fields.map(_.name).toSet.intersect(readFieldNames).size == readFieldNames.size
        ) match {
            case setOfOne if setOfOne.size == 1 =>
              parser.revertToMark(savedReader)
              typeAdapters(setOfOne.head).read(parser).asInstanceOf[T]

            case emptySet if emptySet.isEmpty =>
              parser.backspace()
              throw new ScalaJackError(
                parser.showError(
                  s"No sub-classes of ${info.name} match field names $readFieldNames"
                )
              )

            case _ =>
              // $COVERAGE-OFF$Should be impossible--here for safety.  Something to trigger this would be ambiguous and would then be detected as a WrappedSealedTraitTypeAdapter, not here.
              parser.backspace()
              throw new ScalaJackError(
                parser.showError(
                  s"Multiple sub-classes of ${info.name} match field names $readFieldNames"
                )
              )
            // $COVERAGE-ON$
          }
    }

  def write[WIRE](
      t:      T,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeString(null, out)
      case _ =>
        sealedInfo.children.find(f => t.getClass <:< f.infoClass) match {
          case Some(implementation) => typeAdapters(implementation).asInstanceOf[TypeAdapter[T]]write(t, writer, out)
          // $COVERAGE-OFF$Should be impossible, but including here for safety.  Can't think of how to actaully trigger this for testing.
          case None =>
            throw new IllegalStateException(
              s"Given object ($t) doesn't seem to be a sealed trait."
            )
          // $COVERAGE-ON$
        }
    }

case class CaseObjectTypeAdapter[T](
      info:             RType,
      values:           List[String]
    ) extends TypeAdapter[T]:
  
  val sealedInfo = info.asInstanceOf[SealedTraitInfo]

  def read(parser: Parser): T =
    parser.expectString() match {
      case null => null.asInstanceOf[T]
      case s: String if values.contains(s) =>
        val simpleNameLen = info.infoClass.getSimpleName.length+1
        val clazz = Class.forName(info.infoClass.getName.dropRight(simpleNameLen) + "." + s + "$")
        val objInstance = clazz.getField("MODULE$").get(null).asInstanceOf[T]
        objInstance
      case x =>
        parser.backspace()
        throw new ScalaJackError(parser.showError(s"Expected a valid subclass of ${info.name} but got $x")
        )
    }

  def write[WIRE](
    t:      T,
    writer: Writer[WIRE],
    out:    mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeString(null, out)
      case _    => writer.writeString(t.toString, out)
    }