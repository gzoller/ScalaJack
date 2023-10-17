package co.blocke.scalajack
package typeadapter

import model._

import co.blocke.scala_reflection.impl.Clazzes._
import co.blocke.scala_reflection.info._
import co.blocke.scala_reflection._

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.language.implicitConversions


object ArrayTypeAdapterFactory extends TypeAdapterFactory:

  def matches(concrete: RType): Boolean = 
    concrete match {
      case _: ArrayInfo => true
      case _: JavaArrayInfo => true
      case _ => false
    }

  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[_] = 
    concrete match {
      case arrInfo: ArrayInfo =>
        val elementInfo = arrInfo.elementType
        ArrayTypeAdapter(
          concrete, 
          elementInfo.isInstanceOf[OptionInfo],
          elementInfo,
          taCache.typeAdapterOf(elementInfo))
      case javaInfo: JavaArrayInfo =>
        val elementInfo = javaInfo.elementType
        ArrayTypeAdapter(
          concrete, 
          false, // TODO: Support java.Optional ==>  elementInfo.isInstanceOf[OptionInfo],
          elementInfo,
          taCache.typeAdapterOf(elementInfo))
    }


case class ArrayTypeAdapter[ELEM](
    info:               RType,
    elemIsOptional:     Boolean,
    elementType:        RType,
    elementTypeAdapter: TypeAdapter[ELEM]
  ) extends TypeAdapter[Array[ELEM]] with ScalarTypeAdapter[Array[ELEM]]:

  def read(parser: Parser): Array[ELEM] = 
    parser.peekForNull match {
      case true               => null
      case _                  => 
        val classtag = ClassTag[ELEM](elementType.infoClass)
        val builder: mutable.Builder[ELEM,Array[ELEM]] = Array.newBuilder[ELEM](classtag.asInstanceOf[ClassTag[ELEM]]).asInstanceOf[mutable.Builder[ELEM,Array[ELEM]]]
        val values = parser.expectList(elementTypeAdapter, builder)
        builder.result
    }

  def write[WIRE](t: Array[ELEM], writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit = 
    t match {
      case null                => writer.writeNull(out)
      case _ if elemIsOptional => 
        writer.writeArray(
          t.toList.filterNot(_ == None),
          elementTypeAdapter,
          out
        )
      case _ =>
        writer.writeArray(t.toList, elementTypeAdapter, out)
    }
