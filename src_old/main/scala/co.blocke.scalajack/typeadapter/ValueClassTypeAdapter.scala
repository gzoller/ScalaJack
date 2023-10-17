package co.blocke.scalajack
package typeadapter

import model._
import co.blocke.scala_reflection.info._
import co.blocke.scala_reflection._
import scala.collection.mutable

object ValueClassTypeAdapterFactory extends TypeAdapterFactory:
  def matches(concrete: RType): Boolean = concrete match {
    case c: ScalaCaseClassInfo if c.isValueClass => true
    case c: ScalaClassInfo if c.isValueClass => true
    case _ => false
  }

  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[_] =
    val elementType = concrete.asInstanceOf[ClassInfo].fields(0).fieldType
    val field0 = concrete match {
      case c: ScalaCaseClassInfo => c.fields(0).asInstanceOf[ScalaFieldInfo]
      case c: ScalaClassInfo => c.fields(0).asInstanceOf[ScalaFieldInfo]
    }
    ValueClassTypeAdapter(concrete, field0, taCache.typeAdapterOf(elementType))


case class ValueClassTypeAdapter[VC, Value](
    info:               RType,
    field0:             ScalaFieldInfo,
    elementTypeAdapter: TypeAdapter[Value]
) extends TypeAdapter[VC] {

  // For wrapping map keys
  override def isStringish: Boolean = elementTypeAdapter.isStringish
  override def maybeStringish: Boolean = !elementTypeAdapter.isStringish

  def read(parser: Parser): VC = 
    info.asInstanceOf[ClassInfo].infoClass.getConstructors.head.newInstance(List(elementTypeAdapter.read(parser).asInstanceOf[Object]):_*).asInstanceOf[VC]

  def write[WIRE](
      t:      VC,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    elementTypeAdapter.write(t.getClass.getMethod(field0.name).invoke(t).asInstanceOf[Value], writer, out)
}
