package co.blocke.scalajack
package typeadapter

import model._
import co.blocke.scala_reflection._
import co.blocke.scala_reflection.info.TupleInfo
import java.lang.reflect.Method
import scala.collection.mutable
import scala.util.matching.Regex

object TupleTypeAdapterFactory extends TypeAdapterFactory:

  private val tupleFullName: Regex = """scala.Tuple(\d+)""".r

  def matches(concrete: RType): Boolean = 
    concrete match {
      case ti: TupleInfo => true
      case _ => false
    }

  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[_] =
    val fieldTAs = concrete.asInstanceOf[TupleInfo].tupleTypes.map{ f =>
        taCache.typeAdapterOf(f) match {
          case ota: OptionTypeAdapter[_] => ota.copy(nullIsNone = true)
          case jota: JavaOptionalTypeAdapter[_] => jota.copy(nullIsNone = true)
          case other => other
        }
      }.toList
    val writeFn = (t: Product) => fieldTAs.zip(t.productIterator)
    TupleTypeAdapter(concrete, writeFn, fieldTAs, concrete.infoClass.getConstructors.head)


case class TupleTypeAdapter[T](
  info:          RType,
  writeFn:       (Product) => List[(TypeAdapter[_], Any)],
  fieldTAs:      List[TypeAdapter[_]],
  constructor:   java.lang.reflect.Constructor[T]
  ) extends TypeAdapter[T] with Collectionish {

  def read(parser: Parser): T =
    if (parser.peekForNull) then
      null.asInstanceOf[T]
    else
      constructor.newInstance(parser.expectTuple(fieldTAs): _*).asInstanceOf[T]

  def write[WIRE](
      t:      T,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    if (t == null)
      writer.writeNull(out)
    else
      writer.writeTuple(t, writeFn, out)
}
