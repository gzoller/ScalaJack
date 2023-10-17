package co.blocke.scalajack
package typeadapter

import model._
import co.blocke.scala_reflection._
import co.blocke.scala_reflection.info.IntersectionInfo
import co.blocke.scala_reflection.impl.Clazzes._

import scala.collection.mutable.Builder
import scala.util.{ Failure, Success, Try }

object IntersectionTypeAdapterFactory extends TypeAdapterFactory:

  def matches(concrete: RType): Boolean = 
    concrete match {
      case _: IntersectionInfo => true
      case _ => false
    }

  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[_] =
    val intersectionInfo = concrete.asInstanceOf[IntersectionInfo]
    val leftInfo = intersectionInfo.leftType
    val rightInfo = intersectionInfo.rightType

    val leftTypeAdapter = taCache.typeAdapterOf(leftInfo)
    val rightTypeAdapter = taCache.typeAdapterOf(rightInfo)

    IntersectionTypeAdapter(
      concrete,
      leftTypeAdapter,
      rightTypeAdapter)


/** NOTE: This parsing "and" behavior only works for traits.  The Dotty Intersection type is
 *  likely broader than that, but traits is the main use case here.  It is unclear (from a 
 *  serialization perspective) how you would combine "fields" from, say, a primitive type.
 */
case class IntersectionTypeAdapter[L, R](
    info: RType,
    leftTypeAdapter:  TypeAdapter[L],
    rightTypeAdapter: TypeAdapter[R])(implicit taCache: TypeAdapterCache)
  extends TypeAdapter[L & R]:

  val syntheticTA = taCache.typeAdapterOf[L]
  override def isStringish: Boolean = leftTypeAdapter.isStringish && rightTypeAdapter.isStringish
  override def maybeStringish: Boolean = !isStringish

  def read(parser: Parser): L & R = 
    syntheticTA.read(parser).asInstanceOf[L & R]

  def write[WIRE](
      t:      L & R,
      writer: Writer[WIRE],
      out:    Builder[WIRE, WIRE]): Unit =
    syntheticTA.write(t.asInstanceOf[L], writer, out)
