package co.blocke.scalajack
package delimited

import model._
import typeadapter.{EitherTypeAdapter, MaybeStringWrapTypeAdapter, StringWrapTypeAdapter}
import co.blocke.scala_reflection._
import co.blocke.scala_reflection.info.EitherInfo
import co.blocke.scala_reflection.impl.Clazzes._

import scala.collection.mutable.Builder
import scala.util.{ Failure, Success, Try }

/**
 * The only reason this machinery exists for delimited either is that writes need to be string-wrapped, where they don't in the general/JSON case.
 */
object DelimitedEitherTypeAdapterFactory extends TypeAdapterFactory:
  
  def matches(concrete: RType): Boolean = 
    concrete match {
      case _: EitherInfo => true
      case _ => false
    }

  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[_] =
    val eitherInfo = concrete.asInstanceOf[EitherInfo]
    val leftInfo = eitherInfo.leftType
    val rightInfo = eitherInfo.rightType

    if( leftInfo.infoClass <:< rightInfo.infoClass || rightInfo.infoClass <:< leftInfo.infoClass)
      throw new IllegalArgumentException(
        s"Types ${leftInfo.name} and ${rightInfo.name} are not mutually exclusive"
      )

    val leftTypeAdapter = taCache.typeAdapterOf(leftInfo) match {
      case ta if ta.isStringish => ta
      case ta => MaybeStringWrapTypeAdapter(taCache.jackFlavor, ta)
    }
    val rightTypeAdapter = taCache.typeAdapterOf(rightInfo) match {
      case ta if ta.isStringish => ta
      case ta => MaybeStringWrapTypeAdapter(taCache.jackFlavor, ta)
    }

    EitherTypeAdapter(
      concrete,
      leftTypeAdapter,
      rightTypeAdapter)