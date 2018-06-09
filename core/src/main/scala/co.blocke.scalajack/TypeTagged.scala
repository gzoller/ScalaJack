package co.blocke.scalajack

import scala.reflect.runtime.universe.Type

object TypeTagged {

  /**
   * Enables pattern matching.
   */
  @inline final def unapply[T](tagged: TypeTagged[T]): tagged.type = tagged

  def apply[T](value: T, valueType: Type): TypeTagged[T] = Fixed(value, valueType)

  private case class Fixed[+T](get: T, tpe: Type) extends TypeTagged[T]

}

/**
 * A combination of a value and its known [[Type]].
 *
 * @tparam T
 */
trait TypeTagged[+T] {

  def get: T

  def tpe: Type

  /**
   * Used to fulfill the requirements of [[TypeTagged#unapply]].
   */
  @inline final def isEmpty: Boolean = false

}
