package co.blocke.scalajack
package json.misc

import model._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.Matchers._
import scala.reflect.runtime.universe._

import scala.collection.mutable
import scala.collection.immutable.ListMap

object Factory1 extends TypeAdapterFactory.===.withOneTypeParam[List] {
  def create[E, T <: List[E]](next: TypeAdapterFactory)(
      implicit
      taCache:   TypeAdapterCache,
      tt:        TypeTag[T],
      ttX:       TypeTag[List[E]],
      ttElement: TypeTag[E]
  ): TypeAdapter[T] = {
    new TA1[T]().asInstanceOf[TypeAdapter[T]]
  }
}

object Factory2 extends TypeAdapterFactory.<:<.withOneTypeParam[List] {
  def create[E, T <: List[E]](next: TypeAdapterFactory)(
      implicit
      taCache:   TypeAdapterCache,
      tt:        TypeTag[T],
      ttX:       TypeTag[List[E]],
      ttElement: TypeTag[E]
  ): TypeAdapter[T] = {
    new TA2[T]().asInstanceOf[TypeAdapter[T]]
  }
}

object Factory3 extends TypeAdapterFactory.===.withTwoTypeParams[Map] {
  def create[E1, E2, T <: Map[E1, E2]](next: TypeAdapterFactory)(
      implicit
      taCache:    TypeAdapterCache,
      tt:         TypeTag[T],
      ttX:        TypeTag[Map[E1, E2]],
      ttElement1: TypeTag[E1],
      ttElement2: TypeTag[E2]
  ): TypeAdapter[T] = {
    new TA3[T]().asInstanceOf[TypeAdapter[T]]
  }
}

object Factory7 extends TypeAdapterFactory.=:=.withTwoTypeParams[ListMap] {
  def create[E1, E2](next: TypeAdapterFactory)(
      implicit
      tt:         TypeTag[ListMap[E1, E2]],
      ttElement1: TypeTag[E1],
      ttElement2: TypeTag[E2]
  ): TypeAdapter[ListMap[E1, E2]] =
    new TA7[ListMap[E1, E2]]().asInstanceOf[TypeAdapter[ListMap[E1, E2]]]
}

object Factory4 extends TypeAdapterFactory.<:<.withTwoTypeParams[Map] {
  def create[E1, E2, T <: Map[E1, E2]](next: TypeAdapterFactory)(
      implicit
      taCache:    TypeAdapterCache,
      tt:         TypeTag[T],
      ttX:        TypeTag[Map[E1, E2]],
      ttElement1: TypeTag[E1],
      ttElement2: TypeTag[E2]
  ): TypeAdapter[T] = {
    new TA4[T]().asInstanceOf[TypeAdapter[T]]
  }
}

object Factory5 extends TypeAdapterFactory.<:<[String] {
  def create[T <: String](
      next: TypeAdapterFactory
  )(implicit taCache: TypeAdapterCache, tt: TypeTag[T]): TypeAdapter[T] = {
    new TA5[T]().asInstanceOf[TypeAdapter[T]]
  }
}

object Factory6 extends TypeAdapterFactory.=:=.withOneTypeParam[Set] {
  def create[E](next: TypeAdapterFactory)(
      implicit
      taCache:   TypeAdapterCache,
      tt:        TypeTag[Set[E]],
      ttElement: TypeTag[E]
  ): TypeAdapter[Set[E]] =
    new TA6[Set[E]]().asInstanceOf[TypeAdapter[Set[E]]]
}

trait EmptyTypeAdapter[T] extends TypeAdapter[T] {
  def read(parser: Parser): T = null.asInstanceOf[T]
  def write[WIRE](
      t:      T,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = {}
}

class TA1[T]() extends EmptyTypeAdapter[T]
class TA2[T]() extends EmptyTypeAdapter[T]
class TA3[T]() extends EmptyTypeAdapter[T]
class TA4[T]() extends EmptyTypeAdapter[T]
class TA5[T]() extends EmptyTypeAdapter[T]
class TA6[T]() extends EmptyTypeAdapter[T]
class TA7[T]() extends EmptyTypeAdapter[T]
class TAx[T]() extends EmptyTypeAdapter[T]

class TypeAdapterFactorySpec extends AnyFunSpec {
  type Phone = String

  val context: TypeAdapterCache = TypeAdapterCache(ScalaJack())
    .withFactory(Factory1)
    .withFactory(Factory2)
    .withFactory(Factory3)
    .withFactory(Factory7)
    .withFactory(Factory4)
    .withFactory(Factory5)
    .withFactory(Factory6)

  describe(
    "----------------------------\n:  TypeAdapter Type Tests  :\n----------------------------"
  ) {
      it("Find with === (one type param)") {
        context.typeAdapterOf[List[Any]].getClass.getName should be(
          "co.blocke.scalajack.json.misc.TA1"
        )
      }
      it("Find with =:= (one type param)") {
        context.typeAdapterOf[Set[Any]].getClass.getName should be(
          "co.blocke.scalajack.json.misc.TA6"
        )
      }
      it("Find with <:< (one type param)") {
        context.typeAdapterOf[List[Int]].getClass.getName should be(
          "co.blocke.scalajack.json.misc.TA2"
        )
      }
      it("Find with === (two type params)") {
        context.typeAdapterOf[Map[Any, Any]].getClass.getName should be(
          "co.blocke.scalajack.json.misc.TA3"
        )
      }
      it("Find with =:= (two type params)") {
        context.typeAdapterOf[ListMap[Any, Any]].getClass.getName should be(
          "co.blocke.scalajack.json.misc.TA7"
        )
      }
      it("Find with <:< (two type params)") {
        context.typeAdapterOf[Map[String, Int]].getClass.getName should be(
          "co.blocke.scalajack.json.misc.TA4"
        )
      }
      it("Show implicit type support with <:<") {
        context.typeAdapterOf[Phone].getClass.getName should be(
          "co.blocke.scalajack.json.misc.TA5"
        )
        context.typeAdapterOf[String].getClass.getName should be(
          "co.blocke.scalajack.json.misc.TA5"
        )
      }
      it("Register a simple/primitive TypeAdapter") {
        val ta = new TAx[Boolean]()
        val factory = TypeAdapterFactory.=:=[Boolean](ta)
        val c2 = context.withFactory(factory)
        c2.typeAdapterOf[Boolean].getClass.getName should be(
          "co.blocke.scalajack.json.misc.TAx"
        )
      }
    }
}
