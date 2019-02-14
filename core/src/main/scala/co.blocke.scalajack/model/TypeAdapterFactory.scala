package co.blocke.scalajack
package model

import util.TypeTags

/*
 All these comparators are confusing!  So let's define them...  First some setting:

 type Phone = String
 trait Foo
 class Bar() extends Foo
 class Blah() extends Foo
 class Other() extends Bar

 A === B -> Type A exactly matches type B.  Phone === String is false.

 A =:= B -> Type A can be implicitly converted to type B.   Phone =:= String is true

 A <:< B -> Type A is a subtype of type B (e.g. inheritance).   Other <:< Bar is true

 In this examples Bar and Blah have no relation at all to each other--no comparison matches.

 */
object TypeAdapterFactory {

  def apply(factories: List[TypeAdapterFactory]): TypeAdapterFactory =
    factories match {
      case Nil =>
        new TypeAdapterFactory {
          override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
            next.typeAdapterOf[T]
        }

      case head :: tail =>
        new TypeAdapterFactory {
          override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
            head.typeAdapterOf[T](next = TypeAdapterFactory(tail))
          }
        }
    }

  trait FromClassSymbol extends TypeAdapterFactory {

    override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
      val typeSymbol = tt.tpe.typeSymbol
      if (typeSymbol.isClass) {
        typeAdapterOf[T](typeSymbol.asClass, next)
      } else {
        next.typeAdapterOf[T]
      }
    }

    def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T]
  }

  abstract class ===[X](implicit ttFactory: TypeTag[X]) extends TypeAdapterFactory {

    def create(next: TypeAdapterFactory)(implicit tt: TypeTag[X]): TypeAdapter[X]

    override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
      if (tt.tpe.toString == ttFactory.tpe.toString) {
        create(next)(tt.asInstanceOf[TypeTag[X]]).asInstanceOf[TypeAdapter[T]]
      } else {
        next.typeAdapterOf[T]
      }
    }
  }

  abstract class =:=[X](implicit ttFactory: TypeTag[X]) extends TypeAdapterFactory {

    def create(next: TypeAdapterFactory)(implicit tt: TypeTag[X]): TypeAdapter[X]

    override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
      if (tt.tpe =:= ttFactory.tpe) {
        create(next)(tt.asInstanceOf[TypeTag[X]]).asInstanceOf[TypeAdapter[T]]
      } else {
        next.typeAdapterOf[T]
      }
  }

  object === {

    abstract class withOneTypeParam[X[_]](implicit ttFactory: TypeTag[X[Any]]) extends TypeAdapterFactory {

      def create[E, T <: X[E]](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T], ttX: TypeTag[X[E]], ttElement: TypeTag[E]): TypeAdapter[T]

      override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
        if (tt.tpe == ttFactory.tpe) {
          type E = Any
          type TT = X[E]
          val elementType :: Nil = tt.tpe.typeArgs
          create[E, TT](next)(context, tt.asInstanceOf[TypeTag[TT]], TypeTags.of[X[E]](appliedType(ttFactory.tpe.typeConstructor, elementType)), TypeTags.of[E](elementType)).asInstanceOf[TypeAdapter[T]]
        } else {
          next.typeAdapterOf[T]
        }
      }
    }

    abstract class withTwoTypeParams[X[_, _]](implicit ttFactory: TypeTag[X[Any, Any]]) extends TypeAdapterFactory {

      def create[E1, E2, T <: X[E1, E2]](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T], ttX: TypeTag[X[E1, E2]], ttElement1: TypeTag[E1], ttElement2: TypeTag[E2]): TypeAdapter[T]

      override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
        if (tt.tpe == ttFactory.tpe) {
          type E1 = Any
          type E2 = Any
          type TT = X[E1, E2]
          val elementType1 :: elementType2 :: Nil = tt.tpe.typeArgs
          create[E1, E2, TT](next)(context, tt.asInstanceOf[TypeTag[TT]], TypeTags.of[X[E1, E2]](appliedType(ttFactory.tpe.typeConstructor, elementType1, elementType2)), TypeTags.of[E1](elementType1), TypeTags.of[E2](elementType2)).asInstanceOf[TypeAdapter[T]]
        } else {
          next.typeAdapterOf[T]
        }
    }
  }

  object =:= {

    def apply[T: TypeTag](typeAdapter: TypeAdapter[T]): TypeAdapterFactory =
      new TypeAdapterFactory.=:=[T] {
        override def create(next: TypeAdapterFactory)(implicit tt: TypeTag[T]): TypeAdapter[T] = typeAdapter
      }

    abstract class withOneTypeParam[X[_]](implicit ttFactory: TypeTag[X[Any]]) extends TypeAdapterFactory {

      def create[E](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[X[E]], ttElement: TypeTag[E]): TypeAdapter[X[E]]

      override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
        if (tt.tpe.typeConstructor =:= ttFactory.tpe.typeConstructor) {
          type E = Any
          val elementType :: Nil = tt.tpe.typeArgs
          create[E](next)(context, tt.asInstanceOf[TypeTag[X[E]]], TypeTags.of[E](elementType)).asInstanceOf[TypeAdapter[T]]
        } else {
          next.typeAdapterOf[T]
        }
    }

    abstract class withTwoTypeParams[X[_, _]](implicit ttFactory: TypeTag[X[Any, Any]]) extends TypeAdapterFactory {

      def create[E1, E2](next: TypeAdapterFactory)(implicit tt: TypeTag[X[E1, E2]], ttElement1: TypeTag[E1], ttElement2: TypeTag[E2]): TypeAdapter[X[E1, E2]]

      override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
        if (tt.tpe.typeConstructor =:= ttFactory.tpe.typeConstructor) {
          type E1 = Any
          type E2 = Any
          val elementType1 :: elementType2 :: Nil = tt.tpe.typeArgs
          create[E1, E2](next)(tt.asInstanceOf[TypeTag[X[E1, E2]]], TypeTags.of[E1](elementType1), TypeTags.of[E2](elementType2)).asInstanceOf[TypeAdapter[T]]
        } else {
          next.typeAdapterOf[T]
        }
    }
  }

  abstract class <:<[X](implicit ttFactory: TypeTag[X]) extends TypeAdapterFactory {

    def create[T <: X](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T]

    override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
      if (tt.tpe <:< ttFactory.tpe) {
        type TT = X
        create[TT](next)(context, tt.asInstanceOf[TypeTag[TT]]).asInstanceOf[TypeAdapter[T]]
      } else {
        next.typeAdapterOf[T]
      }
  }

  object <:< {

    abstract class withOneTypeParam[X[_]](implicit ttFactory: TypeTag[X[Any]]) extends TypeAdapterFactory {

      def create[E, T <: X[E]](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T], ttX: TypeTag[X[E]], ttElement: TypeTag[E]): TypeAdapter[T]

      override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
        tt.tpe.baseType(ttFactory.tpe.typeSymbol) match {
          case NoType =>
            next.typeAdapterOf[T]

          case baseType =>
            type E = Any
            type TT = X[E]
            val elementType :: Nil = baseType.typeArgs
            create[E, TT](next)(context, tt.asInstanceOf[TypeTag[TT]], TypeTags.of[X[E]](baseType), TypeTags.of[E](elementType)).asInstanceOf[TypeAdapter[T]]
        }
    }

    abstract class withTwoTypeParams[X[_, _]](implicit ttFactory: TypeTag[X[Any, Any]]) extends TypeAdapterFactory {

      def create[E1, E2, T <: X[E1, E2]](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T], ttX: TypeTag[X[E1, E2]], ttElement1: TypeTag[E1], ttElement2: TypeTag[E2]): TypeAdapter[T]

      override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
        tt.tpe.baseType(ttFactory.tpe.typeSymbol) match {
          case NoType =>
            next.typeAdapterOf[T]

          case baseType =>
            type E1 = Any
            type E2 = Any
            type TT = X[E1, E2]
            val elementType1 :: elementType2 :: Nil = baseType.typeArgs
            create[E1, E2, TT](next)(context, tt.asInstanceOf[TypeTag[TT]], TypeTags.of[X[E1, E2]](baseType), TypeTags.of[E1](elementType1), TypeTags.of[E2](elementType2)).asInstanceOf[TypeAdapter[T]]
        }
    }
  }
}

trait TypeAdapterFactory {
  def typeAdapterOf[T](implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = typeAdapterOf[T](DefaultTypeAdapterFactory)
  def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T]
}