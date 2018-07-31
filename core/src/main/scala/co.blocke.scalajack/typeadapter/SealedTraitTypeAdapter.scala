package co.blocke.scalajack.typeadapter

import co.blocke.scalajack.{ Context, MemberName, Reader, TokenType, TypeAdapter, TypeAdapterFactory, Writer }

import scala.reflect.runtime.universe.{ Type, TypeTag }
import scala.reflect.runtime.currentMirror
import scala.util.Try

object SealedTraitTypeAdapter extends TypeAdapterFactory {

  case class Subclass[T](
      subclassType:  Type,
      subclassClass: Class[_ <: T],
      typeAdapter:   TypeAdapter[Any],
      memberNames:   Set[MemberName]
  )

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
    tt.tpe.typeSymbol.asClass.knownDirectSubclasses.toList match {
      case Nil =>
        next.typeAdapterOf[T]

      case subclassSymbols =>
        val subclassTypes = subclassSymbols.map(_.asType.toType)

        val subclassAttempts =
          for (subclassType <- subclassTypes) yield Try {
            val subclassTypeAdapter = context.typeAdapter(subclassType).asInstanceOf[ClassLikeTypeAdapter[Any]]
            val memberNames = subclassTypeAdapter.members.map(_.name)
            Subclass[T](subclassType, currentMirror.runtimeClass(subclassType).asInstanceOf[Class[_ <: T]], subclassTypeAdapter, memberNames.toSet)
          }

        if (subclassAttempts.exists(_.isFailure)) {
          next.typeAdapterOf[T]
        } else {
          val subclasses = subclassAttempts.map(_.get)

          def allPairsOf[E](superset: Set[E]): Set[(E, E)] =
            superset.subsets(2)
              .map(_.toList)
              .map(list => {
                val a :: b :: Nil = list
                (a, b)
              })
              .toSet

          val allPairsOfSubclasses: Set[(Subclass[T], Subclass[T])] = allPairsOf(subclasses.toSet)

          val someSubclassesAreAmbiguous = allPairsOfSubclasses.exists({
            case (a, b) =>
              a.memberNames.subsetOf(b.memberNames) || b.memberNames.subsetOf(a.memberNames)
          })

          if (someSubclassesAreAmbiguous) {
            next.typeAdapterOf[T]
          } else {
            new TypeAdapter[T] {

              override def read(reader: Reader): T =
                reader.peek match {
                  case TokenType.BeginObject =>
                    val originalPosition = reader.position

                    reader.beginObject()

                    val memberNames = new scala.collection.mutable.HashSet[MemberName]

                    while (reader.hasMoreMembers) {
                      val memberName = reader.readString()
                      memberNames += memberName
                      reader.skipValue()
                    }

                    reader.position = originalPosition

                    subclasses.filter(subclass => subclass.memberNames.subsetOf(memberNames)) match {
                      case Nil =>
                        throw new RuntimeException(s"No sub-classes of ${tt.tpe.typeSymbol.fullName} match member names $memberNames")

                      case matchingSubclass :: Nil =>
                        matchingSubclass.typeAdapter.read(reader).asInstanceOf[T]

                      case matchingSubclasses =>
                        throw new RuntimeException(s"Multiple sub-classes of ${tt.tpe.typeSymbol.fullName} match member names $memberNames")
                    }

                  case TokenType.Null =>
                    reader.readNull().asInstanceOf[T]
                }

              override def write(value: T, writer: Writer): Unit =
                if (value == null) {
                  writer.writeNull()
                } else {
                  for (subclass <- subclasses) {
                    if (subclass.subclassClass.isInstance(value)) {
                      subclass.typeAdapter.write(value, writer)
                    }
                  }
                }

            }
          }
        }
    }
  }

}
