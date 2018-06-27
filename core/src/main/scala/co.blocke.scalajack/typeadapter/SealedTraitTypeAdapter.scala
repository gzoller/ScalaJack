package co.blocke.scalajack
package typeadapter

import scala.language.existentials
import scala.util.Try

object SealedTraitTypeAdapter extends TypeAdapterFactory {

  trait Subclass[T] {
    type U <: T
    val subclassType: Type
    val subclassClass: Class[U]
    val typeAdapter: TypeAdapter[U]
    val deserializer: Deserializer[U]
    val serializer: Serializer[U]
    val memberNames: Set[MemberName]
  }

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe.typeSymbol.isClass) {
      tt.tpe.typeSymbol.asClass.knownDirectSubclasses.toList match {
        case Nil =>
          next.typeAdapterOf[T]

        case subclassSymbols =>
          val subclassTypes = subclassSymbols.map(_.asType.toType)

          val subclassAttempts =
            for (subclassType2 <- subclassTypes) yield Try {
              type U = T

              val subclassTypeAdapter = context.typeAdapter(subclassType2).asInstanceOf[TypeAdapter[U]]
              //.asInstanceOf[ClassLikeTypeAdapter[Any]]
              val memberNames2 = subclassTypeAdapter.as[ClassLikeTypeAdapter[Any]].members.map(_.name)
              //              Subclass[T](subclassType, runtimeClass(subclassType).asInstanceOf[Class[_ <: T]], subclassTypeAdapter, memberNames.toSet)
              new Subclass[T] {
                override type U = T
                override val subclassType: Type = subclassType2
                override val subclassClass: Class[U] = runtimeClass(subclassType).asInstanceOf[Class[U]]
                override val typeAdapter: TypeAdapter[U] = subclassTypeAdapter
                override val deserializer: Deserializer[U] = subclassTypeAdapter.deserializer
                override val serializer: Serializer[U] = subclassTypeAdapter.serializer
                override val memberNames: Set[MemberName] = memberNames2.toSet
              }
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
                /*
  FIXME
                override object deserializer extends Deserializer[T] {

                  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[T] =
                    json match {
                      case JsonObject(x) =>
                        val memberNames = new scala.collection.mutable.HashSet[MemberName]

                        ops.foreachObjectField(x.asInstanceOf[ops.ObjectFields], { (fieldName, fieldValue) =>
                          memberNames += fieldName
                        })

                        subclasses.filter(subclass => subclass.memberNames.subsetOf(memberNames)) match {
                          case Nil =>
                            throw new RuntimeException(s"No sub-classes of ${tt.tpe.typeSymbol.fullName} match member names $memberNames")

                          case matchingSubclass :: Nil =>
                            matchingSubclass.typeAdapter.deserializer.deserialize(path, json).asInstanceOf[DeserializationResult[T]]

                          case matchingSubclasses =>
                            throw new RuntimeException(s"Multiple sub-classes of ${tt.tpe.typeSymbol.fullName} match member names $memberNames")
                        }

                      case _ =>
                        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON object"))
                    }

                }
  */
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

              val deserializer = new SealedTraitDeserializer[T](subclasses.map({ subclass =>
                new SealedTraitDeserializer.Implementation[T] {
                  override val fieldNames: Set[String] = subclass.memberNames
                  override val deserializer: Deserializer[T] = subclass.deserializer
                }
              }).toSet)

              val serializer = new SealedTraitSerializer[T](subclasses.map({ subclass =>
                new SealedTraitSerializer.Implementation[T] {
                  private val runtimeClass: RuntimeClass = subclass.subclassClass
                  private val serializer: Serializer[T] = subclass.typeAdapter.serializer
                  override def isInstance(tagged: TypeTagged[T]): Boolean =
                    tagged match {
                      case TypeTagged(null) => false
                      case TypeTagged(x)    => runtimeClass.isInstance(x)
                    }
                  override def serialize[J](tagged: TypeTagged[T])(implicit ops: JsonOps[J]): SerializationResult[J] =
                    serializer.serialize(tagged)
                }
              }).toSet)

              TypeAdapter(deserializer, serializer)
            }
          }
      }
    } else {
      next.typeAdapterOf[T]
    }

}
