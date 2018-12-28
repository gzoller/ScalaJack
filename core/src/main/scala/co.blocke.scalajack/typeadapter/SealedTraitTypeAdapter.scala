package co.blocke.scalajack
package typeadapter

import util.Path
import model._

import scala.collection.immutable
import scala.util.Try

object SealedTraitTypeAdapterFactory extends TypeAdapterFactory {

  trait Subclass[T] {
    type U <: T
    val subclassType: Type
    val subclassClass: Class[U]
    val typeAdapter: TypeAdapter[U]
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
              val memberNames2 = subclassTypeAdapter.as[ClassHelper.ClassLikeTypeAdapter[Any]].members.map(_.name)
              new Subclass[T] {
                override type U = T
                override val subclassType: Type = subclassType2
                override val subclassClass: Class[U] = runtimeClass(subclassType).asInstanceOf[Class[U]]
                override val typeAdapter: TypeAdapter[U] = subclassTypeAdapter
                override val memberNames: Set[MemberName] = memberNames2.toSet
              }
            }
          null.asInstanceOf[TypeAdapter[T]]

          if (subclassAttempts.exists(_.isFailure)) {
            println("--3--")
            // If subclassAttempts is full of Failure, the "subclasses" may be case objects, not case classes.
            // This is a common alternative implementation for Enumerations.
            subclassTypes.headOption match {
              case Some(t) if t.typeSymbol.isModuleClass => // Bake a serializer/deserializer than handles case objects
                new CaseObjectTypeAdapter(subclassTypes.map(_.typeSymbol.name.toString))
              case _ => typeAdapterOf[T]
            }
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

            val someSubclassesAreAmbiguous = allPairsOfSubclasses.exists {
              case (a, b) =>
                a.memberNames.subsetOf(b.memberNames) || b.memberNames.subsetOf(a.memberNames)
            }

            if (someSubclassesAreAmbiguous)
              next.typeAdapterOf[T]
            else {
              val impls: Set[SealedImplementation[T]] = subclasses.map { subclass =>
                new SealedImplementation[T] {
                  private val runtimeClass: RuntimeClass = subclass.subclassClass
                  val typeAdapter: TypeAdapter[T] = subclass.typeAdapter
                  override val fieldNames: Set[String] = subclass.memberNames

                  override def isInstance(tagged: T): Boolean =
                    tagged match {
                      case null => false
                      case x    => runtimeClass.isInstance(x)
                    }

                  //                    override def write[IR, WIRE](tagged: TypeTagged[T])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
                  //                      irTransceiver.write(tagged)
                }
              }.toSet
              new SealedTraitTypeAdapter[T](impls)
            }
          }
      }
    } else {
      next.typeAdapterOf[T]
    }
}

class CaseObjectTypeAdapter[T](subclasses: List[String])(implicit tt: TypeTag[T]) extends TypeAdapter[T] {
  def read(path: Path, reader: Reader, isMapKey: Boolean): T = reader.readString(path) match {
    case null => null.asInstanceOf[T]
    case s: String if subclasses.contains(s) =>
      val clazz = Class.forName(tt.tpe.typeSymbol.asClass.owner.fullName + "." + s + "$")
      val objInstance = clazz.getField("MODULE$").get(null).asInstanceOf[T]
      objInstance
    case x => throw new SJReadError(path, Unexpected, s"Expected a valid subclass of ${typeOf[T]} but got ${x}", List(typeOf[T].toString, x))
  }
}

trait SealedImplementation[T] {
  val fieldNames: immutable.Set[String]
  val typeAdapter: TypeAdapter[T]
  def isInstance(tagged: T): Boolean
  //  def write[IR, WIRE](tagged: TypeTagged[T])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR]
}

class SealedTraitTypeAdapter[T](implementations: immutable.Set[SealedImplementation[T]])(implicit context: Context, tt: TypeTag[T]) extends TypeAdapter[T] {
  def read(path: Path, reader: Reader, isMapKey: Boolean): T = {
    reader.savePos()
    reader.readMap(path, Map.canBuildFrom[String, Any], context.typeAdapterOf[String], context.typeAdapterOf[Any], isMapKey) match {
      case null => null.asInstanceOf[T]
      case fields: Map[String, Any] =>
        val allFieldNames = fields.map(_._1).toSet
        implementations.filter(implementation => implementation.fieldNames.subsetOf(allFieldNames)) match {
          case setOfOne if setOfOne.size == 1 =>
            reader.rollbackToSave()
            setOfOne.head.typeAdapter.read(path, reader, isMapKey)

          case emptySet if emptySet.isEmpty =>
            throw new SJReadError(path, Invalid, s"No sub-classes of ${tt.tpe.typeSymbol.fullName} match field names $allFieldNames", List(tt.tpe.typeSymbol.fullName, allFieldNames.mkString("[", ",", "]")))

          case _ =>
            throw new SJReadError(path, Invalid, s"Multiple sub-classes of ${tt.tpe.typeSymbol.fullName} match field names $allFieldNames", List(tt.tpe.typeSymbol.fullName, allFieldNames.mkString("[", ",", "]")))
        }
    }
  }
}

/*
class CaseObjectIRTransceiver[T](subclasses: List[String])(implicit tt: TypeTag[T]) extends IRTransceiver[T] {

  self =>

  private val CaseObjectType: Type = typeOf[T]
  private val taggedNull: TypeTagged[T] = TypeTagged(null.asInstanceOf[T], tt.tpe)

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[T] =
    ir match {
      case IRNull() => ReadSuccess(taggedNull)

      case IRString(s) if (subclasses.contains(s)) =>
        val clazz = Class.forName(tt.tpe.typeSymbol.asClass.owner.fullName + "." + s + "$")
        val objInstance = clazz.getField("MODULE$").get(null).asInstanceOf[T]
        ReadSuccess(TypeTagged(objInstance, typeOf[T]))

      case _ => ReadFailure(path, ReadError.Unexpected(s"Expected a valid subclass of $CaseObjectType", reportedBy = self))
    }

  override def write[IR, WIRE](tagged: TypeTagged[T])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
    tagged match {
      case TypeTagged(null) => WriteSuccess(IRNull())
      case TypeTagged(c)    => WriteSuccess(IRString(c.toString))
    }
}0

object SealedTraitIRTransceiver {
  trait Implementation[T] {
    val fieldNames: immutable.Set[String]
    val irTransceiver: IRTransceiver[T]
    def isInstance(tagged: TypeTagged[T]): Boolean
    def write[IR, WIRE](tagged: TypeTagged[T])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR]
  }
}

class SealedTraitIRTransceiver[T](implementations: immutable.Set[SealedTraitIRTransceiver.Implementation[T]])(implicit tt: TypeTag[T]) extends IRTransceiver[T] {

  self =>

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[T] =
    ir match {
      case IRObject(fields) =>
        val allFieldNames = fields.map(_._1).toSet
        implementations.filter(implementation => implementation.fieldNames.subsetOf(allFieldNames)) match {
          case emptySet if emptySet.isEmpty =>
            throw new RuntimeException(s"No sub-classes of ${tt.tpe.typeSymbol.fullName} match field names $allFieldNames")

          case setOfOne if setOfOne.size == 1 =>
            val implementation = setOfOne.head
            implementation.irTransceiver.read(path, ir)

          case _ =>
            throw new RuntimeException(s"Multiple sub-classes of ${tt.tpe.typeSymbol.fullName} match field names $allFieldNames")
        }

      case _ =>
        ReadFailure(path, ReadError.Unexpected("Expected a JSON object", reportedBy = self))
    }

  override def write[IR, WIRE](tagged: TypeTagged[T])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
    tagged match {
      case TypeTagged(null) => WriteSuccess(IRNull())
      case TypeTagged(_) =>
        implementations.find(_.isInstance(tagged)) match {
          case Some(implementation) => implementation.write(tagged)
          case None                 => ??? // TODO: What's this ??? mean here?
        }
    }
}
*/ 