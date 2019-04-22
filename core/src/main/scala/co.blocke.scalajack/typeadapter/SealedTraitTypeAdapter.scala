package co.blocke.scalajack
package typeadapter

import util.Path
import model._

import scala.collection.immutable
import scala.collection.mutable.Builder
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

          val isCaseObjects = subclasses.foldRight(true) {
            case (oneSubclass, acc) =>
              acc && oneSubclass.subclassType.typeSymbol.isModuleClass
          }
          if (isCaseObjects)
            new CaseObjectTypeAdapter(subclassTypes.map(_.typeSymbol.name.toString))
          else {
            val someSubclassesAreAmbiguous = allPairsOfSubclasses.exists {
              case (a, b) =>
                a.memberNames.subsetOf(b.memberNames) || b.memberNames.subsetOf(a.memberNames)
            }

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
              }
            }.toSet
            if (someSubclassesAreAmbiguous)
              new WrappedSealedTraitTypeAdapter(next.typeAdapterOf[T], impls)
            else {
              val builderFactory = context.typeAdapterOf[Map[String, Any]].asInstanceOf[CanBuildMapTypeAdapter[String, Any, Map[String, Any]]].builderFactory
              new SealedTraitTypeAdapter[T](impls, builderFactory)
            }
          }
      }
    } else {
      // $COVERAGE-OFF$This should be impossible--can't trigger it.  Should always be a class of some kind arrive at this adapter.
      next.typeAdapterOf[T]
      // $COVERAGE-ON$
    }
}

class CaseObjectTypeAdapter[T](subclasses: List[String])(implicit tt: TypeTag[T]) extends TypeAdapter[T] with Stringish {
  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): T = reader.readString(path) match {
    case null => null.asInstanceOf[T]
    case s: String if subclasses.contains(s) =>
      val clazz = Class.forName(tt.tpe.typeSymbol.asClass.owner.fullName + "." + s + "$")
      val objInstance = clazz.getField("MODULE$").get(null).asInstanceOf[T]
      objInstance
    case x =>
      reader.back
      throw new ReadUnexpectedError(reader.showError(path, s"Expected a valid subclass of ${typeOf[T]} but got ${x}"))
  }

  def write[WIRE](t: T, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit =
    t match {
      case null => writer.writeString(null, out)
      case _    => writer.writeString(t.toString, out)
    }
}

trait SealedImplementation[T] {
  val fieldNames: immutable.Set[String]
  val typeAdapter: TypeAdapter[T]
  def isInstance(tagged: T): Boolean
}

class SealedTraitTypeAdapter[T](implementations: immutable.Set[SealedImplementation[T]], builderFactory: MethodMirror)(implicit tt: TypeTag[T]) extends TypeAdapter[T] {

  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): T = {
    val savedReader = reader.copy
    reader.readMap[String, Any, Map[String, Any]](path, builderFactory, reader.jackFlavor.stringTypeAdapter, reader.jackFlavor.anyTypeAdapter) match {
      case null =>
        null.asInstanceOf[T]
      case fields: Map[String, Any] =>
        val allFieldNames = fields.map(_._1).toSet
        implementations.filter(implementation => implementation.fieldNames.subsetOf(allFieldNames)) match {
          case setOfOne if setOfOne.size == 1 =>
            reader.syncPositionTo(savedReader)
            setOfOne.head.typeAdapter.read(path, reader)

          case emptySet if emptySet.isEmpty =>
            reader.back
            throw new ReadInvalidError(reader.showError(path, s"No sub-classes of ${tt.tpe.typeSymbol.fullName} match field names $allFieldNames"))

          case _ =>
            // $COVERAGE-OFF$Should be impossible--here for safety.  Something to trigger this would be ambiguous and would then be detected as a WrappedSealedTraitTypeAdapter, not here.
            reader.back
            throw new ReadInvalidError(reader.showError(path, s"Multiple sub-classes of ${tt.tpe.typeSymbol.fullName} match field names $allFieldNames"))
          // $COVERAGE-ON$
        }
    }
  }

  def write[WIRE](t: T, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit =
    t match {
      case null =>
        writer.writeString(null, out)
      case _ =>
        implementations.find(_.isInstance(t)) match {
          case Some(implementation) => implementation.typeAdapter.write(t, writer, out, isMapKey)
          // $COVERAGE-OFF$Should be impossible, but including here for safety.  Can't think of how to actaully trigger this for testing.
          case None                 => throw new IllegalStateException(s"Given object ($t) doesn't seem to be a sealed trait.")
          // $COVERAGE-ON$
        }
    }
}

class WrappedSealedTraitTypeAdapter[T](
    wrappedTypeAdapter: TypeAdapter[T], // (probably a TraitTypeAdapter at runtime)
    implementations:    immutable.Set[SealedImplementation[T]]
)(implicit tt: TypeTag[T]) extends TypeAdapter[T] {

  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): T = {
    val inst = wrappedTypeAdapter.read(path, reader, isMapKey)
    if (implementations.exists(_.isInstance(inst)))
      inst
    else {
      reader.back
      throw new ReadInvalidError(reader.showError(path, s"${inst.getClass.getName} isn't a subclass of sealed trait ${tt.tpe.typeSymbol.fullName}"))
    }
  }

  def write[WIRE](t: T, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = wrappedTypeAdapter.write(t, writer, out, isMapKey)
}