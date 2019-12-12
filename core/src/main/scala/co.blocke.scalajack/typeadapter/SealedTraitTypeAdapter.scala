package co.blocke.scalajack
package typeadapter

import model._

import scala.collection.immutable
import scala.collection.mutable
import scala.util.Try
import scala.reflect.runtime.universe._

object SealedTraitTypeAdapterFactory extends TypeAdapterFactory {

  trait Subclass[T] {
    type U <: T
    val subclassType: Type
    val subclassClass: Class[U]
    val typeAdapter: TypeAdapter[U]
    val memberNames: Set[MemberName]
  }

  override def typeAdapterOf[T](
      next: TypeAdapterFactory
  )(implicit taCache: TypeAdapterCache, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe.typeSymbol.isClass) {
      tt.tpe.typeSymbol.asClass.knownDirectSubclasses.toList match {
        case Nil =>
          next.typeAdapterOf[T]

        case subclassSymbols =>
          val subclassTypes = subclassSymbols.map(_.asType.toType)

          val subclassAttempts =
            for (subclassType2 <- subclassTypes)
              yield Try {
              type U = T

              val subclassTypeAdapter = taCache
                .typeAdapter(subclassType2)
                .asInstanceOf[TypeAdapter[U]]
              val memberNames2 = subclassTypeAdapter
                .as[CaseClassTypeAdapter[_]]
                .fieldMembersByName
                .keySet
              new Subclass[T] {
                override type U = T
                override val subclassType: Type = subclassType2
                override val subclassClass: Class[U] =
                  runtimeClass(subclassType).asInstanceOf[Class[U]]
                override val typeAdapter: TypeAdapter[U] =
                  subclassTypeAdapter
                override val memberNames: Set[MemberName] = memberNames2
              }
            }
          null.asInstanceOf[TypeAdapter[T]]

          val subclasses = subclassAttempts.map(_.get)

          def allPairsOf[E](superset: Set[E]): Set[(E, E)] =
            superset
              .subsets(2)
              .map(_.toList)
              .map(list => {
                val List(a, b) = list.take(2)
                (a, b)
              })
              .toSet

          val allPairsOfSubclasses: Set[(Subclass[T], Subclass[T])] =
            allPairsOf(subclasses.toSet)

          val isCaseObjects = subclasses.foldRight(true) {
            case (oneSubclass, acc) =>
              acc && oneSubclass.subclassType.typeSymbol.isModuleClass
          }
          if (isCaseObjects)
            new CaseObjectTypeAdapter(
              subclassTypes.map(_.typeSymbol.name.toString)
            )
          else {
            val someSubclassesAreAmbiguous = allPairsOfSubclasses.exists {
              case (a, b) =>
                a.memberNames.subsetOf(b.memberNames) || b.memberNames.subsetOf(
                  a.memberNames
                )
            }

            val impls: Set[SealedImplementation[T]] = subclasses.map {
              subclass =>
                new SealedImplementation[T] {
                  private val runtimeClass: RuntimeClass =
                    subclass.subclassClass
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
              val builderFactory = taCache
                .typeAdapterOf[Map[String, Any]]
                .asInstanceOf[MapLikeTypeAdapter[String, Any, Map[String, Any]]]
                .builderFactory
              new SealedTraitTypeAdapter[T](
                impls,
                builderFactory,
                taCache.jackFlavor
              )
            }
          }
      }
    } else {
      // $COVERAGE-OFF$This should be impossible--can't trigger it.  Should always be a class of some kind arrive at this adapter.
      next.typeAdapterOf[T]
      // $COVERAGE-ON$
    }
}

class CaseObjectTypeAdapter[T](subclasses: List[String])(
    implicit
    tt: TypeTag[T]
) extends TypeAdapter[T]
  with Stringish {
  def read(parser: Parser): T = parser.expectString() match {
    case null => null.asInstanceOf[T]
    case s: String if subclasses.contains(s) =>
      val clazz =
        Class.forName(tt.tpe.typeSymbol.asClass.owner.fullName + "." + s + "$")
      val objInstance = clazz.getField("MODULE$").get(null).asInstanceOf[T]
      objInstance
    case x =>
      parser.backspace()
      throw new ScalaJackError(
        parser
          .showError(s"Expected a valid subclass of ${typeOf[T]} but got $x")
      )
  }

  def write[WIRE](
      t:      T,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
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

class SealedTraitTypeAdapter[T](
    implementations: immutable.Set[SealedImplementation[T]],
    builderFactory:  MethodMirror,
    jackFlavor:      JackFlavor[_]
)(implicit tt: TypeTag[T])
  extends TypeAdapter[T] {

  def read(parser: Parser): T = {
    val savedReader = parser.mark()
    if (parser.peekForNull)
      null.asInstanceOf[T]
    else {
      val allFieldNames = parser
        .expectMap[String, Any, Map[String, Any]](
          jackFlavor.stringTypeAdapter,
          jackFlavor.anyTypeAdapter,
          builderFactory()
            .asInstanceOf[mutable.Builder[(String, Any), Map[String, Any]]]
        )
        .keySet
      implementations.filter(
        implementation => implementation.fieldNames.subsetOf(allFieldNames)
      ) match {
          case setOfOne if setOfOne.size == 1 =>
            parser.revertToMark(savedReader)
            setOfOne.head.typeAdapter.read(parser)

          case emptySet if emptySet.isEmpty =>
            parser.backspace()
            throw new ScalaJackError(
              parser.showError(
                s"No sub-classes of ${tt.tpe.typeSymbol.fullName} match field names $allFieldNames"
              )
            )

          case _ =>
            // $COVERAGE-OFF$Should be impossible--here for safety.  Something to trigger this would be ambiguous and would then be detected as a WrappedSealedTraitTypeAdapter, not here.
            parser.backspace()
            throw new ScalaJackError(
              parser.showError(
                s"Multiple sub-classes of ${tt.tpe.typeSymbol.fullName} match field names $allFieldNames"
              )
            )
          // $COVERAGE-ON$
        }
    }
  }

  def write[WIRE](
      t:      T,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null =>
        writer.writeString(null, out)
      case _ =>
        implementations.find(_.isInstance(t)) match {
          case Some(implementation) =>
            implementation.typeAdapter.write(t, writer, out)
          // $COVERAGE-OFF$Should be impossible, but including here for safety.  Can't think of how to actaully trigger this for testing.
          case None =>
            throw new IllegalStateException(
              s"Given object ($t) doesn't seem to be a sealed trait."
            )
          // $COVERAGE-ON$
        }
    }
}

class WrappedSealedTraitTypeAdapter[T](
    wrappedTypeAdapter: TypeAdapter[T], // (probably a TraitTypeAdapter at runtime)
    implementations:    immutable.Set[SealedImplementation[T]]
)(implicit tt: TypeTag[T])
  extends TypeAdapter[T] {

  def read(parser: Parser): T = {
    val inst = wrappedTypeAdapter.read(parser)
    if (implementations.exists(_.isInstance(inst)))
      inst
    else {
      parser.backspace()
      throw new ScalaJackError(
        parser.showError(
          s"${inst.getClass.getName} isn't a subclass of sealed trait ${tt.tpe.typeSymbol.fullName}"
        )
      )
    }
  }

  def write[WIRE](
      t:      T,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    wrappedTypeAdapter.write(t, writer, out)
}
