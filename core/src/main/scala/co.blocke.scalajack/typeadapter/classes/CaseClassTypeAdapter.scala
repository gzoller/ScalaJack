package co.blocke.scalajack
package typeadapter
package classes

import co.blocke.scalajack.model.ClassHelper.ExtraFieldValue
import util.Path
import model._

import scala.collection.immutable.{ ListMap, Map }
import scala.collection.mutable.Builder
import scala.reflect.runtime.universe._ //{ MethodMirror, Type, TypeTag }

case class CaseClassTypeAdapter[T](
    className:          String,
    typeMembersByName:  Map[String, ClassHelper.TypeMember[T]],
    fieldMembersByName: ListMap[String, ClassHelper.ClassFieldMember[T, Any]],
    constructorMirror:  MethodMirror,
    isSJCapture:        Boolean,
    collectionName:     Option[String]                                        = None)(implicit context: Context, tt: TypeTag[T]) extends ClassHelper.ClassLikeTypeAdapter[T] {

  // Hook for subclasses (e.g. Mongo) do to anything needed to handle the db key field(s) as given by the @DBKey annotation
  protected def handleDBKeys[AST](fieldValues: Map[String, Any]): Map[String, Any] = fieldValues

  // OK, so all this hokem is to figure out what to do for embedded type member (i.e. externalized type hint feature).  Doesn't seem to be needed
  // for anything else.
  private def fixTypeMemberFields[WIRE](concrete: Map[String, ClassHelper.TypeMember[_]]): ListMap[String, ClassHelper.ClassFieldMember[T, Any]] = {
    if (typeMembersByName.isEmpty) {
      fieldMembersByName
    } else {
      // If type members are defined --> externalized trait concrete type
      // Create a mapping of type label, e.g. 'T', to TypeMember where we've resolved the type member's value into a Type

      // Now buzz through known field members and replaces all the 'T' type with the concrete type and insert the correct concrete TypeAdapter.
      fieldMembersByName.map {
        case (name, field) =>
          val findConcrete = concrete.get(field.declaredValueType.toString) match {
            case Some(c) =>
              val runtimeTypeAdapter = c.runtimeConcreteType.map(context.typeAdapter(_))
              val newTypeAdapter = field.valueTypeAdapter match {
                case falling: FallbackTypeAdapter[_, _] =>
                  FallbackTypeAdapter(runtimeTypeAdapter.asInstanceOf[Option[TypeAdapter[Any]]], falling.orElseTypeAdapter)
                case _ =>
                  runtimeTypeAdapter.getOrElse(throw new IllegalStateException("Can't find type value (e.g. unknown class) for hint " + name))
              }
              field.copy(
                valueTypeAdapter  = newTypeAdapter,
                declaredValueType = c.runtimeConcreteType.getOrElse(field.declaredValueType)
              ).asInstanceOf[ClassHelper.ClassFieldMember[T, Any]]
            case None =>
              field
          }
          (name, findConcrete)
      }
    }
  }

  def read[WIRE](path: Path, reader: Transceiver[WIRE]): T = {
    val concreteTypes = typeMembersByName.map {
      case (name, tm) =>
        (tm.typeSignature.toString, tm.copy(runtimeConcreteType = {
          val lookAhead = reader.lookAheadForTypeHint(name, (s: String) => {
            reader.jackFlavor.typeValueModifier match {
              case Some(fn) => fn.apply(s) // apply type value modifier if there is one
              case None     => reader.jackFlavor.typeTypeAdapter.read(path, reader)
            }
          })
          if (lookAhead.isEmpty)
            reader.rollbackToSave()
          lookAhead
        }))
    }
    reader.readObjectFields[T](path, isSJCapture, fixTypeMemberFields(concreteTypes)) match {
      case null => null.asInstanceOf[T]
      case objectFieldResult: ObjectFieldResult =>
        if (!objectFieldResult.allThere) {
          val fieldArray = fieldMembersByName.values.toArray
          for (p <- 0 to fieldArray.size - 1) {
            if (!objectFieldResult.fieldSet(p)) {
              fieldArray(p).defaultValue.map(default => objectFieldResult.objectArgs(p) = default).orElse(
                if (fieldArray(p).isOptional)
                  None
                else
                  throw new ReadMissingError(path, s"Class $className missing field ${fieldArray(p).name}\n" + reader.showError(), List(className, fieldArray(p).name)))
            }
          }
        }
        val asBuilt = constructorMirror.apply(objectFieldResult.objectArgs: _*).asInstanceOf[T]
        if (isSJCapture)
          asBuilt.asInstanceOf[SJCapture].captured = objectFieldResult.captured.get
        asBuilt
    }
  }

  def write[WIRE](t: T, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = {
    val extras = scala.collection.mutable.ListBuffer.empty[(String, ExtraFieldValue[_])]
    val typeMembersWithRealTypes = typeMembersByName.map {
      case (typeMemberName, tm) =>
        val tType = tm.typeSignature.toString
        val tmWithActualType = fieldMembersByName.values.collectFirst {
          case f if f.declaredValueType.toString == tType =>
            val realValue = f.valueIn(t)
            val realType: Type = runtimeMirror(realValue.getClass.getClassLoader()).classSymbol(realValue.getClass).toType
            tm.copy(runtimeConcreteType = Some(realType))
        }.get // must find one!
        val typeMemberValue = writer.jackFlavor.typeValueModifier match {
          case Some(fn) => fn.unapply(tmWithActualType.runtimeConcreteType.get)
          case None     => tmWithActualType.runtimeConcreteType.get.toString
        }
        extras.append((typeMemberName, ExtraFieldValue(typeMemberValue, writer.jackFlavor.stringTypeAdapter)))
        (tm.typeSignature.toString, tmWithActualType)
    }
    writer.writeObject(t, fixTypeMemberFields(typeMembersWithRealTypes.toMap), out, extras.toList)
  }

  // Used by AnyTypeAdapter to insert type hint (not normally needed) into output so object
  // may be reconsituted on read
  def writeWithHint[WIRE](t: T, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = {
    val hintValue = t.getClass.getName
    val hintLabel = writer.jackFlavor.getHintLabelFor(tt.tpe)
    val extra = List((hintLabel, ClassHelper.ExtraFieldValue(hintValue, writer.jackFlavor.stringTypeAdapter)))
    writer.writeObject(t, fieldMembersByName, out, extra)
  }
}