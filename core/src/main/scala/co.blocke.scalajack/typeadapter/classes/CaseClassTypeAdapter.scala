package co.blocke.scalajack
package typeadapter
package classes

import co.blocke.scalajack.model.ClassHelper.ExtraFieldValue
import util.Path
import model._

import scala.collection.immutable.{ ListMap, Map }
import scala.collection.mutable.Builder
import scala.reflect.runtime.universe._

case class CaseClassTypeAdapter[T](
    className:          String,
    typeMembersByName:  Map[String, ClassHelper.TypeMember[T]],
    fieldMembersByName: ListMap[String, ClassHelper.ClassFieldMember[T, Any]],
    constructorMirror:  MethodMirror,
    isSJCapture:        Boolean,
    collectionName:     Option[String])(implicit context: Context, tt: TypeTag[T]) extends ClassHelper.ClassLikeTypeAdapter[T] {

  // Hook for subclasses (e.g. Mongo) do to anything needed to handle the db key field(s) as given by the @DBKey annotation
  protected def handleDBKeys(fieldValues: Map[String, Any]): Map[String, Any] = fieldValues

  def read[WIRE](path: Path, reader: Transceiver[WIRE]): T = {
    val concreteTypes = typeMembersByName.map {
      case (name, tm) =>
        (tm.typeSignature.toString, tm.copy(runtimeConcreteType = {
          val lookAhead = reader.lookAheadForTypeHint(path, className, name, (s: String) => {
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
    reader.readObjectFields[T](path, isSJCapture, ClassHelper.applyConcreteTypeMembersToFields(concreteTypes, typeMembersByName, fieldMembersByName)) match {
      case null => null.asInstanceOf[T]
      case objectFieldResult: ObjectFieldResult =>
        if (!objectFieldResult.allThere) {
          val fieldArray = fieldMembersByName.values.toArray
          for (p <- 0 to fieldArray.size - 1) {
            if (!objectFieldResult.fieldSet(p)) {
              fieldArray(p).defaultValue.map(default => objectFieldResult.objectArgs(p) = default).orElse(
                throw new ReadMissingError(path, s"Class $className missing field ${fieldArray(p).name}\n" + reader.showError(-1), List(className, fieldArray(p).name))
              )
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
        fieldMembersByName.values.collectFirst {
          case f if f.declaredValueType.toString == tType =>
            // If the T in the type member is a trait, print the trait name for the type member and allow the values
            // (e.g. payload) to go ahead and print concrete type hint as usual.  (Allows filtering on the trait rather than value's type.)
            // i.e. The rendered output will have 2 hints: an externalized one (the trait) and the internal one (the concrete value type).
            //
            // If the T is a concrete type then only render the externalized type and bypass the internal one.
            if (tm.baseType.typeSymbol.asClass.isTrait) {
              tm.copy(runtimeConcreteType = Some(tm.baseType))
            } else {
              val realValue = f.valueIn(t)
              val realType: Type = runtimeMirror(realValue.getClass.getClassLoader()).classSymbol(realValue.getClass).toType
              tm.copy(runtimeConcreteType = Some(realType))
            }
        } match {
          case Some(tmWithActualType) =>
            val rawTypeValueBeforeMod = tmWithActualType.runtimeConcreteType.get
            val typeMemberValue = writer.jackFlavor.typeValueModifier match {
              case Some(fn) => fn.unapply(rawTypeValueBeforeMod)
              case None     => rawTypeValueBeforeMod.toString
            }

            extras.append((typeMemberName, ExtraFieldValue(typeMemberValue, writer.jackFlavor.stringTypeAdapter)))
            (tm.typeSignature.toString, tmWithActualType)
          case None =>
            // Internal type member usage--uninteresting from a construction perspective, so skip it (nulls to be filtered below)
            (tm.typeSignature.toString, null)
        }
    }.filter(_._2 != null)
    writer.writeObject(t, ClassHelper.applyConcreteTypeMembersToFields(typeMembersWithRealTypes, typeMembersByName, fieldMembersByName), out, extras.toList)
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