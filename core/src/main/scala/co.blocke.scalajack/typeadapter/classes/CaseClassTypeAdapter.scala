package co.blocke.scalajack
package typeadapter
package classes

import model.ClassHelper.ExtraFieldValue
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

  def read[WIRE](path: Path, reader: Reader[WIRE]): T = {
    val concreteTypes = typeMembersByName.map {
      case (name, tm) =>
        (tm.typeSignature.toString, tm.copy(runtimeConcreteType =
          // 1. Look Ahead for type hint
          // 2. Modify it if needed
          // 3. Marshal it into a Type
          reader.scanForType(path, name, reader.jackFlavor.typeValueModifier).orElse {
            // Consume object as map to basically skip over it to place error pointer correctly and end of object
            reader.skipObject(path)
            reader.back
            throw new ReadMissingError(reader.showError(path, s"Class $className missing type hint for type member ${tm.typeSignature.toString} (looking for $name)"))
          }
        ))
    }
    reader.readObjectFields[T](path, isSJCapture, ClassHelper.applyConcreteTypeMembersToFields(concreteTypes, typeMembersByName, fieldMembersByName)) match {
      case fieldsRead: ObjectFieldsRead if (fieldsRead.allThere) =>
        val asBuilt = constructorMirror.apply(fieldsRead.objectArgs: _*).asInstanceOf[T]
        if (isSJCapture)
          asBuilt.asInstanceOf[SJCapture].captured = fieldsRead.captured
        asBuilt
      case fieldsRead: ObjectFieldsRead => // if missing something...
        val fieldArray = fieldMembersByName.values.toArray
        for (p <- 0 to fieldArray.size - 1)
          if (!fieldsRead.fieldSet(p))
            fieldArray(p).defaultValue.map(default => fieldsRead.objectArgs(p) = default).orElse {
              reader.back
              throw new ReadMissingError(reader.showError(path, s"Class $className missing field ${fieldArray(p).name}"))
            }
        val asBuilt = constructorMirror.apply(fieldsRead.objectArgs: _*).asInstanceOf[T]
        if (isSJCapture)
          asBuilt.asInstanceOf[SJCapture].captured = fieldsRead.captured
        asBuilt
      case null => null.asInstanceOf[T]
    }
  }

  def write[WIRE](t: T, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = {
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
  // may be reconstituted on read
  def writeWithHint[WIRE](t: T, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = {
    val hintValue = t.getClass.getName
    val hintLabel = writer.jackFlavor.getHintLabelFor(tt.tpe)
    val extra = List((hintLabel, ClassHelper.ExtraFieldValue(hintValue, writer.jackFlavor.stringTypeAdapter)))
    writer.writeObject(t, fieldMembersByName, out, extra)
  }
}