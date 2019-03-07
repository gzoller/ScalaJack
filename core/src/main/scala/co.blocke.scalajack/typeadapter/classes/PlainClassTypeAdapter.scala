package co.blocke.scalajack
package typeadapter
package classes

import model._
import util.Path
import ClassHelper._

import scala.collection.immutable.{ListMap, Map}
import scala.collection.mutable.Builder
import scala.reflect.runtime.universe._

case class PlainClassTypeAdapter[T](
  className: String,
  typeMembersByName: Map[String, ClassHelper.TypeMember[T]],
  fieldMembersByName: ListMap[String, ClassHelper.ClassFieldMember[T, Any]],
  nonConstructorFields: ListMap[String, ClassHelper.ClassFieldMember[T, Any]],
  constructorMirror: MethodMirror,
  isSJCapture: Boolean,
  collectionName: Option[String],
  isScala: Boolean,
  )(implicit context: Context, tt: TypeTag[T]) extends ClassLikeTypeAdapter[T] {

  override def dbKeys: List[ClassFieldMember[T, Any]] =
    (fieldMembersByName.values.toList ++ nonConstructorFields.values.toList).filter(_.dbKeyIndex.isDefined).sortBy(_.dbKeyIndex.get)

  def read[WIRE](path: Path, reader: Transceiver[WIRE]): T = {

    // Any externalized trait hints? (as type members)
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

    // Apply any concrete type member definitions to placeholder types 'T'->MyThing
    val both = ClassHelper.applyConcreteTypeMembersToFields(concreteTypes, typeMembersByName, fieldMembersByName ++ nonConstructorFields)

    reader.readObjectFields[T](path, isSJCapture, both) match {
      case null => null.asInstanceOf[T]
      case objectFieldResult: ObjectFieldResult =>

        val finalConstructorArgList =
          if (!objectFieldResult.allThere) {
            val fieldArray = both.values.toArray
            val justConstructorArgs = objectFieldResult.objectArgs.take(fieldMembersByName.size)
            val justConstructorIsSet = objectFieldResult.fieldSet.take(fieldMembersByName.size)

            justConstructorArgs.zip(justConstructorIsSet).zipWithIndex.map{
              // Missing with default value
              case ((_, false), index) if fieldArray(index).defaultValue.isDefined =>
                fieldArray(index).defaultValue.get
              // Missing but optional
              case ((_, false), index) if fieldArray(index).isOptional =>
                None
              // Any other missing
              case ((_, false), index) =>
                throw new ReadMissingError(path, s"Class $className missing field ${fieldArray(index).name}\n" + reader.showError(), List(className, fieldArray(index).name))
              // Anything else... as-read
              case ((result, true), index) =>
                result
            }
          } else
            objectFieldResult.objectArgs.take(fieldMembersByName.size)

        // Call constructor with constructor args (may be 0)
        val asBuilt = constructorMirror.apply(finalConstructorArgList: _*).asInstanceOf[T]

        // Populate non-constructor fields (getters/setters, vars)
        val fieldsAndArgs =
          if (!objectFieldResult.allThere) {
            val nonConstructorArgs = objectFieldResult.objectArgs.takeRight(nonConstructorFields.size)
            val nonConstructorIsSet = objectFieldResult.fieldSet.takeRight(nonConstructorFields.size)

            // Exclude any @Maybe fields and remove corresponding field elements from list
            nonConstructorFields.values.zip(nonConstructorArgs.zip(nonConstructorIsSet)).collect{
              // Missing but optional
              case (field, (_, false)) if field.isOptional && !field.isMaybe =>
                (field, None)
              // Any other missing (but not @Maybe)
              case (field, (_, false)) if !field.isMaybe =>
                throw new ReadMissingError(path, s"Class $className missing field ${field.name}\n" + reader.showError(), List(className, field.name))
              // Anything else... as-read
              case (field, (arg, true)) =>
                (field, arg)
            }.toList
          } else
            // Simple mapping--everything expected was found
            nonConstructorFields.values.zip(objectFieldResult.objectArgs.takeRight(nonConstructorFields.size)).toList

        fieldsAndArgs.foreach{ case(field, aValue) =>
            field.valueSet(asBuilt, aValue)(tt, typeToClassTag[T])
        }

        if (isSJCapture)
          asBuilt.asInstanceOf[SJCapture].captured = objectFieldResult.captured.get

        asBuilt
    }
  }

  // This is a carbon-copy of CaseClassTypeAdapter.write *EXCEPT* we need to also write out the nonConstructorFields too.
  def write[WIRE](t: T, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = {
    val extras = scala.collection.mutable.ListBuffer.empty[(String, ExtraFieldValue[_])]
    val typeMembersWithRealTypes = typeMembersByName.map {
      case (typeMemberName, tm) =>
        val tType = tm.typeSignature.toString
        (fieldMembersByName ++ nonConstructorFields).values.collectFirst {
          case f if f.declaredValueType.toString == tType =>
            val realValue = f.valueIn(t)
            val realType: Type = runtimeMirror(realValue.getClass.getClassLoader()).classSymbol(realValue.getClass).toType
            tm.copy(runtimeConcreteType = Some(realType))
        } match {
          case Some(tmWithActualType) =>
            val typeMemberValue = writer.jackFlavor.typeValueModifier match {
              case Some(fn) => fn.unapply(tmWithActualType.runtimeConcreteType.get)
              case None     => tmWithActualType.runtimeConcreteType.get.toString
            }
            extras.append((typeMemberName, ExtraFieldValue(typeMemberValue, writer.jackFlavor.stringTypeAdapter)))
            (tm.typeSignature.toString, tmWithActualType)
          case None =>
            (tm.typeSignature.toString, null)
        }
    }.filter(_._2 != null)
    writer.writeObject(t, ClassHelper.applyConcreteTypeMembersToFields(typeMembersWithRealTypes, typeMembersByName, fieldMembersByName ++ nonConstructorFields), out, extras.toList)
  }
}