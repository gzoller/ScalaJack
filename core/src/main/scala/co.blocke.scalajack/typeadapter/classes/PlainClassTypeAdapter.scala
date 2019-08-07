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
)(implicit context: Context, tt: TypeTag[T])
    extends ClassLikeTypeAdapter[T]
    with Classish {

  override def dbKeys: List[ClassFieldMember[T, Any]] =
    (fieldMembersByName.values.toList ++ nonConstructorFields.values.toList).filter(_.dbKeyIndex.isDefined).sortBy(_.dbKeyIndex.get)

  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): T = {

    // Any externalized trait hints? (as type members)
    val concreteTypes = typeMembersByName.map {
      case (name, tm) =>
        (tm.typeSignature.toString, tm.copy(runtimeConcreteType = {
          // 1. Look Ahead for type hint
          // 2. Modify it if needed
          // 3. Marshal it into a Type
          reader.scanForType(path, name, reader.jackFlavor.typeValueModifier).orElse {
            // Consume object as map to basically skip over it to place error pointer correctly and end of object
            reader.skipObject(path)
            reader.back
            throw new ReadMissingError(
              reader.showError(path, s"Class $className missing type hint for type member ${tm.typeSignature.toString} (looking for $name)"))
          }
        }))
    }

    // Apply any concrete type member definitions to placeholder types 'T'->MyThing
    val both = ClassHelper.applyConcreteTypeMembersToFields(concreteTypes, typeMembersByName, fieldMembersByName ++ nonConstructorFields)

    reader.readObjectFields[T](path, isSJCapture, both) match {
      case null => null.asInstanceOf[T]
      case objectFieldResult: ObjectFieldsRead =>
        val finalConstructorArgList =
          if (!objectFieldResult.allThere) {
            val fieldArray           = both.values.toArray
            val justConstructorArgs  = objectFieldResult.objectArgs.take(fieldMembersByName.size)
            val justConstructorIsSet = objectFieldResult.fieldSet.take(fieldMembersByName.size)

            justConstructorArgs.zip(justConstructorIsSet).zipWithIndex.map {
              // Missing with default value
              case ((_, false), index) if fieldArray(index).defaultValue.isDefined =>
                fieldArray(index).defaultValue.get
              // Missing but optional
              case ((_, false), index) if fieldArray(index).isOptional =>
                None
              // Any other missing
              case ((_, false), index) =>
                reader.back
                throw new ReadMissingError(reader.showError(path, s"Class $className missing field ${fieldArray(index).name}"))
              // Anything else... as-read
              case ((result, true), index) =>
                result
            }
          } else
            objectFieldResult.objectArgs.take(fieldMembersByName.size)

        // Call constructor with constructor args (may be 0)
        val asBuilt = constructorMirror.apply(finalConstructorArgList.toIndexedSeq: _*).asInstanceOf[T]

        // Populate non-constructor fields (getters/setters, vars)
        val fieldsAndArgs =
          if (!objectFieldResult.allThere) {
            val nonConstructorArgs  = objectFieldResult.objectArgs.takeRight(nonConstructorFields.size)
            val nonConstructorIsSet = objectFieldResult.fieldSet.takeRight(nonConstructorFields.size)

            // Exclude any @Optional fields and remove corresponding field elements from list
            nonConstructorFields.values
              .zip(nonConstructorArgs.zip(nonConstructorIsSet))
              .collect {
                // Missing but optional
                case (field, (_, false)) if field.isOptional && !field.hasOptionalAnnotation =>
                  (field, None)
                // Any other missing (but not @Optional)
                case (field, (_, false)) if !field.hasOptionalAnnotation =>
                  reader.back
                  throw new ReadMissingError(reader.showError(path, s"Class $className missing field ${field.name}"))
                // Anything else... as-read
                case (field, (arg, true)) =>
                  (field, arg)
              }
              .toList
          } else
            // Simple mapping--everything expected was found
            nonConstructorFields.values.zip(objectFieldResult.objectArgs.takeRight(nonConstructorFields.size)).toList

        fieldsAndArgs.foreach {
          case (field, aValue) =>
            field.valueSet(asBuilt, aValue)(tt, typeToClassTag[T])
        }

        if (isSJCapture)
          asBuilt.asInstanceOf[SJCapture].captured = objectFieldResult.captured

        asBuilt
    }
  }

  // This is a carbon-copy of CaseClassTypeAdapter.write *EXCEPT* we need to also write out the nonConstructorFields too.
  def write[WIRE](t: T, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = {
    val extras = scala.collection.mutable.ListBuffer.empty[(String, ExtraFieldValue[_])]
    val typeMembersWithRealTypes = typeMembersByName
      .map {
        case (typeMemberName, tm) =>
          val tType = tm.typeSignature.toString
          (fieldMembersByName ++ nonConstructorFields).values.collectFirst {
            case f if f.declaredValueType.toString == tType =>
              val realValue      = f.valueIn(t)
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
      }
      .filter(_._2 != null)
    writer.writeObject(
      t,
      ClassHelper.applyConcreteTypeMembersToFields(typeMembersWithRealTypes, typeMembersByName, fieldMembersByName ++ nonConstructorFields),
      out,
      extras.toList)
  }
}
