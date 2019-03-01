package co.blocke.scalajack
package typeadapter
package classes

import model._
import util.Path
import ClassHelper._

import scala.collection.immutable.{ListMap, Map}
import scala.collection.mutable.Builder
import scala.language.existentials
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

//  val wrappedCaseClassTypeAdapter = CaseClassTypeAdapter[T](
//    className,
//    typeMembersByName,
//    fieldMembersByName,
//    constructorMirror,
//    isSJCapture,
//    collectionName)

  def read[WIRE](path: Path, reader: Transceiver[WIRE]): T = {

    println("------------------------------")

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

        println("Fields: "+both.map(_._1))
        println("Args: "+objectFieldResult.objectArgs.toList)
        println("Set: "+objectFieldResult.fieldSet.toList)

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
              case ((field, (_, false))) if field.isOptional =>
                (field, None)
              // Any other missing (but not @Maybe)
              case ((field, (_, false))) if !field.isMaybe =>
                throw new ReadMissingError(path, s"Class $className missing field ${field.name}\n" + reader.showError(), List(className, field.name))
              // Anything else... as-read
              case ((field, (arg, true))) =>
                (field, arg)
            }.toList
          } else
            // Simple mapping--everything expected was found
            nonConstructorFields.values.zip(objectFieldResult.objectArgs.takeRight(nonConstructorFields.size)).toList

        fieldsAndArgs.foreach{ case(field, aValue) =>
          println("Set "+field.name.toString+" to value "+aValue)
            field.valueSet(asBuilt, aValue)(tt, typeToClassTag[T])
        }

        if (isSJCapture)
          asBuilt.asInstanceOf[SJCapture].captured = objectFieldResult.captured.get

        asBuilt
    }
  }

  def write[WIRE](t: T, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = {
  }
}