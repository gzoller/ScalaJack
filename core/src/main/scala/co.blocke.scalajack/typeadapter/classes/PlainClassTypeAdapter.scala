package co.blocke.scalajack
package typeadapter
package classes

import model._
import util.{Path, Reflection}
import java.beans.Introspector
import java.lang.reflect.Method

import ClassHelper._

import scala.collection.immutable.{List, ListMap}
import scala.collection.mutable.Builder
import scala.language.existentials
import scala.reflect.ClassTag
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}

/*
caseclasstypeadapter(
    className:          String,
    typeMembersByName:  Map[String, ClassHelper.TypeMember[T]],
    fieldMembersByName: ListMap[String, ClassHelper.CaseClassFieldMember[T, Any]],
    constructorMirror:  MethodMirror,
    isSJCapture:        Boolean,
    collectionName:     Option[String])(implicit context: Context, tt: TypeTag[T]) extends ClassHelper.ClassLikeTypeAdapter[T] { */

case class PlainClassTypeAdapter[T](
  className: String,
  typeMembersByName: Map[String, ClassHelper.TypeMember[T]],
  fieldMembersByName: ListMap[String, ClassHelper.ClassFieldMember[T, Any]],
  collectionName: Option[String],
  isSJCapture: Boolean,
  constructorMirror: MethodMirror) extends ClassLikeTypeAdapter[T] {

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

    val asBuilt = constructorMirror.apply().asInstanceOf[T] // call 0-parameter constructor


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
    null.asInstanceOf[T]
  }

  def write[WIRE](t: T, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = {
  }
}