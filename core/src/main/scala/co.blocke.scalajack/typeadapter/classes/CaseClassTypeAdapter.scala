package co.blocke.scalajack
package typeadapter
package classes

import util.Path
import model._

import scala.collection.immutable.{ ListMap, Map }
import scala.collection.mutable.Builder
import scala.reflect.runtime.universe._ //{ MethodMirror, Type, TypeTag }

case class CaseClassTypeAdapter[T](
    className:          String,
    typeMembersByName:  Map[String, ClassHelper.TypeMember[T]],
    fieldMembersByName: ListMap[String, ClassHelper.ClassFieldMember[T, Any]],
    typeTypeAdapter:    TypeAdapter[Type],
    constructorMirror:  MethodMirror,
    isSJCapture:        Boolean,
    collectionName:     Option[String]                                        = None)(implicit context: Context, tt: TypeTag[T]) extends ClassHelper.ClassLikeTypeAdapter[T] {

  // Hook for subclasses (e.g. Mongo) do to anything needed to handle the db key field(s) as given by the @DBKey annotation
  protected def handleDBKeys[AST](fieldValues: Map[String, Any]): Map[String, Any] = fieldValues

  // If the given field f is involved with type member parameter T then create a new wrapped type over the now-known concrete type for T.
  // For example if the f is Option[T] and we know T is Foo, then create Type Option[Foo] and update f with the new type and
  // appropriate TypeAdapter for Option[Foo].
  private def wrap1Type[T](f: ClassHelper.ClassFieldMember[T,Any], typeHit: Option[ClassHelper.TypeMember[Nothing]]): ClassHelper.ClassFieldMember[T,Any] =
    typeHit match {
      case Some(hit) =>
        val mirror = runtimeMirror(ClassLoader.getSystemClassLoader)
        // from Scala tutorial...not recommended due to use of internals, but...
        val optionType = mirror.universe.internal.typeRef(
          definitions.PredefModule.typeSignature,
          f.valueType.typeSymbol,
          List(hit.runtimeConcreteType.get)
        )
        f.copy(
          declaredValueType = optionType,
          valueTypeAdapter = context.typeAdapter(optionType),
        ).asInstanceOf[ClassHelper.ClassFieldMember[T, Any]]
      case None =>
        f
    }

  // OK, so all this hokem is to figure out what to do for embedded type member (i.e. externalized type hint feature).  Doesn't seem to be needed
  // for anything else.
  private def inferConcreteCaseClassTypeAdapter[WIRE](path: Path, reader: Transceiver[WIRE]): ListMap[String, ClassHelper.ClassFieldMember[T, Any]] = {
    if (typeMembersByName.isEmpty) {
      fieldMembersByName
    } else {
      // If type members are defined --> externalized trait concrete type
      // Create a mapping of type label, e.g. 'T', to TypeMember where we've resolved the type member's value into a Type
      val concrete = typeMembersByName.map { case (name, tm) => (tm.typeSignature.toString, tm.copy(runtimeConcreteType = reader.lookAheadForTypeHint(name, (s: String) => typeTypeAdapter.read(path, reader)))) }

      // Now buzz through known field members and replaces all the 'T' type with the concrete type and insert the correct concrete TypeAdapter.
      fieldMembersByName.map {
        case (name, field) =>
          val findDirect = concrete.get(field.declaredValueType.toString) match {
            case Some(c) =>
              field.copy(
                valueTypeAdapter  = context.typeAdapter(c.runtimeConcreteType.get),
                declaredValueType = c.runtimeConcreteType.get
              ).asInstanceOf[ClassHelper.ClassFieldMember[T, Any]]
            case None =>
              field
          }

          // Also handle any Try[T], Other[T], Either[T,T], CanBuildFrom[T]
          val findConcrete = field.valueTypeAdapter.asInstanceOf[TypeAdapter[_]] match {
            case _: OptionTypeAdapter[_] =>
              wrap1Type(field, concrete.get(field.declaredValueType.typeArgs(0).toString))
            case _: CanBuildFromTypeAdapter[_,_] =>
              wrap1Type(field, concrete.get(field.declaredValueType.typeArgs(0).toString))
            case _: TryTypeAdapter[_] =>
              wrap1Type(field, concrete.get(field.declaredValueType.typeArgs(0).toString))
            case _ =>
              findDirect
          }
          (name, findConcrete) //.map(actual => field.copy(declaredValueType = actual.runtimeConcreteType.get)).getOrElse(field))
      }
    }
  }

  def read[WIRE](path: Path, reader: Transceiver[WIRE]): T = {
    val fixed = inferConcreteCaseClassTypeAdapter(path, reader)
    reader.readObjectFields[T](path, isSJCapture, fixed) match {
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

  def write[WIRE](t: T, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit =
    writer.writeObject(t, fieldMembersByName, out)

  // Used by AnyTypeAdapter to insert type hint (not normally needed) into output so object
  // may be reconsituted on read
  def writeWithHint[WIRE](t: T, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = {
    val hintValue = t.getClass.getName
    val hintLabel = writer.jackFlavor.getHintLabelFor(tt.tpe)
    val extra = List((hintLabel, ClassHelper.ExtraFieldValue(hintValue, writer.jackFlavor.stringTypeAdapter)))
    writer.writeObject(t, fieldMembersByName, out, extra)
  }
}