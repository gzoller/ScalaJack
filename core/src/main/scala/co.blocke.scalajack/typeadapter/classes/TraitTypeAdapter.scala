package co.blocke.scalajack
package typeadapter
package classes

import model._
import util._

import scala.collection.mutable
import scala.collection.mutable.Builder
import scala.reflect.runtime.currentMirror

// This should come *after* SealedTraitTypeAdapter in the Context factory list, as all sealed traits are
// also traits, and this factory would pick them all up, hiding the sealed ones.
//
object TraitTypeAdapterFactory extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (classSymbol.isTrait)
      TraitTypeAdapter(classSymbol.fullName, tt.tpe)
    else
      next.typeAdapterOf[T]
}

case class TraitTypeAdapter[T](
    traitName:       String,
    polymorphicType: Type)(implicit tt: TypeTag[T], context: Context) extends TypeAdapter[T] {

  private val populatedConcreteTypeCache = new mutable.WeakHashMap[Type, Type]
  private var hintLabel: Option[String] = None

  private def populateConcreteType(concreteType: Type): Type =
    populatedConcreteTypeCache.getOrElseUpdate(concreteType, Reflection.populateChildTypeArgs(tt.tpe, concreteType))

  private def getHintLabel[WIRE](xceiver: Transceiver[WIRE]): String = hintLabel.getOrElse {
    hintLabel = Some(xceiver.jackFlavor.getHintLabelFor(tt.tpe))
    hintLabel.get
  }

  // The battle plan here is:  Scan the keys of the object looking for type typeHintField.  Perform any (optional)
  // re-working of the hint value via hintModFn.  Look up the correct concete TypeAdapter based on the now-known type
  // and re-read the object as a case class.
  def read[WIRE](path: Path, reader: Transceiver[WIRE]): T = {
    val hintModFn = reader.jackFlavor.hintValueModifiers.get(tt.tpe)
    val hintLabel = getHintLabel(reader) // Apply any hint label modifiers
    reader.peek() match {
      case TokenType.Null =>
        reader.skip()
        null.asInstanceOf[T]
      case TokenType.BeginObject =>
        val concreteType =
          reader.lookAheadForTypeHint(hintLabel, (hintString: String) =>
            hintModFn.map(th => th.apply(hintString))
              .getOrElse(reader.jackFlavor.typeTypeAdapter.read(path, reader))
          ).getOrElse(throw new ReadInvalidError(path \ hintLabel, s"Couldn't materialize class for trait $traitName hint $hintLabel\n" + reader.showError()))
        val populatedConcreteType = populateConcreteType(concreteType)
        context.typeAdapter(populatedConcreteType).read(path, reader).asInstanceOf[T]
      case t =>
        throw new ReadUnexpectedError(path, "Expected start of an object but read token " + t, List.empty[String])
    }
  }

  def write[WIRE](t: T, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = {
    val hintModFn = writer.jackFlavor.hintValueModifiers.get(tt.tpe)
    if (t == null)
      writer.writeNull(out)
    else {
      val concreteType = currentMirror.classSymbol(t.getClass).toType
      val populatedConcreteType = populateConcreteType(concreteType)
      context.typeAdapter(populatedConcreteType).asInstanceOf[TypeAdapter[T]] match {
        case cc: CaseClassTypeAdapter[T] =>
          val hintValue = hintModFn.map(_.unapply(populatedConcreteType)).getOrElse(t.getClass.getName)
          writer.writeObject(t, cc.fieldMembersByName, out, List((getHintLabel(writer), ClassHelper.ExtraFieldValue(hintValue, writer.jackFlavor.stringTypeAdapter))))
      }
    }
  }

}
