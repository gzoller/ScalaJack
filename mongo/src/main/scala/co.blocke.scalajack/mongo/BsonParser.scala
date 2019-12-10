package co.blocke.scalajack
package mongo

import model._
import org.bson._
import org.bson.types.ObjectId
import typeadapter.ClassTypeAdapterBase

import scala.collection.mutable
import scala.reflect.runtime.universe.Type
import scala.collection.JavaConverters._

case class BsonParser(input: BsonValue, jackFlavor: JackFlavor[BsonValue])
  extends Parser {
  type WIRE = BsonValue

  def expectString(nullOK: Boolean = true): String =
    if (input == null || input.isNull)
      null
    else if (input.isString)
      input.asString.getValue
    else
      throw new ScalaJackError(s"Expected string here, not '$input'")

  def expectList[K, TO](
      KtypeAdapter: TypeAdapter[K],
      builder:      mutable.Builder[K, TO]): TO =
    if (input == null || input.isNull)
      // $COVERAGE-OFF$Null caught by TypeAdapter but this left here as a safety
      null.asInstanceOf[TO]
    // $COVERAGE-ON$
    else if (input.isArray) {
      input.asArray.getValues.asScala
        .foreach(v => builder += KtypeAdapter.read(BsonParser(v, jackFlavor)))
      builder.result()
    } else
      throw new ScalaJackError(s"Expected list here, not '$input'")

  def expectTuple(
      readFns: List[typeadapter.TupleTypeAdapterFactory.TupleField[_]]
  ): List[Any] =
    if (input == null || input.isNull)
      // $COVERAGE-OFF$Null caught by TypeAdapter but this left here as a safety
      null
    // $COVERAGE-ON$
    else if (input.isArray) {
      val together = readFns.zip(input.asArray.getValues.asScala)
      together.map {
        case (fn, v) => fn.valueTypeAdapter.read(BsonParser(v, jackFlavor))
      }
    } else
      throw new ScalaJackError(s"Expected tuple (list) here, not '$input'")

  def expectMap[K, V, TO](
      keyTypeAdapter:   TypeAdapter[K],
      valueTypeAdapter: TypeAdapter[V],
      builder:          mutable.Builder[(K, V), TO]): TO =
    if (input == null || input.isNull)
      // $COVERAGE-OFF$Null caught by TypeAdapter but this left here as a safety
      null.asInstanceOf[TO]
    // $COVERAGE-ON$
    else if (input.isDocument) {
      input.asDocument.entrySet.asScala.foreach { entry =>
        val mapKey = keyTypeAdapter.read(
          BsonParser(new BsonString(entry.getKey), jackFlavor)
        )
        val mapValue =
          valueTypeAdapter.read(BsonParser(entry.getValue, jackFlavor))
        val newElem: (K, V) = (mapKey, mapValue)
        builder += newElem
      }
      builder.result
    } else
      throw new ScalaJackError(s"Expected document (map) here, not '$input'")

  def expectObject(
      classBase: ClassTypeAdapterBase[_],
      hintLabel: String
  ): (mutable.BitSet, Array[Any], java.util.HashMap[String, _]) =
    if (input == null || input.isNull)
      // $COVERAGE-OFF$Null caught by TypeAdapter but this left here as a safety
      null
    // $COVERAGE-ON$
    else if (input.isDocument) {
      val args = classBase.argsTemplate.clone()
      val fieldBits = classBase.fieldBitsTemplate.clone()
      val captured =
        if (classBase.isSJCapture) new java.util.HashMap[String, BsonValue]()
        else null
      input.asDocument.entrySet.asScala.foreach {
        case entry if entry.getKey == ID_FIELD && classBase.dbKeys.size == 1 =>
          val dbKey = classBase.dbKeys.head
          fieldBits -= dbKey.index
          args(dbKey.index) =
            dbKey.valueTypeAdapter.read(BsonParser(entry.getValue, jackFlavor))
        case entry if entry.getKey == ID_FIELD => // compound key
          entry.getValue.asDocument.entrySet.asScala.foreach { dbKeyEntry =>
            classBase.fieldMembersByName
              .get(dbKeyEntry.getKey)
              .map { field =>
                fieldBits -= field.index
                args(field.index) = field.valueTypeAdapter.read(
                  BsonParser(dbKeyEntry.getValue, jackFlavor)
                )
              }
          }
        case entry =>
          classBase.fieldMembersByName
            .get(entry.getKey)
            .map { field =>
              fieldBits -= field.index
              args(field.index) = field.valueTypeAdapter.read(
                BsonParser(entry.getValue, jackFlavor)
              )
            }
            .getOrElse {
              if (captured != null)
                captured.put(entry.getKey, entry.getValue)
            }
      }
      val missing = fieldBits.intersect(classBase.dbKeys.map(_.index).toSet)
      if (missing.nonEmpty) {
        if (classBase.dbKeys.size == 1)
          throw new ScalaJackError(
            "Missing key (_id) field, or a component of a compound key field"
          )
        else
          throw new ScalaJackError(
            "Missing key (_id) field, or a component of a compound key field: " +
              classBase.dbKeys
              .collect { case f if missing.contains(f.index) => f.name }
              .mkString(",")
          )
      }
      (fieldBits, args, captured)
    } else
      throw new ScalaJackError(s"Expected document (object) here, not '$input'")

  def expectBoolean(): Boolean =
    if (input.isBoolean)
      input.asBoolean.getValue
    else
      throw new ScalaJackError(s"Expected boolean here, not '$input'")

  def expectNumber(): String =
    input match {
      case i if i.isNull       => null
      case i if i.isDecimal128 => i.asDecimal128.getValue.toString
      case i if i.isDouble     => i.asDouble.getValue.toString
      case i if i.isInt32      => i.asInt32.getValue.toString
      case i if i.isInt64      => i.asInt64.getValue.toString
      case i if i.isDateTime   => i.asDateTime.getValue.toString
      case _                   => throw new ScalaJackError(s"Expected number here, not '$input'")
    }

  def peekForNull: Boolean = input == null || input.isNull

  def scanForHint(hint: String, converterFn: HintBijective): Type =
    if (input.isDocument) {
      val doc = input.asDocument
      Option(doc.get(hint)) match {
        case Some(hintValue) if hintValue.isString =>
          val hintType = try {
            converterFn.apply(hintValue.asString.getValue)
          } catch {
            case t: Throwable =>
              throw new ScalaJackError(
                s"Couldn't marshal class for ${hintValue.asString.getValue}"
              )
          }
          hintType
        case Some(hintValue) =>
          throw new ScalaJackError(s"Hint value $hint must be a string value")
        case None => throw new ScalaJackError(s"Type hint '$hint' not found")
      }
    } else
      throw new ScalaJackError(s"Expected document here, not '$input'")

  // For embedded type members.  Convert the type member into runtime "actual" type, e.g. T --> Foo
  def resolveTypeMembers(
      typeMembersByName: Map[String, ClassHelper.TypeMember[_]],
      converterFn:       HintBijective
  ): Map[Type, Type] = // Returns Map[Type Signature Type (e.g. 'T'), Type]
    if (input.isDocument) {
      val doc = input.asDocument
      val collected = doc.keySet.asScala.collect {
        case key if typeMembersByName.contains(key) =>
          (
            typeMembersByName(key).typeSignature,
            converterFn.apply(doc.get(key).asString.getValue)
          )
      }
      collected.toMap
    } else
      throw new ScalaJackError(s"Expected document (object) here, not '$input'")

  def showError(msg: String): String = msg
  // $COVERAGE-OFF$Not used for MongoDB
  def skipOverElement(): Unit = {}
  // $COVERAGE-ON$
  def backspace(): Unit = {}
  def mark(): Int = -1
  def revertToMark(mark: Int): Unit = {}
  def nextIsString: Boolean = input.isString
  def nextIsNumber: Boolean = input.isNumber
  def nextIsObject: Boolean = input.isDocument
  def nextIsArray: Boolean = input.isArray
  def nextIsBoolean: Boolean = input.isBoolean
  def subParser(input: BsonValue): Parser = this
  def sourceAsString: String =
    throw new ScalaJackError(
      s"""BSON type ${input.getClass.getName} is not currently supported in ScalaJack."""
    )

  //--- Mongo Specific ---
  def expectObjectId(): ObjectId =
    if (input == null || input.isNull)
      null
    else if (input.isObjectId)
      input.asObjectId.getValue
    else
      throw new ScalaJackError(s"Expected ObjectId here, not '$input'")

}
