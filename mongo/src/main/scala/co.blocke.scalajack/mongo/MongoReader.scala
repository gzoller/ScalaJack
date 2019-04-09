package co.blocke.scalajack
package mongo

import model._
import util.Path
import TokenDetail._
import co.blocke.scalajack.typeadapter.{ AnyTypeAdapter, CanBuildMapTypeAdapter, TupleTypeAdapterFactory }

import org.bson.BsonValue
import org.mongodb.scala.bson._
import java.util.ArrayList

import scala.collection.immutable.{ ListMap, Map }
import scala.collection.mutable.Builder
import scala.util.Try

case class MongoReader(jackFlavor: JackFlavor[BsonValue], bson: BsonValue, tokens: ArrayList[BsonToken], initialPos: Int = 0) extends Reader[BsonValue] {

  private var pos = initialPos

  // For skipping objects
  private lazy val mapAnyTypeAdapter: TypeAdapter[Map[Any, Any]] = jackFlavor.context.typeAdapterOf[Map[Any, Any]]

  // Use this to "save" current state into a copy in case you need to revert
  def copy: Reader[BsonValue] = MongoReader(jackFlavor, bson, tokens, pos)
  def syncPositionTo(reader: Reader[BsonValue]): Unit = this.pos = reader.asInstanceOf[MongoReader].pos // "merge" state with given reader

  /**
   * Nondestructive (doesn't change pointer position) lookahead for a named field (presumes an object)
   * @param label Name of field to search for
   * @return value of the found field
   */
  private def _scanForString(label: String): (Option[String], Int) = {
    var level = 0 // we only care about looking for hints at level 1 (presume first token is '{')
    var p = pos
    var found: Option[String] = None
    //    if (head.tokenType == TokenType.BeginObject)
    //      p += 1 // Skip initial BeginObject if present
    while (p < tokens.size && found.isEmpty) {
      tokens.get(p) match {
        case tok if tok.tokenType == TokenType.String && level == 1 =>
          val labelValue = tok.textValue
          p += 1
          if (labelValue == label) {
            if (tokens.get(p).tokenType == TokenType.String) {
              found = Some(tokens.get(p).textValue)
            } else
              p = tokens.size
          }
        case tok if tok.tokenType == TokenType.BeginObject || tok.tokenType == TokenType.BeginArray =>
          level += 1
        case tok if tok.tokenType == TokenType.EndArray || tok.tokenType == TokenType.EndObject =>
          level -= 1
        case _ =>
      }
      p += 1
    }
    (found, p)
  }

  def scanForHint(hintLabel: String): Option[String] = _scanForString(hintLabel)._1

  def scanForType(path: Path, hintLabel: String, hintModFn: Option[HintValueModifier]): Option[Type] = {
    val (found, p) = _scanForString(hintLabel)
    found match {
      case Some(hintValue) if hintModFn.isDefined =>
        try {
          Some(hintModFn.get.apply(hintValue))
        } catch {
          case _: Throwable =>
            pos = p - 1
            throw new ReadInvalidError(showError(path, s"Failed to apply type modifier to type member hint ${hintValue}"))
        }
      case Some(hintValue) =>
        val savedP = pos
        pos = p - 1
        val result = Some(jackFlavor.typeTypeAdapter.typeNameToType(path, hintValue, this))
        pos = savedP
        result
      case None =>
        None
    }
  }

  // BufferedIterator controls
  def hasNext: Boolean = pos < tokens.size
  def head: ParseToken[BsonValue] = tokens.get(pos)
  def next: ParseToken[BsonValue] = {
    val t = tokens.get(pos)
    pos += 1
    t
  }
  def back: ParseToken[BsonValue] = {
    if (pos > 0)
      pos -= 1
    else
      pos
    tokens.get(pos)
  }
  def reset(): Unit = pos = 0

  // Print a clip from the input and a grapical pointer to the problem for clarity
  def showError(path: Path, msg: String): String =
    "[" + path.toString + "]: " + msg

  @inline private def expect[T](t: TokenType.Value, detail: Option[TokenDetail], path: Path, fn: BsonToken => T, isNullable: Boolean = false): T =
    next.asInstanceOf[BsonToken] match {
      case tok if tok.tokenType == t && (detail.isEmpty || detail.get == tok.detail) =>
        Try(fn(tok)).getOrElse {
          // $COVERAGE-OFF$Means Scala can't handle a number format the Mongo can.  Don't know of any such example, but it is possible, so...
          back
          throw new ReadMalformedError(showError(path, s"Unable to read value (e.g. bad number format)"))
          // $COVERAGE-ON$
        }
      case tok if tok.tokenType == TokenType.Null && isNullable =>
        null.asInstanceOf[T]
      case tok =>
        back
        val withMsg = if (tok.tokenType == t && detail.isDefined) " of kind " + detail.get else ""
        val butWithMsg = if (tok.tokenType == t && detail.isDefined) " of kind " + tok.detail else ""
        throw new ReadUnexpectedError(showError(path, "Expected " + t + s"$withMsg here but found " + tok.tokenType + s"$butWithMsg"), tok.tokenType == TokenType.Null)
    }

  @inline private def assertExists[T](t: TokenType.Value, path: Path): Unit =
    if (head.tokenType == t)
      next
    else
      throw new ReadUnexpectedError(showError(path, s"Expected $t here but found ${head.tokenType}"))

  private def unpackDecimal(bt: BsonToken): BigDecimal = bt match {
    case b if b.input.isDecimal128 => b.input.asDecimal128.getValue.bigDecimalValue
    case b if b.input.isInt32      => scala.math.BigDecimal(b.input.asInt32.getValue)
    case b if b.input.isInt64      => scala.math.BigDecimal(b.input.asInt64.getValue)
    case b if b.input.isDouble     => scala.math.BigDecimal(b.input.asDouble.getValue)
    // $COVERAGE-OFF$Numbers caught first by numeric types, so *shouldn't* be possible--haven't been able to trigger it
    case b if b.input.isNumber     => scala.math.BigDecimal(b.input.asNumber.decimal128Value.bigDecimalValue)
    // $COVERAGE-ON$
  }

  // Read Primitives
  // $COVERAGE-OFF$Can't happen--Mongo has no BigInt data type so nothing can possibly call readBigInt.  Eror left here for future safety
  def readBigInt(path: Path): BigInt = throw new ReadInvalidError(showError(path, "BigInt data type unsupported by MongoDB.  Consider using Long or BigDecimal."))
  // $COVERAGE-ON$
  def readBoolean(path: Path): Boolean = expect(TokenType.Boolean, None, path, (bt: BsonToken) => bt.input.asBoolean.getValue, false)
  def readDecimal(path: Path): BigDecimal = expect(TokenType.Number, None, path, unpackDecimal, true)
  def readDouble(path: Path): Double = expect(TokenType.Number, Some(TokenDetail.Double), path, (bt: BsonToken) => bt.input.asDouble.getValue, false)
  def readInt(path: Path): Int = expect(TokenType.Number, Some(TokenDetail.Int32), path, (bt: BsonToken) => bt.input.asInt32.getValue, false)
  def readLong(path: Path): Long = expect(TokenType.Number, Some(TokenDetail.Int64), path, (bt: BsonToken) => bt.input.asInt64.getValue, false)
  def readString(path: Path): String = expect(TokenType.String, None, path, (bt: BsonToken) => bt.input.asString.getValue, false)

  // Mongo-specific
  def readObjectId(path: Path): ObjectId = expect(TokenType.String, Some(TokenDetail.ObjectId), path, (bt: BsonToken) => bt.input.asObjectId().getValue, true)
  def readDateTime(path: Path): Long = expect(TokenType.Number, Some(TokenDetail.DateTime), path, (bt: BsonToken) => bt.input.asInt64().getValue, false)

  // Read Basic Collections
  def readArray[Elem, To](path: Path, builderFactory: MethodMirror, elementTypeAdapter: TypeAdapter[Elem]): To =
    expect(TokenType.BeginArray, None, path, (_) => "", true) match {
      case "" =>
        val builder = builderFactory().asInstanceOf[Builder[Elem, To]]
        var i = 0
        while (head.tokenType != TokenType.EndArray) {
          builder += elementTypeAdapter.read(path \ i, this)
          i += 1
        }
        next // consume the end array token
        builder.result
      case null => null.asInstanceOf[To]
    }

  def readMap[Key, Value, To](path: Path, builderFactory: MethodMirror, keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value]): To =
    expect(TokenType.BeginObject, None, path, (_) => "", true) match {
      case "" =>
        val builder = builderFactory().asInstanceOf[Builder[(Key, Value), To]]
        while (head.tokenType != TokenType.EndObject) {
          keyTypeAdapter.read(path \ Path.MapKey, this) match {
            // $COVERAGE-OFF$Should be possible to call this
            case null =>
              throw new ReadInvalidError(showError(path, "Map keys cannot be null"))
            // $COVERAGE-ON$
            case key =>
              builder += key -> valueTypeAdapter.read(path \ key.toString, this)
          }
        }
        next // consume EndObject
        builder.result
      case null =>
        null.asInstanceOf[To]
    }

  def readTuple(path: Path, readFns: List[TupleTypeAdapterFactory.TupleField[_]]): List[Any] =
    expect(TokenType.BeginArray, None, path, (_) => "", true) match {
      case "" =>
        var fnPos = -1
        val tup = readFns.map { fn =>
          fnPos += 1
          fn.read(path \ fnPos, this)
        }
        assertExists(TokenType.EndArray, path)
        tup
    }

  // Read fields we know to be object fields
  def readObjectFields[T](path: Path, isSJCapture: Boolean, fields: ListMap[String, ClassHelper.ClassFieldMember[T, Any]]): ObjectFieldsRead = //(Boolean, Array[Any], Array[Boolean])
    expect(TokenType.BeginObject, None, path, (_) => "", true) match {
      case "" =>
        var fieldCount = 0
        var captured = Map.empty[String, Any] // a place to cache SJCapture'd fields
        val args = new Array[Any](fields.size)
        val flags = new Array[Boolean](fields.size)
        val dbkeys = fields.filter(_._2.dbKeyIndex.isDefined)
        val ID_FIELD = jackFlavor.asInstanceOf[MongoFlavor].ID_FIELD

        def readOneField(fieldName: String): Unit =
          fields.get(fieldName) match {
            case Some(oneField) =>
              args(oneField.index) = oneField.valueTypeAdapter.read(path \ fieldName, this)
              flags(oneField.index) = true
              fieldCount += 1
            case _ if isSJCapture =>
              captured = captured.+((fieldName, jackFlavor.anyTypeAdapter.asInstanceOf[AnyTypeAdapter]._read(path \ fieldName, this, true)))
            case _ =>
              // Skip over field not in class if we're not capturing
              jackFlavor.anyTypeAdapter.read(path \ fieldName, this)
          }

        var foundID = false
        while (head.tokenType != TokenType.EndObject) {
          val fieldName = expect(TokenType.String, None, path, (bt: BsonToken) => bt.textValue, false)
          fieldName match {
            case ID_FIELD if dbkeys.size == 1 =>
              readOneField(dbkeys.head._2.name)
              foundID = true
            case ID_FIELD if dbkeys.size > 1 => // read embedded _id object
              expect(TokenType.BeginObject, None, path, (_) => "", true) match {
                case "" =>
                  while (head.tokenType != TokenType.EndObject)
                    readOneField(expect(TokenType.String, None, path, (bt: BsonToken) => bt.textValue, false))
                  next // consume EndObject
                  foundID = true
              }
            case _ => readOneField(fieldName)
          }
        }
        if (!foundID && dbkeys.size > 0)
          throw new ReadMissingError(showError(path, "Missing key field " + ID_FIELD))
        next // consume EndObject
        ObjectFieldsRead(fieldCount == fields.size, args, flags, captured)
      case null =>
        null
    }

  def skipObject(path: Path): Unit =
    if (head.tokenType == TokenType.BeginObject)
      readMap[String, Any, Map[String, Any]](path, mapAnyTypeAdapter.asInstanceOf[CanBuildMapTypeAdapter[Any, Any, Map[Any, Any]]].builderFactory, jackFlavor.stringTypeAdapter, jackFlavor.anyTypeAdapter)
}