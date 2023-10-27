package co.blocke.scalajack
package json

import scala.util.*
import co.blocke.scala_reflection.TypedName

case class JsonParser(js: String, cache: Map[TypedName, (JsonConfig, JsonParser) => Either[ParseError, ?]]):

  private val jsChars: Array[Char] = js.toCharArray
  private var i = 0
  private val max: Int = jsChars.length

  def getPos = i

  // Housekeeping
  // ------------------------------
  @inline def nullCheck: Boolean =
    val res = i + 3 < max && jsChars(i) == 'n' && jsChars(i + 1) == 'u' && jsChars(i + 2) == 'l' && jsChars(i + 3) == 'l'
    if res then i += 4
    res
  @inline def eatWhitespace: Either[ParseError, Unit] =
    while i < max && jsChars(i).isWhitespace do i += 1
    Right(())
  @inline def expectQuote: Either[ParseError, Unit] =
    if jsChars(i) == '\"' then
      i += 1
      Right(())
    else Left(ParseError(s"Quote expected at position [$i]"))
  @inline def expectComma: Either[ParseError, Unit] = // Note: this consumes whitespace before/after the ','
    for {
      _ <- eatWhitespace
      r <-
        if jsChars(i) == ',' then
          i += 1
          eatWhitespace
          Right(())
        else Left(CommaExpected())
    } yield r
  @inline def expectColon: Either[ParseError, Unit] = // Note: this consumes whitespace before/after the ':'
    for {
      _ <- eatWhitespace
      r <-
        if jsChars(i) == ':' then
          i += 1
          eatWhitespace
          Right(())
        else Left(JsonParseError(s"Expected colon at position [$i]"))
    } yield r

  // JSON label (i.e. object key, which is always a simple string, ie no escaped/special chars)
  def expectLabel: Either[ParseError, String] =
    jsChars(i) match
      case '"' =>
        i += 1
        val mark = i
        while i < max && jsChars(i) != '"' do i += 1
        i += 1
        Right(js.substring(mark, i - 1))
      case x => Left(JsonParseError(s"Unexpected character '$x' where beginning of label expected at position [$i]"))

  // Data Types
  // ------------------------------
  def expectBoolean(cfg: JsonConfig, p: JsonParser): Either[ParseError, Boolean] =
    jsChars(i) match
      case 't' if i + 3 < max && jsChars(i + 1) == 'r' && jsChars(i + 2) == 'u' && jsChars(i + 3) == 'e' =>
        i += 4
        Right(true)
      case 'f' if i + 4 < max && jsChars(i + 1) == 'a' && jsChars(i + 2) == 'l' && jsChars(i + 3) == 's' && jsChars(i + 4) == 'e' =>
        i += 5
        Right(false)
      case x => Left(JsonParseError(s"Unexpected character '$x' where beginning of boolean value expected at position [$i]"))

  def expectLong(cfg: JsonConfig, p: JsonParser): Either[ParseError, Long] =
    val mark = i
    var done = false
    while !done do
      jsChars(i) match
        case c if (c >= '0' && c <= '9') || c == '-' => i += 1
        case _                                       => done = true
    Try(js.substring(mark, i).toLong) match
      case Success(g) => Right(g)
      case Failure(f) =>
        val msg =
          if mark == i then s"""Int/Long expected but couldn't parse from "${jsChars(i)}" at position [$i]"""
          else s"""Int/Long expected but couldn't parse from "${js.substring(mark, i)}" at position [$i]"""
        i = mark
        Left(JsonParseError(msg))

  def expectDouble(cfg: JsonConfig, p: JsonParser): Either[ParseError, Double] =
    val mark = i
    var done = false
    while !done do
      jsChars(i) match
        case c if (c >= '0' && c <= '9') || c == '-' || c == '.' || c == 'e' || c == 'E' || c == '+' => i += 1
        case _                                                                                       => done = true
    Try(js.substring(mark, i - 1).toDouble) match
      case Success(g) => Right(g)
      case Failure(_) =>
        val msg =
          if mark == i then s"Float/Double expected but couldn't parse from \"${jsChars(i)}\" at position [$i]"
          else s"Float/Double expected but couldn't parse from \"${js.substring(mark, i - 1)}\" at position [$i]"
        i = mark
        Left(JsonParseError(msg))

  def expectString(cfg: JsonConfig, p: JsonParser): Either[ParseError, String] =
    nullCheck match
      case true if cfg.forbidNullsInInput => Left(JsonParseError(s"Forbidden 'null' value received at position [$i]"))
      case true                           => Right(null.asInstanceOf[String])
      case false =>
        jsChars(i) match
          case '"' =>
            i += 1
            val mark = i // save position in case we need complex string parse
            var captured: Option[String] = None
            while i < max && jsChars(i) != '"' do
              jsChars(i) match
                case '\\' => // oops!  special char found--do slow string parse
                  i = mark
                  captured = Some(_expectString)
                case _ => i += 1
            i += 1
            Right(captured.getOrElse(js.substring(mark, i - 1)))
          case x => Left(JsonParseError(s"Unexpected character '$x' where beginning of label expected at position [$i]"))

  def expectList[T](cfg: JsonConfig, expectElement: (JsonConfig, JsonParser) => Either[ParseError, T]): Either[ParseError, List[T]] =
    if jsChars(i) != '[' then Left(JsonParseError(s"Beginning of list expected at position [$i]"))
    else
      i += 1
      eatWhitespace
      val acc = scala.collection.mutable.ListBuffer.empty[T]
      var done: Option[Either[ParseError, List[T]]] = None
      while done.isEmpty do
        (for {
          el <- expectElement(cfg, this)
          _ = acc.addOne(el)
          _ <- expectComma
        } yield el) match
          case Left(CommaExpected(_)) if jsChars(i) == ']' =>
            i += 1
            eatWhitespace
            done = Some(Right(acc.toList))
          case Left(e) =>
            done = Some(Left(e))
          case Right(_) =>
      done.get

  def expectObject[K, V](
      cfg: JsonConfig,
      keyElement: (JsonConfig, JsonParser) => Either[ParseError, K],
      valueElement: (JsonConfig, JsonParser) => Either[ParseError, V]
  ): Either[ParseError, Map[K, V]] =
    if jsChars(i) != '{' then Left(JsonParseError(s"Beginning of object expected at position [$i]"))
    else
      i += 1
      eatWhitespace
      val acc = scala.collection.mutable.Map.empty[K, V]
      var done: Option[Either[ParseError, Map[K, V]]] = None
      while done.isEmpty do
        (for {
          keyLabel <- keyElement(cfg, this)
          _ <- expectColon
          mapVal <- valueElement(cfg, this)
          _ = acc.put(keyLabel, mapVal)
          _ <- expectComma
        } yield (keyLabel, mapVal)) match
          case Left(CommaExpected(_)) if jsChars(i) == '}' =>
            i += 1
            eatWhitespace
            done = Some(Right(acc.toMap))
          case Left(e) =>
            done = Some(Left(e))
          case Right(_) =>
      done.get

  // Special case of JSON object where each entry is a field of a class
  def expectClass[T](
      cfg: JsonConfig,
      fieldMap: Map[String, (JsonConfig, JsonParser) => Either[ParseError, ?]],
      instantiator: Map[String, ?] => T,
      fieldValues: scala.collection.mutable.HashMap[String, Any] // pre-set values (Option:None, default values)
  ): Either[ParseError, T] =
    if jsChars(i) != '{' then Left(JsonParseError(s"Beginning of class object expected at position [$i]"))
    else
      i += 1
      eatWhitespace
      var done: Option[Either[ParseError, T]] = None
      while done.isEmpty do
        (for {
          fieldLabel <- expectLabel
          _ <- expectColon
          fieldValue <- fieldMap(fieldLabel)(cfg, this)
          _ = fieldValues.put(fieldLabel, fieldValue)
          _ <- expectComma
        } yield fieldValue) match
          case Left(CommaExpected(_)) if jsChars(i) == '}' =>
            i += 1
            eatWhitespace
            done = Try(instantiator(fieldValues.toMap)) match // instantiate the class here!!!
              case Success(v) => Some(Right(v))
              case Failure(e) => Some(Left(ParseError(s"Unable to instantiate class at position [${i - 1}] with message ${e.getMessage}")))
          case Left(e) =>
            done = Some(Left(e))
          case Right(_) =>
      done.get

  // Slower String parsing that handles special escaped chars
  def _expectString =
    val builder = new java.lang.StringBuilder()
    while i < max && jsChars(i) != '"' do
      if jsChars(i) == '\\' then {
        jsChars(i + 1) match {
          case '"' =>
            builder.append('\"')
            i += 2

          case '\\' =>
            builder.append('\\')
            i += 2

          case 'b' =>
            builder.append('\b')
            i += 2

          case 'f' =>
            builder.append('\f')
            i += 2

          case 'n' =>
            builder.append('\n')
            i += 2

          case 'r' =>
            builder.append('\r')
            i += 2

          case 't' =>
            builder.append('\t')
            i += 2

          case 'u' =>
            val hexEncoded = js.substring(i + 2, i + 6)
            val unicodeChar = Integer.parseInt(hexEncoded, 16).toChar
            builder.append(unicodeChar.toString)
            i += 6

          case c =>
            builder.append(c)
            i += 2
        }
      } else {
        builder.append(jsChars(i))
        i += 1
      }
    builder.toString
