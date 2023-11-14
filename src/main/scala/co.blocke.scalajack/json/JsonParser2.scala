package co.blocke.scalajack
package json

import parser.* 

import scala.util.*
import co.blocke.scala_reflection.TypedName
import scala.collection.mutable.{ListBuffer, HashMap}

case class JsonParser2(js: String, cfg: JsonConfig) extends Parser: 

  //--------------------------------------
  //--------------------------------------
  //      JSON Housekeeping
  //--------------------------------------
  //--------------------------------------

  private val jsChars: Array[Char] = js.toCharArray
  private var i = 0
  private val max: Int = jsChars.length

  inline def eatWhitespace =
    while i < max && jsChars(i).isWhitespace do i += 1

  inline def nullCheck: Boolean =
    if i + 3 < max && jsChars(i) == 'n' && jsChars(i + 1) == 'u' && jsChars(i + 2) == 'l' && jsChars(i + 3) == 'l' then
        i += 4
        true
    else
        false

  inline def expectComma: Either[ParseError, Unit] = // Note: this consumes whitespace before/after the ','
    eatWhitespace
    if jsChars(i) == ',' then
      i += 1
      eatWhitespace
      Right(())
    else 
        Left(CommaExpected(showError(s"Comma expected at position [$i]")))

  inline def expectColon: Either[ParseError, Unit] = // Note: this consumes whitespace before/after the ':'
    eatWhitespace
    if jsChars(i) == ':' then
      i += 1
      eatWhitespace
      Right(())
    else 
      Left(JsonParseError(showError(s"Expected colon at position [$i]")))

  // JSON label (i.e. object key, which is always a simple string, ie no escaped/special chars)
  inline def expectLabel: Either[ParseError, String] =
    jsChars(i) match
      case '"' =>
        i += 1
        val mark = i
        while i < max && jsChars(i) != '"' do i += 1
        i += 1
        Right(js.substring(mark, i - 1))
      case x => Left(JsonParseError(showError(s"Unexpected character '$x' where beginning of label expected at position [$i]")))

  //--------------------------------------
  //--------------------------------------
  //      Parser Implementation
  //--------------------------------------
  //--------------------------------------
  inline def parseBoolean(): Either[ParseError, Boolean] =
    jsChars(i) match
      case 't' if i + 3 < max && jsChars(i + 1) == 'r' && jsChars(i + 2) == 'u' && jsChars(i + 3) == 'e' =>
        i += 4
        Right(true)
      case 'f' if i + 4 < max && jsChars(i + 1) == 'a' && jsChars(i + 2) == 'l' && jsChars(i + 3) == 's' && jsChars(i + 4) == 'e' =>
        i += 5
        Right(false)
      case x =>
        // if (isMapKey || cfg.permissivePrimitives) && jsChars(i) == '"' then
        //   for {
        //     _ <- expectQuote
        //     result <- expectBoolean()
        //     _ <- expectQuote
        //   } yield result
        // else
            Left(JsonParseError(showError(s"Unexpected character '$x' where beginning of boolean value expected at position [$i]")))


  inline def parseString(): Either[ParseError, String] =
    nullCheck match
      case true if cfg.forbidNullsInInput => Left(JsonParseError(showError(s"Forbidden 'null' value received at position [$i]")))
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
          case x => Left(JsonParseError(showError(s"Unexpected character '$x' where beginning of a string expected at position [$i]")))

  inline def parseLong(): Either[ParseError, Long] =
    val mark = i
    var done = false
    while !done do
      jsChars(i) match
        case c if (c >= '0' && c <= '9') || c == '-' => i += 1
        case _                                       => done = true
    Try(js.substring(mark, i).toLong) match
      case Success(g) => Right(g)
      case Failure(f) =>
        // if (isMapKey || cfg.permissivePrimitives) && jsChars(i) == '"' then
        //   for {
        //     _ <- expectQuote
        //     result <- expectLong()
        //     _ <- expectQuote
        //   } yield result
        // else
          val msg =
            if mark == i then s"""Int/Long expected but couldn't parse from "${jsChars(i)}" at position [$i]"""
            else s"""Int/Long expected but couldn't parse from "${js.substring(mark, i)}" at position [$i]"""
          i = mark
          Left(JsonParseError(showError(msg)))

  def parseList(inst: Instruction): Either[ParseError, List[inst.Z]] =
    nullCheck match
      case true if cfg.forbidNullsInInput => Left(JsonParseError(showError(s"Forbidden 'null' value received at position [$i]")))
      case true                           => Right(null)
      case false =>
        if jsChars(i) != '[' then Left(JsonParseError(showError(s"Beginning of list expected at position [$i]")))
        else
          i += 1
          eatWhitespace
          val acc = ListBuffer.empty[inst.Z]
          var done: Option[Either[ParseError, List[inst.Z]]] = None
          while done.isEmpty do
            (for {
              el <- parse(inst)
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

  // Special case of JSON object where each entry is a field of a class
  def parseClass(inst: Map[String,Instruction], fieldValues: HashMap[String,Any]): Either[ParseError, Map[String,Any]] = 
    if jsChars(i) != '{' then Left(JsonParseError(showError(s"Beginning of class object expected at position [$i]")))
    else
      i += 1
      eatWhitespace
      var done: Option[Either[ParseError, Map[String, ?]]] = None
      while done.isEmpty do
        (for {
          fieldLabel <- expectLabel
          _ <- expectColon
          fieldValue <- inst.get(fieldLabel).map(parse(_)).getOrElse(null)
          _ = fieldValues.put(fieldLabel, fieldValue)
          _ <- expectComma
        } yield fieldValue) match
          case Left(CommaExpected(_)) if jsChars(i) == '}' =>
            i += 1
            eatWhitespace
            done = Some(Right(fieldValues.toMap))
          case Left(e) =>
            done = Some(Left(e))
          case Right(_) =>
      done.get

  //--------------------------------------
  //--------------------------------------
  //      Utility Functions
  //--------------------------------------
  //--------------------------------------

  // Slower String parsing that handles special escaped chars
  private def _expectString =
    val builder = new java.lang.StringBuilder()
    while i < max && jsChars(i) != '"' do
      if jsChars(i) == '\\' then {
        jsChars(i + 1) match {
          case c @ ('"' | '\\' | 'b' | 'f' | 'n' | 'r' | 't') =>
            builder.append(c)
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

  def showError(msg: String): String = {
    val (clip, dashes) = i match {
      case ep if ep <= 50 && max < 80 => (js, ep)
      case ep if ep <= 50             => (js.substring(0, 77) + "...", ep)
      case ep if ep > 50 && ep + 30 >= max =>
        ("..." + js.substring(i - 49), 52)
      case ep => ("..." + js.substring(ep - 49, ep + 27) + "...", 52)
    }
    msg + s" at position [$i]" + "\n" + clip.replaceAll("[\n\t]", "~") + "\n" + ("-" * dashes) + "^"
  }
