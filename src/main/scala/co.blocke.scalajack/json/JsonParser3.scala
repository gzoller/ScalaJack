package co.blocke.scalajack
package json

import scala.util.{Try, Success}
import scala.annotation._

case class JsonParser3( js: String ):

  trait Terminal
  sealed trait JSON

  case class StringJson( b: Int, e: Int ) extends JSON:
      def get: String = js.substring(b,e)
  case class BooleanJson( v: Boolean ) extends JSON
  case class IntJson( v: Long ) extends JSON
  case class FloatJson( v: Double ) extends JSON
  case class ObjectJson( v: Map[String, JSON] ) extends JSON
  case class ArrayJson( v: List[JSON] ) extends JSON
  case class NullJson() extends JSON

  case class TerminateArrayJson() extends JSON with Terminal  // marker for end of Array/Object
  case class TerminateObjectJson() extends JSON with Terminal // marker for end of Array/Object

  private val jsChars: Array[Char] = js.toCharArray
  private var i = 0
  private val max: Int = jsChars.length

  def reset() = i = 0
    
  def parse(expectComma: Boolean = false): JSON =
    var result: Option[JSON] = None
    val commaFound: Option[JSON] = 
        if expectComma then
            var found = if i == max && (jsChars(i-1)==']' || jsChars(i-1)=='}') then true else false
            var done = false
            while i < max && !done do
                (jsChars(i): @switch) match
                    case ' ' =>
                        i += 1
                    case '\n' =>
                        i += 1
                    case '\t' =>
                        i += 1
                    case '\r' =>
                        i += 1
                    case ',' =>
                        i += 1
                        found = true
                        done = true
                    case ':' =>  // cheat!  Treat ',' and ':' the same, so {"foo","bar","a","b"} == {"foo":"bar","a":"b"}
                        i += 1
                        found = true
                        done = true
                    case ']' =>
                        i += 1
                        found = true
                        done = true
                    case '}' =>
                        i += 1
                        found = true
                        done = true
                    case c =>
                        done = true
            found match
                case true if jsChars(i-1) == '}' => Some(TerminateObjectJson())
                case true if jsChars(i-1) == ']' => Some(TerminateArrayJson())
                case true => None // comma found... continue parsing
                case _ => throw new JsonParseError("Unterminated array or object")
        else
            None // no comma involvement--continue parsing        
        
    commaFound.getOrElse{
        while i < max && result.isEmpty do {
            (jsChars(i): @switch) match
                case '"' =>
                    i += 1
                    result = Some(parseString())
                case 't' | 'f' =>
                    result = Some(parseBoolean())
                case 'n' =>
                    result = Some(parseNull())
                case ' ' =>
                    i += 1
                case '\n' =>
                    i += 1
                case '\t' =>
                    i += 1
                case '\r' =>
                    i += 1
                case ']' =>
                    i += 1
                    result = Some(TerminateArrayJson())
                case '}' =>
                    i += 1
                    result = Some(TerminateObjectJson())
                case '[' =>
                    val acc = scala.collection.mutable.ListBuffer.empty[JSON]
                    i += 1
                    var last: JSON = parse()
                    while !last.isInstanceOf[TerminateArrayJson] do
                        acc.addOne(last)
                        last = parse(true)
                    result = Some(ArrayJson(acc.toList))
                case '{' =>
                    val acc = scala.collection.mutable.Map.empty[String,JSON]
                    i += 1
                    var done = false
                    var key: JSON = parse()
                    while !done do
                        key match
                            case k if k.isInstanceOf[StringJson] => 
                                // while i < max && !key.isInstanceOf[TerminateObjectJson] do
                                val value: JSON = parse(true)
                                if value.isInstanceOf[Terminal] then
                                    throw new JsonParseError("Malformed object")
                                acc.put(k.asInstanceOf[StringJson].get, value)
                                key = parse(true)
                            case k if k.isInstanceOf[TerminateObjectJson] => 
                                result = Some(ObjectJson(acc.toMap))
                                done = true
                            case _ => 
                                throw new JsonParseError("Malformed object (invalid key value)")
                    if i == max && key.isInstanceOf[TerminateObjectJson] then result = Some(ObjectJson(acc.toMap))
                case c if (c >= '0' && c <= '9') || c == '-' || c == '.' || c == '+' =>
                    result = Some(parseNumber())
        }
        result.getOrElse(throw new JsonParseError("Unterminated array or object"))
    }
        
  inline def parseNull() =
    if i + 3 < max && jsChars(i) == 'n' && jsChars(i + 1) == 'u' && jsChars(i + 2) == 'l' && jsChars(i + 3) == 'l' then
        i += 4
        NullJson()
    else
        throw new JsonParseError(s"Unexpected character '${jsChars(i)}'")

  inline def parseBoolean() =
    jsChars(i) match
      case 't' if i + 3 < max && jsChars(i + 1) == 'r' && jsChars(i + 2) == 'u' && jsChars(i + 3) == 'e' =>
        i += 4
        BooleanJson(true)
      case 'f' if i + 4 < max && jsChars(i + 1) == 'a' && jsChars(i + 2) == 'l' && jsChars(i + 3) == 's' && jsChars(i + 4) == 'e' =>
        i += 5
        BooleanJson(false)
      case _ =>
        throw new JsonParseError(s"Unexpected character '${jsChars(i)}'")

  inline def parseString() =
    val mark = i
    while i < max && jsChars(i) != '"' do
        if jsChars(i) == '\\' then // skip escaped special characters
            i += 1
        i += 1
    if i == max then
        i = mark
        throw new JsonParseError(s"Unterminated string starting")
    i += 1
    StringJson(mark, i-1)

  inline def parseNumber() =
    var isFloat = false
    var done = false
    val mark = i
    while i < max && !done do
        jsChars(i) match
            case c if (c >= '0' && c <= '9') => 
                i += 1
            case '.' =>
                isFloat = true
                i += 1
            case 'e' | 'E' | '-' | '+' =>
                i += 1
            case _ => 
                done = true
    if isFloat then
        Try(js.substring(mark,i).toDouble) match
            case Success(v) => 
                FloatJson(v)
            case _ => 
                i = mark
                throw new JsonParseError(s"Cannot parse number starting")
    else
        Try(js.substring(mark,i).toLong) match
            case Success(v) => 
                IntJson(v)
            case _ => 
                i = mark
                throw new JsonParseError(s"Cannot parse number starting")
