package co.blocke.scalajack
package json

import scala.util.*

case class ParseError(msg: String) extends Throwable

case class JsonParser( js: String ):

    private val jsChars: Array[Char] = js.toCharArray
    private var i                    = 0
    private val max: Int             = jsChars.length

    // Housekeeping
    //------------------------------
    @inline def nullCheck: Boolean = 
        val res = i+3<max && jsChars(i)=='n' && jsChars(i+1)=='u' && jsChars(i+2)=='l' && jsChars(i+3)=='l'
        if res then i += 4
        res
    @inline def eatWhitespace: Either[ParseError,Unit] = 
        while (i < max && jsChars(i).isWhitespace) i += 1
        Right(())
    @inline def expectComma: Either[ParseError,Unit] =  // Note: this consumes whitespace before/after the ','
        for {
            _ <- eatWhitespace
            r <- if jsChars(i) == ',' then 
                i += 1
                eatWhitespace
                Right(()) 
            else 
                Left(ParseError(s"Expected comma at position [$i]"))
        } yield r
    @inline def expectColon: Either[ParseError,Unit] =  // Note: this consumes whitespace before/after the ':'
        for {
            _ <- eatWhitespace
            r <- if jsChars(i) == ':' then 
                i += 1
                eatWhitespace
                Right(()) 
            else 
                Left(ParseError(s"Expected colon at position [$i]"))
        } yield r

    // JSON label (i.e. object key, which is always a simple string)
    def expectLabel: Either[ParseError,String] = 
        jsChars(i) match
            case '"' =>
                i += 1
                val mark = i
                while (i < max && jsChars(i) != '"') i += 1
                i += 1
                Right(js.substring(mark, i-1))
            case x => Left(ParseError(s"Unexpected character '$x' where beginning of label expected at position [$i]"))


    // Data Types
    //------------------------------
    def expectBoolean(cfg: JsonConfig): Either[ParseError,Boolean] =
        jsChars(i) match
            case 't' if i+3<max && jsChars(i+1)=='r' && jsChars(i+2)=='u' && jsChars(i+3)=='e' => 
                i += 4
                Right(true)
            case 'f' if i+4<max && jsChars(i+1)=='a' && jsChars(i+2)=='l' && jsChars(i+3)=='s' && jsChars(i+3)=='e' => 
                i += 5
                Right(false)
            case x => Left(ParseError(s"Unexpected character '$x' where beginning of boolean value expected at position [$i]"))

    def expectLong(cfg: JsonConfig): Either[ParseError,Long] =
        val mark = i
        var done = false
        while !done do
            jsChars(i) match
                case c if (c >= '0' && c <= '9') || c == '-' => i += 1
                case _ => done = true
        Try( js.substring(mark,i).toLong ) match
            case Success(g) => Right(g)
            case Failure(f) => 
                val msg = if mark == i then
                    s"""Int/Long expected but couldn't parse from "${jsChars(i)}" at position [$i]"""
                else
                    s"""Int/Long expected but couldn't parse from "${js.substring(mark,i)}" at position [$i]"""
                i = mark
                Left(ParseError(msg))

    def expectDouble(cfg: JsonConfig): Either[ParseError,Double] =
        val mark = i
        var done = false
        while !done do
            jsChars(i) match
                case c if (c >= '0' && c <= '9') || c == '-' || c == '.' || c == 'e' || c == 'E' || c == '+' => i += 1
                case _ => done = true
        Try( js.substring(mark,i-1).toDouble ) match
            case Success(g) => Right(g)
            case Failure(_) => 
                val msg = if mark == i then
                    s"Float/Double expected but couldn't parse from \"${jsChars(i)}\" at position [$i]"
                else
                    s"Float/Double expected but couldn't parse from \"${js.substring(mark,i-1)}\" at position [$i]"
                i = mark
                Left(ParseError(msg))

    def expectOption[T](cfg: JsonConfig, expectElement: JsonConfig=>Either[ParseError,T]): Either[ParseError, Option[T]] =
        nullCheck match
            case false => expectElement(cfg).map(t => Some(t))
            case true if cfg.noneAsNull => Right(None)
            case true if cfg.forbidNullsInInput => Left(ParseError(s"Forbidden 'null' value received at position [$i]")) 
            case true => Right(Some(null.asInstanceOf[T]))

    def expectList[T]( cfg: JsonConfig, expectElement: JsonConfig=>Either[ParseError,T]): Either[ParseError,List[T]] = 
        if jsChars(i) != '[' then Left(ParseError(s"Beginning of list expected at position [$i]"))
        else
            i += 1
            eatWhitespace
            val acc = scala.collection.mutable.ListBuffer.empty[T]
            var done: Option[Either[ParseError,List[T]]] = None
            while done.isEmpty do
                (for {
                    el <- expectElement(cfg)
                    _ = acc.addOne(el)
                    _ <- expectComma
                } yield el) match
                    case Left(_) if jsChars(i) == ']' => 
                        i += 1
                        eatWhitespace
                        done = Some(Right(acc.toList))
                    case Left(e) =>
                        done = Some(Left(e))
                    case Right(_) => 
            done.get
            
    def expectClass[T]( 
        cfg: JsonConfig, 
        fieldMap: Map[String, (JsonConfig) => Either[ParseError, ?]],
        instantiator: Map[String, ?] => T
      ): Either[ParseError,T] =
        if jsChars(i) != '{' then Left(ParseError(s"Beginning of object expected at position [$i]"))
        else
            i += 1
            eatWhitespace
            var done: Option[Either[ParseError,T]] = None
            val fields = scala.collection.mutable.HashMap.empty[String,Any]
            while done.isEmpty do
                (for {
                    fieldLabel <- expectLabel
                    _ <- expectColon
                    fieldValue <- fieldMap(fieldLabel)(cfg)
                    _ = fields.put(fieldLabel, fieldValue)
                    _ <- expectComma
                } yield fieldValue) match
                    case Left(_) if jsChars(i) == '}' => 
                        i += 1
                        eatWhitespace
                        done = Some(Right( instantiator(fields.toMap) )) // instantiate the class here!!!
                    case Left(e) =>
                        done = Some(Left(e))
                    case Right(_) => 
            done.get