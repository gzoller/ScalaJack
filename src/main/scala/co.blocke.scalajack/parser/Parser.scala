package co.blocke.scalajack
package parser

import scala.collection.mutable.HashMap

abstract class ParseError(message: String) extends Throwable(message)


trait Parser:
 
    inline def parse(inst: Instruction): Either[ParseError, inst.Z] = 
        (inst.expect match
            case Expects.ExpectString    => inst.asInstanceOf[SimpleInstruction[String,?]].transform(parseString())
            case Expects.ExpectLong      => inst.asInstanceOf[SimpleInstruction[Long,?]].transform(parseLong())
            case Expects.ExpectBoolean   => parseBoolean()
            case Expects.ExpectList      => 
                val z = inst.asInstanceOf[ListInstruction[?,inst.Z]]
                z.transform(parseList(z.elementInstruction))
            case Expects.ExpectClass     => 
                val z = inst.asInstanceOf[ScalaClassInstruction[inst.Z]]
                z.transform(parseClass(z.fieldInstructions, HashMap.empty[String,Any])) // TODO: HashMap of default values, option:None, etc
            case Expects.ExpectObject    => Left(new json.JsonParseError("Unupported"))
            case Expects.ExpectTuple     => Left(new json.JsonParseError("Unupported"))
            case Expects.ExpectDouble    => Left(new json.JsonParseError("Unupported"))
            case Expects.ExpectBigLong   => Left(new json.JsonParseError("Unupported"))
            case Expects.ExpectBigDouble => Left(new json.JsonParseError("Unupported"))
        ).asInstanceOf[Either[ParseError,inst.Z]]

        
    //-----------------------------------------------------

    // Parse the raw primitives
    inline def parseBoolean(): Either[ParseError, Boolean]
    inline def parseString(): Either[ParseError, String]
    inline def parseLong(): Either[ParseError, Long]
    def parseList(inst: Instruction): Either[ParseError, List[inst.Z]]
    def parseClass(inst: Map[String,Instruction], fieldValues: HashMap[String,Any]): Either[ParseError, Map[String,Any]]

    //-----------------------------------------------------
    
