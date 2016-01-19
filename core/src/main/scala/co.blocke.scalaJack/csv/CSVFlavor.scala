package co.blocke.scalajack
package csv

import scala.reflect.runtime.universe._
import scala.collection.mutable.{Map => MMap,ListBuffer}
import org.joda.time.DateTime
import PrimitiveTypes._

/*
	NOTE: CSV support for classes is very limited, mainly because CSV is not an expressvie representation.
	Classes must be "flat": no sub-classes or collections as members.  Optionals are OK.
	No traits allowed.  Case classes must be concrete.
 */

case class CSVParseException(msg:String) extends Exception(msg)
case class CSVField( value:String, isQuoted:Boolean )

case class CSVFlavor() extends FlavorKind[String] {
	def makeScalaJack : ScalaJack[String] = new CSVScalaJack()  
	class CSVScalaJack() extends ScalaJack[String] with CSVJackFlavor
}

trait CSVJackFlavor extends JackFlavor[String] {
	def rr = new CSVReadRenderer()
	class CSVReadRenderer() extends ReadRenderer {
		def read[T](src:String)(implicit tt:TypeTag[T], vc:VisitorContext=VisitorContext()) : T = {
			val sjTypeName = tt.tpe.typeSymbol.fullName
			Analyzer.inspectByName(sjTypeName) match {
				case cc:CCType =>
					val data = parse(src)
					if( cc.members.size != data.size ) throw new CSVParseException("Number of CSV fields doesn't match the number of case class fields.")
					val fldval = cc.members.zip( data ).map{ case ((fieldName,(atype, defaultVal)), value) =>
						val marshalled = value match {
							case CSVField("null",false) => null
							case CSVField("",false) if(defaultVal.isDefined) => defaultVal.get
							case csv:CSVField if(primitiveTypes.contains(atype.name)) => reifyValue(atype,csv.value)
							case csv:CSVField if(fieldIsOptional(atype)) => reifyOptional(atype.asInstanceOf[CollType],csv.value)
							case _ => throw new CSVParseException("Only primitive fields allowed! (No collections, classes, etc.)")
						}
						(fieldName, marshalled)
					}.toMap
					Util.poof( cc, fldval )(tt).asInstanceOf[T]
				case _ => throw new CSVParseException("CSV parser can only handle case classes.")
			}
		}

		def render[T](instance:T)(implicit tt:TypeTag[T], vc:VisitorContext=VisitorContext()) : String = {
			val sjTypeName = tt.tpe.typeSymbol.fullName
			val buf = scala.collection.mutable.ListBuffer.empty[String]
			Analyzer.inspectByName(sjTypeName) match {
				case cc:CCType =>
					cc.members.foreach{ case(fname, (ftype,defaultVal)) => {
						val cz = instance.getClass()
						val targetField = cz.getDeclaredField(fname)
						targetField.setAccessible(true)
						buf += _render(ftype, targetField.get(instance), tt.tpe.typeArgs)
					}}
				case _ => throw new CSVParseException("CSV parser can only handle case classes.")
			}
			buf.mkString(",")
		}

		private def fieldIsOptional(atype:AType) = atype.isInstanceOf[CollType] && atype.asInstanceOf[CollType].isOptional && primitiveTypes.contains(atype.asInstanceOf[CollType].colTypes.head.name)
		private def reifyValue(atype:AType, value:String) : Any = primitiveTypes.get(atype.name).map(fn => fn(value)).get
		private def reifyOptional(otype:CollType, value:String) : Option[Any] = 
			if(value == "") None
			else Some(reifyValue(otype.colTypes.head, value))

		private def parse( csv:String, delim:Char = ',' ) : List[CSVField] = {

			var startLatch = true
			var inQuotedValue = false
			var quoteEscape = false
			var quotedValue = false

			val buf = scala.collection.mutable.ListBuffer.empty[CSVField]
			var out = scala.collection.mutable.ArrayBuffer.empty[Char]

			csv.toCharArray.toList.zipWithIndex.map{ case(c,i) => c match {
				case `delim` =>
					var wasQuotedVal = false
					if(quoteEscape) {
						inQuotedValue = false
						wasQuotedVal = true
					}
					quoteEscape = false
					if(inQuotedValue)
						out += `delim`
					else {
						buf += CSVField( out.mkString, wasQuotedVal )
						out.clear
						startLatch = true
					}

				case '"' if(startLatch) =>
					inQuotedValue = true
					startLatch = false
					quoteEscape = false

				case '"' if(quoteEscape) =>
					quoteEscape = false
					out += '"'

				case '"' =>
					quoteEscape = true

				case a =>
					if(quoteEscape)
						throw new CSVParseException("Expected either \" or "+delim+" but found "+a)
					out += a
					startLatch = false
			}}
			buf += CSVField( out.mkString, quoteEscape )
			buf.toList
		}

		private def _render[T](
			graph    : AType, 
			instance : T, 
			typeArgs : List[Type]=List.empty[Type]
		)(implicit tt:TypeTag[T], vc:VisitorContext) : String = {
			graph match {
				case _ if( instance == null ) => 
					"null"
				case g:PrimType  => 
					g.name match {
						case "String" | "java.lang.String" | "scala.Char" | "scala.Enumeration.Value" | "java.util.UUID" => 
							clean(instance.toString)
						case "org.joda.time.DateTime" =>
							instance.asInstanceOf[DateTime].getMillis.asInstanceOf[Long].toString
						case "scala.Any" => 
							clean(explodeAny(instance))
						case x => 
							instance.toString
					}
				case g:CollType if(g.name == "scala.Option" && g.colTypes.head.isInstanceOf[PrimType]) =>
					val optVal = instance.asInstanceOf[Option[_]]
					optVal.map( ov => _render(g.colTypes.head, ov, tt.tpe.typeArgs) ).getOrElse("")
				case _ => throw new CSVParseException("Only primitive fields allowed! (No collections, classes, etc.)")
			}
		}

		private def explodeAny( inst:Any ) : String = inst match {
			case l:List[_]  => throw new CSVParseException("Only primitive fields allowed! (No collections, classes, etc.)")
			case m:Map[_,_] => throw new CSVParseException("Only primitive fields allowed! (No collections, classes, etc.)")
			case n if(inst == null) => ""
			case x          => 	x.toString
		}

		override def clean( input:String ) : String = {
			var needsQuotes = false
			val buffer = new StringBuffer(input.length())
			for ( i <- 0 to input.length-1 ) {
				if ( input.charAt(i) > 256 ) {
					val hex = Integer.toHexString( input.charAt(i))
					buffer.append("\\u").append(hex.reverse.padTo(4, "0").reverse.mkString)
				} else buffer.append( input.charAt(i) match {
					case '\"' => 
						needsQuotes = true
						"\"\""
					case c if(c.isLetterOrDigit || c == ' ') => c
					case c    => 
						needsQuotes = true
						c
				})
			}
			if( needsQuotes ) "\""+buffer.toString+"\""
			else {
				val s = buffer.toString
				if( s == "null" ) "\"null\""
				else s
			}
		}
    }
}
