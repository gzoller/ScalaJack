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

trait CSVReadRenderFrame extends ReadRenderFrame[String] { 
	def renderer = new CSVReadRender()

	class CSVReadRender() extends ReadRender {

		def read[T](src:String)(implicit tt:TypeTag[T], vc:VisitorContext=VisitorContext()) : T = {
			val sjTypeName = tt.tpe.typeSymbol.fullName
			Analyzer.inspectByName(sjTypeName) match {
				case cc:CCType =>
					val data = parse(src)
					println(data)
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

		private def fieldIsOptional(atype:AType) = atype.isInstanceOf[CollType] && atype.asInstanceOf[CollType].isOptional && primitiveTypes.contains(atype.asInstanceOf[CollType].colTypes.head.name)
		private def reifyValue(atype:AType, value:String) : Any = primitiveTypes.get(atype.name).map(fn => fn(value)).get
		private def reifyOptional(otype:CollType, value:String) : Option[Any] = 
			if(value == "") None
			else Some(reifyValue(otype.colTypes.head, value))

		def render[T](instance:T)(implicit tt:TypeTag[T], vc:VisitorContext) : String = {
			val buf = new StringBuilder()
			// _render(Analyzer.inspect(instance), instance, buf)
			buf.toString
		}

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
    }
}
