package co.blocke.scalajack
package fields

import com.fasterxml.jackson.core._
import com.mongodb.casbah.Imports._
import scala.collection.JavaConversions._

case class MapField( name:String, valueField:Field ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false )(implicit m:Manifest[T]) : Boolean = {
		val mapVal = target.asInstanceOf[Map[_,_]]
		if( mapVal.isEmpty ) label.fold( sb.append("{}") )((labelStr) => {
				sb.append('"')
				sb.append(labelStr)
				sb.append("\":{},")
			})
		else {
			val sb2 = new StringBuilder
			mapVal.map( { case (k,v) => {
				val sb3 = new StringBuilder
				if( valueField.render( sb3, v, None, ext, hint ) ) {
					sb2.append('"')
					sb2.append(k.toString)
					sb2.append("\":")
					sb2.append(sb3)
					sb2.append(',')
				}
			}})
			if( sb2.charAt(sb2.length-1) == ',' )
				sb2.deleteCharAt(sb2.length-1)
			label.fold({
					sb.append('{')
					sb.append(sb2)
					sb.append('}')
				})((labelStr) => {
					sb.append('"')
					sb.append(labelStr)
					sb.append("\":{")
					sb.append(sb2)
					sb.append("},")
				})
		}
		true
	}
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = {
		val mapVal = target.asInstanceOf[Map[_,_]]
		val mo = MongoDBObject()
		mapVal.collect {  case (k,v) if( v != None ) => {
			mo.put(k.toString, valueField.renderDB(v, None, hint ))
			}}
		mo
	}
	private def readMapField[T]( jp:JsonParser, vf:Field, ext:Boolean, hint:String )(implicit m:Manifest[T]) : (Any,Any) = {
		val fieldName = jp.getCurrentName
		jp.nextToken
		(fieldName, vf.readValue( jp, ext, hint ))
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = {
		if( jp.getCurrentToken != JsonToken.START_OBJECT) throw new IllegalArgumentException("Expected '{'")
		// Token now sitting on '{' so advance and read list
		jp.nextToken
		val fieldData = scala.collection.mutable.Map[Any,Any]()
		while( jp.getCurrentToken != JsonToken.END_OBJECT ) 
			fieldData += readMapField( jp, valueField, ext, hint )
		jp.nextToken
		fieldData.toMap
	}
	override private[scalajack] def readValueDB[T]( src:Any, hint:String )(implicit m:Manifest[T]) : Any = {
		val dbo = src.asInstanceOf[DBObject]
		dbo.keySet.map( key => (key,valueField.readValueDB(dbo.get(key), hint)) ).toMap
	}
}
