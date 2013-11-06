package co.blocke.scalajack
package fields

import reflect.runtime.universe._
import com.fasterxml.jackson.core._
import com.mongodb.casbah.Imports._

case class CaseClassField( name:String, dt:Type, className:String, applyMethod:java.lang.reflect.Method, fields:List[Field], caseObj:Object ) 
	extends Field with ClassOrTrait 
{
	val iFields = fields.map( f => (f.name, f)).toMap
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false ) : Boolean = {
		val cz = target.getClass
		val hintStr = { if( withHint ) "\""+hint+"\":\""+dt.typeSymbol.fullName.toString+"\"," else "" }
		val sb2 = new StringBuilder
		fields.map( oneField => { 
			val targetField = cz.getDeclaredField(oneField.name)
			targetField.setAccessible(true)
			val ftype = targetField.getType.getName
			val fval = targetField.get(target)
			oneField.render( sb2, fval, Some(oneField.name), ext, hint )
		}) 
		if( sb2.charAt(sb2.length-1) == ',' )
			sb2.deleteCharAt(sb2.length-1)
		label.fold({
				sb.append('{')
				sb.append(hintStr)
				sb.append(sb2)
				sb.append('}')
			})((label) => {
				sb.append('"')
				sb.append(label)
				sb.append("\":{")
				sb.append(hintStr)
				sb.append(sb2)
				sb.append("},")
			})
		true
	}

	private def getFieldValue[T]( f:Field, target:T ) = {
		val cz = target.getClass
		val targetField = cz.getDeclaredField(f.name)
		targetField.setAccessible(true)
		val ftype = targetField.getType.getName
		targetField.get(target)
	}

	override private[scalajack] def renderClassDB[T]( target:T, hint:String, withHint:Boolean = false ) : Any = {
		val dbo = MongoDBObject()
		if( withHint )
			dbo.put( hint, dt.typeSymbol.fullName.toString )
		val (keys, rest) = fields.partition( _.hasMongoAnno )
		if( keys.size == 1 ) {
			dbo.put("_id", keys.head.renderDB(getFieldValue(keys.head,target),None,hint))
		}
		else if( keys.size > 0 ) {
			val keydbo = MongoDBObject()
			keys.foreach( f => keydbo.put(f.name,f.renderDB(getFieldValue(f,target),None,hint) ) )
			dbo.put("_id", keydbo)
		}
		rest.map( oneField => {
			val fval = getFieldValue(oneField,target)
			if( fval != None )
				dbo.put( oneField.name, oneField.renderDB(fval, None, hint) ) 
		})
		dbo
	}

	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false ) : Any = {
		val dbo = MongoDBObject()
		val cz = target.getClass
		if( withHint )
			dbo.put( hint, dt.typeSymbol.fullName.toString )
		fields.map( oneField => {
			val targetField = cz.getDeclaredField(oneField.name)
			targetField.setAccessible(true)
			val ftype = targetField.getType.getName
			val fval = targetField.get(target)
			if( fval != None ) 
				dbo.put( oneField.name, oneField.renderDB(fval, None, hint) ) 
		})
		dbo
	}

	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = readClass(jp,ext,hint)

	override private[scalajack] def readClass[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = {
		// Token now sitting on '{' so advance and read list
		jp.nextToken  // consume '{'
		val fieldData = scala.collection.mutable.Map[String,Any]()
		while( jp.getCurrentToken != JsonToken.END_OBJECT ) {
			val fieldName = jp.getCurrentName
			jp.nextToken // scan to value
			if( fieldName == hint ) jp.nextToken
			else {
				val fd = (fieldName, iFields(fieldName).readValue(jp, ext, hint) )
				fieldData += fd
			}
		}
		jp.nextToken
		ScalaJack.poof( className, fieldData.toMap )				
	}

	override private[scalajack] def readClassDB[T]( src:DBObject, hint:String )(implicit m:Manifest[T]) : Any = {
		val fieldData = scala.collection.mutable.Map[String,Any]()
		fields.map( oneField => {
			val fd = ( oneField.name, {
				if( src.containsField(oneField.name) )
					oneField.readValueDB( src.get(oneField.name), hint ) 
				else if( src.containsField("_id") && oneField.hasMongoAnno ) {
					val sval = src.get("_id")
					if( sval.isInstanceOf[java.util.Map[_,_]] ) 
						oneField.readValueDB( sval.asInstanceOf[java.util.Map[String,_]].get(oneField.name), hint )
					else 
						oneField.readValueDB( sval, hint )
				} else None
				})
			fieldData += fd
		})	
		ScalaJack.poof( className, fieldData.toMap )				
	}
	override private[scalajack] def readValueDB[T]( src:Any, hint:String )(implicit m:Manifest[T]) : Any = {
		readClassDB( src.asInstanceOf[DBObject], hint )
	}
}