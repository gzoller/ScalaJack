package co.nubilus.scalajack

import reflect.runtime.universe._
import com.fasterxml.jackson.core._

trait Field {
	private[scalajack] val name         : String
	private[scalajack] val hasMongoAnno : Boolean = false
	private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], hint:String, withHint:Boolean=false ) : Boolean = {
		label.fold( {
				sb.append(target)
			})((labelStr) => {
				sb.append('"')
				sb.append( labelStr )
				sb.append("\":")
				sb.append(target)
				sb.append(',')
			})
		true
	}
	private[scalajack] def readValue[T]( jp:JsonParser, hint:String )(implicit m:Manifest[T]) : Any = { 0 }
}

case class StringField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], hint:String, withHint:Boolean=false ) : Boolean = {
		label.fold( {
				sb.append('"')
				sb.append(target)
				sb.append('"')
			})((labelStr) => {
				sb.append('"')
				sb.append( labelStr )
				sb.append("\":\"")
				sb.append(target.toString)
				sb.append("\",")
			})
		true
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, hint:String )(implicit m:Manifest[T]) : Any = {
		val v = jp.getValueAsString
		jp.nextToken
		v
	}
}
case class IntField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def readValue[T]( jp:JsonParser, hint:String )(implicit m:Manifest[T]) : Any = {
		val v = jp.getValueAsInt
		jp.nextToken
		v
	}}
case class LongField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def readValue[T]( jp:JsonParser, hint:String )(implicit m:Manifest[T]) : Any = {
		val v = jp.getValueAsLong
		jp.nextToken
		v
	}
}
case class BoolField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def readValue[T]( jp:JsonParser, hint:String )(implicit m:Manifest[T]) : Any = {
		val v = jp.getValueAsBoolean
		jp.nextToken
		v
	}
}

case class EnumField( name:String, enum:Enumeration ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], hint:String, withHint:Boolean=false ) : Boolean = {
		label.fold( {
				sb.append('"')
				sb.append(target)
				sb.append('"')
			})((labelStr) => {
				sb.append('"')
				sb.append( labelStr )
				sb.append("\":\"")
				sb.append(target)
				sb.append("\",")
			})
		true
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, hint:String )(implicit m:Manifest[T]) : Any = {
		val v = enum.withName(jp.getValueAsString)
		jp.nextToken
		v
	}
}

case class TraitField( name:String ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], hint:String, withHint:Boolean=false ) : Boolean = {
		Analyzer(target.getClass.getName).render( sb, target, label, hint, true )
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, hint:String )(implicit m:Manifest[T]) : Any = readClass(jp,hint)
	def readClass[T]( jp:JsonParser, hint:String )(implicit m:Manifest[T]) : Any = {
		jp.nextToken  // consume '{'
		val fieldData = scala.collection.mutable.Map[String,Any]()
		// read hint
		jp.nextToken // skip _hint label
		val traitClassName = jp.getValueAsString
		Analyzer(traitClassName).asInstanceOf[CaseClassField].readClass( jp, hint )
	}
}

case class OptField( name:String, subField:Field ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], hint:String, withHint:Boolean=false ) : Boolean = {
		val optVal = target.asInstanceOf[Option[_]]
		if( optVal != None ) {
			subField.render( sb, optVal.get, label, hint )
			true
		}
		else false 
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, hint:String )(implicit m:Manifest[T]) : Any = Some(subField.readValue(jp,hint))
}

case class ListField( name:String, subField:Field ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], hint:String, withHint:Boolean=false ) : Boolean = {
		val listVal = target.asInstanceOf[Iterable[_]]
		if( listVal.isEmpty ) label.fold( sb.append("[]"))((labelStr) => {
				sb.append('"')
				sb.append(labelStr)
				sb.append("\":[],")
			})
		else {
			val sb2 = new StringBuilder
			listVal.map( item => if( subField.render( sb2, item, None, hint ) ) sb2.append(',') )
			if( sb2.charAt(sb2.length-1) == ',' )
				sb2.deleteCharAt(sb2.length-1)
			label.fold({
					sb.append('[')
					sb.append(sb2)
					sb.append(']')
				})((labelStr) => {
					sb.append('"')
					sb.append(labelStr)
					sb.append("\":[")
					sb.append(sb2)
					sb.append("],")
				})
		}
		true
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, hint:String )(implicit m:Manifest[T]) : Any = {
		// Token now sitting on '[' so advance and read list
		jp.nextToken
		val fieldData = scala.collection.mutable.ListBuffer[Any]()
		while( jp.getCurrentToken != JsonToken.END_ARRAY ) {
			fieldData += subField.readValue(jp,hint)
		}
		jp.nextToken
		fieldData.toList
	}
}

case class MapField( name:String, valueField:Field ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], hint:String, withHint:Boolean=false ) : Boolean = {
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
				if( valueField.render( sb3, v, None, hint ) ) {
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
	private def readMapField[T]( jp:JsonParser, vf:Field, hint:String )(implicit m:Manifest[T]) : (Any,Any) = {
		val fieldName = jp.getCurrentName
		jp.nextToken
		(fieldName, vf.readValue( jp, hint ))
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, hint:String )(implicit m:Manifest[T]) : Any = {
		// Token now sitting on '{' so advance and read list
		jp.nextToken
		val fieldData = scala.collection.mutable.Map[Any,Any]()
		while( jp.getCurrentToken != JsonToken.END_OBJECT ) 
			fieldData += readMapField( jp, valueField, hint )
		jp.nextToken
		fieldData.toMap
	}
}

case class CaseClassField( name:String, dt:Type, className:String, applyMethod:java.lang.reflect.Method, fields:List[Field], caseObj:Object ) extends Field {
	val iFields = fields.map( f => (f.name, f)).toMap
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], hint:String, withHint:Boolean=false ) : Boolean = {
		val cz = target.getClass
		val hintStr = { if( withHint ) "\""+hint+"\":\""+dt.typeSymbol.fullName.toString+"\"," else "" }
		val sb2 = new StringBuilder
		fields.map( oneField => { 
			val targetField = cz.getDeclaredField(oneField.name)
			targetField.setAccessible(true)
			val ftype = targetField.getType.getName
			val fval = targetField.get(target)
			oneField.render( sb2, fval, Some(oneField.name), hint )
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
	override private[scalajack] def readValue[T]( jp:JsonParser, hint:String )(implicit m:Manifest[T]) : Any = readClass(jp,hint)
	def readClass[T]( jp:JsonParser, hint:String )(implicit m:Manifest[T]) : Any = {
		// Token now sitting on '{' so advance and read list
		jp.nextToken  // consume '{'
		val fieldData = scala.collection.mutable.Map[String,Any]()
		while( jp.getCurrentToken != JsonToken.END_OBJECT ) {
			val fieldName = jp.getCurrentName
			jp.nextToken // scan to value
			if( fieldName == hint ) jp.nextToken
			else {
				val fd = (fieldName, iFields(fieldName).readValue(jp,hint) )
				fieldData += fd
			}
		}
		jp.nextToken
		ScalaJack.poof( className, fieldData.toMap )				
	}
}

case class ValueClassField( name:String, override val hasMongoAnno:Boolean, valueType:Field ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], hint:String, withHint:Boolean=false ) : Boolean = {
		valueType.render( sb, target, label, hint )
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, hint:String )(implicit m:Manifest[T]) : Any = valueType.readValue(jp,hint)
}
