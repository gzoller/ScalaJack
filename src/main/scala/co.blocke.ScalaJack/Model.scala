package co.blocke.scalajack

import reflect.runtime.universe._
import com.fasterxml.jackson.core._
import scala.language.existentials // compiler-recommended include
import com.mongodb.casbah.Imports._

trait Field {
	private[scalajack] val name         : String
	private[scalajack] val hasMongoAnno : Boolean = false
	private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false ) : Boolean = {
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
	private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = { 0 }

	private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false ) : Any = { 0 }
}

case class StringField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false ) : Boolean = {
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
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false ) : Any = {
		target.toString
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = {
		val v = jp.getValueAsString
		jp.nextToken
		v
	}
}
case class IntField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String)(implicit m:Manifest[T]) : Any = {
		val v = jp.getValueAsInt
		jp.nextToken
		v
	}
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false ) : Any = {
		target.asInstanceOf[Int]
	}
}
case class CharField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false ) : Boolean = {
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
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false ) : Any = {
		target.asInstanceOf[Char]
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = {
		val v = jp.getValueAsString.charAt(0)
		jp.nextToken
		v
	}
}
case class LongField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = {
		val v = jp.getValueAsLong
		jp.nextToken
		v
	}
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false ) : Any = {
		target.asInstanceOf[Long]
	}
}
case class FloatField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = {
		val v = jp.getValueAsDouble.toFloat
		jp.nextToken
		v
	}
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false ) : Any = {
		target.asInstanceOf[Float]
	}
}
case class DoubleField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = {
		val v = jp.getValueAsDouble
		jp.nextToken
		v
	}
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false ) : Any = {
		target.asInstanceOf[Double]
	}
}
case class BoolField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = {
		val v = jp.getValueAsBoolean
		jp.nextToken
		v
	}
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false ) : Any = {
		target.asInstanceOf[Boolean]
	}
}

case class EnumField( name:String, enum:Enumeration ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false ) : Boolean = {
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
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false ) : Any = {
		target.toString
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = {
		val v = enum.withName(jp.getValueAsString)
		jp.nextToken
		v
	}
}

case class TraitField( name:String ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false ) : Boolean = {
		Analyzer(target.getClass.getName).render( sb, target, label, ext, hint, true )
	}
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false ) : Any = {
		Analyzer(target.getClass.getName).renderDB( target, label, hint, true )
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = readClass(jp,ext,hint)
	def readClass[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = {
		jp.nextToken  // consume '{'
		val fieldData = scala.collection.mutable.Map[String,Any]()
		// read hint
		jp.nextToken // skip _hint label
		val traitClassName = jp.getValueAsString
		Analyzer(traitClassName).asInstanceOf[CaseClassField].readClass( jp, ext, hint )
	}
}

case class OptField( name:String, subField:Field ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false ) : Boolean = {
		val optVal = target.asInstanceOf[Option[_]]
		if( optVal != None ) {
			// Handle gonzo value types, which are raw datatypes -- EXCEPT when contained in another structure!  Unbox manually here.
			if( subField.isInstanceOf[ValueClassField] ) {
				val valueFieldName = optVal.get.getClass.getDeclaredFields.head.getName
				val unboxedVal = optVal.get.getClass.getDeclaredMethods.toList.find(m => m.getName == valueFieldName).get.invoke(optVal.get)
				subField.render( sb, unboxedVal, label, ext, hint )
			}
			else 
				subField.render( sb, optVal.get, label, ext, hint ) 
			true
		}
		else false 
	}
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false ) : Any = {
		val optVal = target.asInstanceOf[Option[_]]
		if( optVal != None ) subField.renderDB( optVal.get, label, hint )
		else optVal
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = {
		if( subField.isInstanceOf[ValueClassField] ) {
			val sf = subField.asInstanceOf[ValueClassField]
			val raw = sf.readValue(jp,ext,hint)
			Some(sf.constructor.newInstance(raw.asInstanceOf[Object]))
		} else
			Some(subField.readValue(jp,ext,hint))
	}
}

case class ListField( name:String, subField:Field ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false ) : Boolean = {
		val listVal = target.asInstanceOf[Iterable[_]]
		if( listVal.isEmpty ) label.fold( sb.append("[]"))((labelStr) => {
				sb.append('"')
				sb.append(labelStr)
				sb.append("\":[],")
			})
		else {
			val sb2 = new StringBuilder
			if( subField.isInstanceOf[ValueClassField] ) {
				listVal.map( item => {
					val valueFieldName = item.getClass.getDeclaredFields.head.getName
					val unboxedVal = item.getClass.getDeclaredMethods.toList.find(m => m.getName == valueFieldName).get.invoke(item)
					if( subField.render( sb2, unboxedVal, None, ext, hint ) ) sb2.append(',') 
				})
			} else {
				listVal.map( item => {
					if( subField.render( sb2, item, None, ext, hint ) ) sb2.append(',') 
				})
			}
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
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false ) : Any = {
		val listVal = target.asInstanceOf[Iterable[_]]
		if( subField.isInstanceOf[ValueClassField] ) {
			listVal.collect{ case item => {
				val valueFieldName = item.getClass.getDeclaredFields.head.getName
				val unboxedVal = item.getClass.getDeclaredMethods.toList.find(m => m.getName == valueFieldName).get.invoke(item)
				subField.renderDB(unboxedVal, None, hint )
			} }
		} else 
			listVal.collect{ case item if(item != None) => subField.renderDB(item, None, hint ) }
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = {
		// Token now sitting on '[' so advance and read list
		jp.nextToken
		val fieldData = scala.collection.mutable.ListBuffer[Any]()
		if( subField.isInstanceOf[ValueClassField] ) {
			while( jp.getCurrentToken != JsonToken.END_ARRAY ) {
				val sf = subField.asInstanceOf[ValueClassField]
				val raw = sf.readValue(jp,ext,hint)
				fieldData += sf.constructor.newInstance(raw.asInstanceOf[Object])
			}
		} else {
			while( jp.getCurrentToken != JsonToken.END_ARRAY ) {
				fieldData += subField.readValue(jp, ext, hint)
			}
		}
		jp.nextToken
		fieldData.toList
	}
}

case class MapField( name:String, valueField:Field ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false ) : Boolean = {
		val mapVal = target.asInstanceOf[Map[_,_]]
		if( mapVal.isEmpty ) label.fold( sb.append("{}") )((labelStr) => {
				sb.append('"')
				sb.append(labelStr)
				sb.append("\":{},")
			})
		else {
			val sb2 = new StringBuilder
			if( valueField.isInstanceOf[ValueClassField] ) {
				mapVal.map( { case (k,v) => {
					val sb3 = new StringBuilder
					val valueFieldName = v.getClass.getDeclaredFields.head.getName
					val unboxedVal = v.getClass.getDeclaredMethods.toList.find(m => m.getName == valueFieldName).get.invoke(v)
					if( valueField.render( sb3, unboxedVal, None, ext, hint ) ) {
						sb2.append('"')
						sb2.append(k.toString)
						sb2.append("\":")
						sb2.append(sb3)
						sb2.append(',')
					}
				}})
			} else {
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
			}
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
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false ) : Any = {
		val mapVal = target.asInstanceOf[Map[_,_]]
		val mo = MongoDBObject()
		if( valueField.isInstanceOf[ValueClassField] ) {
			mapVal.collect {  case (k,v) => {
				val valueFieldName = v.getClass.getDeclaredFields.head.getName
				val unboxedVal = v.getClass.getDeclaredMethods.toList.find(m => m.getName == valueFieldName).get.invoke(v)
				mo.put(k.toString, valueField.renderDB(unboxedVal, None, hint ))
			}}
		} else 
			mapVal.collect {  case (k,v) if( v != None ) => {
				mo.put(k.toString, valueField.renderDB(v, None, hint ))
				}}
		mo
	}
	private def readMapField[T]( jp:JsonParser, vf:Field, ext:Boolean, hint:String )(implicit m:Manifest[T]) : (Any,Any) = {
		val fieldName = jp.getCurrentName
		jp.nextToken
		if( vf.isInstanceOf[ValueClassField] ) {
			val sf = vf.asInstanceOf[ValueClassField]
			val raw = sf.readValue(jp,ext,hint)
			(fieldName, sf.constructor.newInstance(raw.asInstanceOf[Object]))
		} else
			(fieldName, vf.readValue( jp, ext, hint ))
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = {
		// Token now sitting on '{' so advance and read list
		jp.nextToken
		val fieldData = scala.collection.mutable.Map[Any,Any]()
		while( jp.getCurrentToken != JsonToken.END_OBJECT ) 
			fieldData += readMapField( jp, valueField, ext, hint )
		jp.nextToken
		fieldData.toMap
	}
}

case class CaseClassField( name:String, dt:Type, className:String, applyMethod:java.lang.reflect.Method, fields:List[Field], caseObj:Object ) extends Field {
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
	def readClass[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = {
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
}

case class ValueClassField( name:String, override val hasMongoAnno:Boolean, valueType:Field, extJson:Option[ExtJson[_]], constructor:java.lang.reflect.Constructor[_] ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false ) : Boolean = {
		if( ext && extJson.isDefined ) {
			label.fold( {
					sb.append( extJson.get.asInstanceOf[ExtJson[T]].toJson( target ) )
				})((labelStr) => {
					sb.append('"')
					sb.append( labelStr )
					sb.append("\":")
					sb.append( extJson.get.asInstanceOf[ExtJson[T]].toJson( target ) )
					sb.append(',')
				})			
			true
		} else 
			valueType.render( sb, target, label, ext, hint )
	}
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false ) : Any = { 
		target
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = {
		if( ext && extJson.isDefined )
			extJson.get.fromJson(valueType, jp, ext, hint)
		else
			valueType.readValue(jp,ext,hint)
	}
}
