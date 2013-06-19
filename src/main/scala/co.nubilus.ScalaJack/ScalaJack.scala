package co.nubilus.scalajack

/**
 * Assumptions:
 *   -- Case classes only
 *   -- Options of value None are removed from generated JSON
 *   -- Default parameters are not supported at this time
 *   -- Simple types supported:
 *         Int, Boolean, Long, String, Enumeration.Value
 *   -- Collections/"containers" supported:
 *         List (mutable (ListBuffer) and immutable)
 *         Map  (mutable and immutable)
 *         Option
 *   -- MongoKey-decorated fields must be type String and map to "_id" (Mongo's default)
 *   
 * Useful references:
 * 		http://stackoverflow.com/questions/17006271/deep-access-of-fields-in-scala-using-runtime-reflection
 */

import com.fasterxml.jackson.core._
import reflect.runtime.universe._
import scala.reflect.NameTransformer._

object ScalaJack {
	type JSON = String
	
	val jsFactory = new JsonFactory();
	jsFactory.enable(JsonParser.Feature.ALLOW_COMMENTS);
	
	def render[T]( target:T, forMongo:Boolean = false )(implicit m:Manifest[T]) : JSON = _gen( 0, target, None, Analyzer(target.getClass.getName) )
	def read[T]( js:JSON )(implicit m:Manifest[T]) : T = {
			val jp = jsFactory.createJsonParser(js)
			jp.nextToken
			_readClass( jp, Analyzer(m.runtimeClass.getName) ).asInstanceOf[T]
	}
	
	private def _gen[T]( level:Int, target:T, label:Option[String], f:Field, withHint:Boolean = false )(implicit m:Manifest[T]) : JSON = {
		f match {
			case bf:BaseField      => {
				val ttype = bf.dt.typeSymbol.fullName.toString
				if( ttype == "java.lang.String" ) label.fold("\""+target+"\"")((label) => {
						val myLabel = { if( bf.hasMongoAnno && level == 1 ) "_id" else label }
						"\""+myLabel+"\":\""+target+"\","
					})
				else if( ttype == "scala.Int" || ttype == "scala.Long" || ttype == "scala.Boolean" ) label.fold(target.toString)((label) => "\""+label+"\":" + target.toString + ",")
				else s"($ttype),"
			}
			case of:OptField       => {
				val optVal = target.asInstanceOf[Option[_]]
				if( optVal == None ) "" else _gen( level+1, optVal.get, label, of.subField)
			}
			case lf:ListField      => {
				val listVal = target.asInstanceOf[Iterable[_]]
				if( listVal.isEmpty ) label.fold("[]")((label) => "\""+label+"\":[],")
				else {
					val items = listVal.map( item => { val anItem = _gen(level+1, item, None, lf.subField); if(anItem == "") "" else anItem + "," })
					val rawContent = items.mkString.reverse.tail.reverse
					val content = { if( rawContent == "" ) rawContent else "["+rawContent+"]" }
					label.fold(content)((label) => if(content != "") "\""+label+"\":"+content+"," else "")
				}
			}
			case mf:MapField       => {
				val mapVal = target.asInstanceOf[Map[_,_]]
				if( mapVal.isEmpty ) label.fold("{}")((label) => "\""+label+"\":{},")
				else {
					val items = mapVal.map( { case (k,v) => {
						val gen = _gen(level+1, v, None, mf.valueField)
						if( gen != "" )
							"\""+k.toString+"\":" + _gen(level+1, v, None, mf.valueField)+","
						else "" // i.e. None value
					}})
					val rawContent = items.mkString.reverse.tail.reverse
					val content = { if( rawContent == "" ) rawContent else "{"+rawContent+"}" }
//					val content ="{"+items.mkString.reverse.tail.reverse+"}"
					label.fold(content)((label) => if(content != "") "\""+label+"\":"+content+"," else "")
				}
			}
			case ef:EnumField      => label.fold("\""+target+"\"")((label) => "\""+label+"\":\""+target+"\",")
			case tf:TraitField     => _gen( level+1, target, label, Analyzer(target.getClass.getName), true)
			case cf:CaseClassField => {
				val cz = target.getClass
				val hint = { if( withHint ) "\"_hint\":\""+cf.dt.typeSymbol.fullName.toString+"\"," else "" }
				val fields = cf.fields.map( oneField => { 
					val targetField = cz.getDeclaredField(oneField.name)
					targetField.setAccessible(true)
					val ftype = targetField.getType.getName
					val fval = targetField.get(target)
					_gen(level+1, fval, Some(oneField.name), oneField)
				}) 
				label.fold("{"+hint+fields.mkString.reverse.tail.reverse+"}")((label) => "\""+label+"\":{"+hint+fields.mkString.reverse.tail.reverse+"},")
			}
		}
	}
	
	//------------------------ Reader ---------------------------
	private def _readClass[T]( jp:JsonParser, f:Field )(implicit m:Manifest[T]) : Any = {
		jp.nextToken  // consume '{'
		val fieldData = scala.collection.mutable.Map[String,Any]()
		f match {
			case cf : TraitField      => {
				// read hint
				jp.nextToken // skip _hint label
				val traitClassName = jp.getValueAsString
				_readClass( jp, Analyzer(traitClassName) )
			}
			case ccf : CaseClassField => {
				while( jp.getCurrentToken != JsonToken.END_OBJECT ) {
					fieldData += _readField(jp,ccf)
					//jp.nextToken
				}
				jp.nextToken
				poof( ccf.className, fieldData.toMap )				
			}
		}
	}

	// In: Token sitting on field name
	// Out: Token sitting one past the field value (ready for the next field or 'end' token consumption)
	private def _readField[T]( jp:JsonParser, ccf:CaseClassField )(implicit m:Manifest[T]) : (String,Any) = {
		val fieldName = {
			val raw = jp.getCurrentName
			if( raw == "_id" ) ccf.fields.find( _.hasMongoAnno ).fold(raw)((f) => f.name)
			else raw
		}
		jp.nextToken // scan to value
		( fieldName, _readValue( jp, ccf.iFields(fieldName) ) )
	}
	
	private def _readMapField[T]( jp:JsonParser, vf:Field )(implicit m:Manifest[T]) : (Any,Any) = {
		val fieldName = jp.getCurrentName
		jp.nextToken
		(fieldName, _readValue( jp, vf ))
	}
	
	// In: Token sitting on field value
	// Out: Token sitting one past the field value (ready for the next field or 'end' token consumption)
	private def _readValue[T]( jp:JsonParser, f:Field )(implicit m:Manifest[T]) : Any = {
		f match {
			case bf:BaseField       => 
				f.dt.typeSymbol.fullName.toString match {
					case "java.lang.String" => { val v = jp.getValueAsString; jp.nextToken; v }
					case "scala.Int"        => { val v = jp.getValueAsInt; jp.nextToken; v }
					case "scala.Long"       => { val v = jp.getValueAsLong; jp.nextToken; v }
					case "scala.Boolean"    => { val v = jp.getValueAsBoolean; jp.nextToken; v }
					case _ => throw new IllegalArgumentException("Unknown/unsupported type "+f.dt.typeSymbol.fullName.toString)
				}
			case ef :EnumField      => { val v = ef.enum.withName(jp.getValueAsString); jp.nextToken; v }
			case of :OptField       => Some(_readValue(jp, of.subField))
			case tf :TraitField     => _readClass( jp, f )
			case ccf:CaseClassField => _readClass( jp, Analyzer(ccf.className) )
			case lf :ListField      => {
				// Token now sitting on '[' so advance and read list
				jp.nextToken
				val fieldData = scala.collection.mutable.ListBuffer[Any]()
				while( jp.getCurrentToken != JsonToken.END_ARRAY ) {
					fieldData += _readValue(jp,lf.subField)
				}
				jp.nextToken
				fieldData.toList
			}
			case mf:MapField        => {
				// Token now sitting on '{' so advance and read list
				jp.nextToken
				val fieldData = scala.collection.mutable.Map[Any,Any]()
				while( jp.getCurrentToken != JsonToken.END_OBJECT ) 
					fieldData += _readMapField( jp, mf.valueField )
				jp.nextToken
				fieldData.toMap
			}
		}
	}

	// Magically create an instance of a case class given a map of name->value parameters.
	// (Reflects on the apply method of the case class' companion object.)
	def poof[T]( data:Map[String,Any] )(implicit m:Manifest[T]) : T = poof( m.runtimeClass.getName, data ).asInstanceOf[T]
		
	private def poof( cname:String, data:Map[String,Any] ) : Any = {
		val classField = Analyzer(cname).asInstanceOf[CaseClassField]
		val args = classField.fields.collect{ case f => data.get(f.name).getOrElse(None) }.toArray.asInstanceOf[Array[AnyRef]]
		classField.applyMethod.invoke( classField.caseObj, args:_* )
	}
}
