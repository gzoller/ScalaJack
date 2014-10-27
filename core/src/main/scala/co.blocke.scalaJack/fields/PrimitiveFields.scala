package co.blocke.scalajack
package fields

import scala.util.Try
import org.joda.time.DateTime
import com.fasterxml.jackson.core._

case class StringField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false )(implicit m:Manifest[T]) : Boolean = {
		label.fold( {
				sb.append('"')
				sb.append( clean(target.toString) )
				sb.append('"')
			})((labelStr) => {
				sb.append('"')
				sb.append( labelStr )
				sb.append("\":\"")
				sb.append( clean(target.toString) )
				sb.append("\",")
			})
		true
	}
	private def clean( input:String ) = {
		val buffer = new StringBuffer(input.length())
		for ( i <- 0 to input.length-1 ) {
			if ( input.charAt(i) > 256) {
				buffer.append("\\u").append(Integer.toHexString( input.charAt(i)) )
			} else buffer.append( input.charAt(i) match {
				case '\n' => "\\n"
				case '\t' => "\\t"
				case '\r' => "\\r"
				case '\b' => "\\b"
				case '\f' => "\\f"
				//case '\'' => "\\'"   <- Don't need this one.  Commented out for now...for deletion later after "settling"
				case '\"' => "\\\""
				case '\\' => "\\\\"
				case c    => c
        	})
        }
    	buffer.toString
	}
	override private[scalajack] def readValue[T]( jp:JsonEmitter, ext:Boolean, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
		if( jp.getCurrentToken != JsonToken.VALUE_STRING && jp.getCurrentToken != JsonToken.VALUE_NULL ) throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_STRING and saw "+jp.getCurrentToken)
		val v = jp.getValueAsString
		jp.nextToken
		v
	}
}

case class UUIDField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false )(implicit m:Manifest[T]) : Boolean = {
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
	override private[scalajack] def readValue[T]( jp:JsonEmitter, ext:Boolean, hint:String, cc:ClassContext)(implicit m:Manifest[T]) : Any = {
		if( jp.getCurrentToken != JsonToken.VALUE_STRING && jp.getCurrentToken != JsonToken.VALUE_NULL ) throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_NUMBER_INT and saw "+jp.getCurrentToken)
		val v = { if( jp.getCurrentToken == JsonToken.VALUE_NULL) null else java.util.UUID.fromString( jp.getValueAsString ) }
		jp.nextToken
		v
	}
}

case class JodaField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false )(implicit m:Manifest[T]) : Boolean = 
		super.render[Long]( sb, target.asInstanceOf[DateTime].getMillis.asInstanceOf[Long], label, ext, hint, withHint )
	override private[scalajack] def readValue[T]( jp:JsonEmitter, ext:Boolean, hint:String, cc:ClassContext)(implicit m:Manifest[T]) : Any = {
		if( jp.getCurrentToken != JsonToken.VALUE_NUMBER_INT && jp.getCurrentToken != JsonToken.VALUE_NULL ) throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_NUMBER_INT and saw "+jp.getCurrentToken)
		val v = { if( jp.getCurrentToken == JsonToken.VALUE_NULL) null else new DateTime(jp.getValueAsLong) }
		jp.nextToken
		v
	}
}

case class IntField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def readValue[T]( jp:JsonEmitter, ext:Boolean, hint:String, cc:ClassContext)(implicit m:Manifest[T]) : Any = {
		if( jp.getCurrentToken != JsonToken.VALUE_NUMBER_INT && jp.getCurrentToken != JsonToken.VALUE_NULL ) throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_NUMBER_INT and saw "+jp.getCurrentToken)
		val v = jp.getValueAsInt
		jp.nextToken
		v
	}
}

case class CharField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false )(implicit m:Manifest[T]) : Boolean = {
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
	override private[scalajack] def readValue[T]( jp:JsonEmitter, ext:Boolean, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
		if( jp.getCurrentToken != JsonToken.VALUE_STRING) throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_CHAR and saw "+jp.getCurrentToken)
		val v = jp.getValueAsString.charAt(0)
		jp.nextToken
		v
	}
}

case class LongField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def readValue[T]( jp:JsonEmitter, ext:Boolean, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
		if( jp.getCurrentToken != JsonToken.VALUE_NUMBER_INT && jp.getCurrentToken != JsonToken.VALUE_NULL ) throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_NUMBER_INT and saw "+jp.getCurrentToken)
		val v = jp.getValueAsLong
		jp.nextToken
		v
	}
}

case class FloatField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def readValue[T]( jp:JsonEmitter, ext:Boolean, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
		if( jp.getCurrentToken != JsonToken.VALUE_NUMBER_FLOAT && jp.getCurrentToken != JsonToken.VALUE_NULL ) throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_FLOAT and saw "+jp.getCurrentToken)
		val v = jp.getValueAsDouble.toFloat
		jp.nextToken
		v
	}
}

case class DoubleField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def readValue[T]( jp:JsonEmitter, ext:Boolean, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
		if( jp.getCurrentToken != JsonToken.VALUE_NUMBER_FLOAT && jp.getCurrentToken != JsonToken.VALUE_NULL ) throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_FLOAT and saw "+jp.getCurrentToken)
		val v = jp.getValueAsDouble
		jp.nextToken
		v
	}
}

case class BoolField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def readValue[T]( jp:JsonEmitter, ext:Boolean, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
		if( jp.getCurrentToken != JsonToken.VALUE_TRUE && jp.getCurrentToken != JsonToken.VALUE_FALSE && jp.getCurrentToken != JsonToken.VALUE_NULL ) throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected boolean and saw "+jp.getCurrentToken)
		val v = jp.getValueAsBoolean
		jp.nextToken
		v
	}
}
