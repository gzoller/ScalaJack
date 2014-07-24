package co.blocke.scalajack
package fields

import com.fasterxml.jackson.core._
import scala.util.Try

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
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = {
		target.toString
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
		if( jp.getCurrentToken != JsonToken.VALUE_STRING && jp.getCurrentToken != JsonToken.VALUE_NULL ) throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_STRING and saw "+jp.getCurrentToken)
		val v = jp.getValueAsString
		jp.nextToken
		v
	}
	override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = 
		Try(src.toString).toOption.getOrElse(throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_STRING and saw "+src.getClass.getName))
}

case class IntField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String, cc:ClassContext)(implicit m:Manifest[T]) : Any = {
		if( jp.getCurrentToken != JsonToken.VALUE_NUMBER_INT && jp.getCurrentToken != JsonToken.VALUE_NULL ) throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_NUMBER_INT and saw "+jp.getCurrentToken)
		val v = jp.getValueAsInt
		jp.nextToken
		v
	}
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = {
		target.asInstanceOf[Int]
	}
	override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = 
		Try(src.asInstanceOf[Int]).toOption.getOrElse(throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_NUMBER_INT and saw "+src.getClass.getName))
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
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = {
		target.asInstanceOf[Char]
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
		if( jp.getCurrentToken != JsonToken.VALUE_STRING) throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_CHAR and saw "+jp.getCurrentToken)
		val v = jp.getValueAsString.charAt(0)
		jp.nextToken
		v
	}
	override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = 
		Try(src.asInstanceOf[Char]).toOption.getOrElse(throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_CHAR and saw "+src.getClass.getName))
}

case class LongField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
		if( jp.getCurrentToken != JsonToken.VALUE_NUMBER_INT && jp.getCurrentToken != JsonToken.VALUE_NULL ) throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_NUMBER_INT and saw "+jp.getCurrentToken)
		val v = jp.getValueAsLong
		jp.nextToken
		v
	}
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = {
		target.asInstanceOf[Long]
	}
	override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = 
		Try(src.asInstanceOf[Long]).toOption.getOrElse(throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_NUMBER_INT and saw "+src.getClass.getName))
}

case class FloatField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
		if( jp.getCurrentToken != JsonToken.VALUE_NUMBER_FLOAT && jp.getCurrentToken != JsonToken.VALUE_NULL ) throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_FLOAT and saw "+jp.getCurrentToken)
		val v = jp.getValueAsDouble.toFloat
		jp.nextToken
		v
	}
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = {
		target.asInstanceOf[Float]
	}
	override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = 
		Try(src.asInstanceOf[Float]).toOption.getOrElse(throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_FLOAT and saw "+src.getClass.getName))
}

case class DoubleField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
		if( jp.getCurrentToken != JsonToken.VALUE_NUMBER_FLOAT && jp.getCurrentToken != JsonToken.VALUE_NULL ) throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_FLOAT and saw "+jp.getCurrentToken)
		val v = jp.getValueAsDouble
		jp.nextToken
		v
	}
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = {
		target.asInstanceOf[Double]
	}
	override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]): Any = 
		Try(src.asInstanceOf[Double]).toOption.getOrElse(throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_FLOAT and saw "+src.getClass.getName))
}

case class BoolField( name:String, override val hasMongoAnno:Boolean ) extends Field {
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
		if( jp.getCurrentToken != JsonToken.VALUE_TRUE && jp.getCurrentToken != JsonToken.VALUE_FALSE && jp.getCurrentToken != JsonToken.VALUE_NULL ) throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected boolean and saw "+jp.getCurrentToken)
		val v = jp.getValueAsBoolean
		jp.nextToken
		v
	}
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = {
		target.asInstanceOf[Boolean]
	}
	override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = 
		Try(src.asInstanceOf[Boolean]).toOption.getOrElse(throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected boolean and saw "+src.getClass.getName))
}
