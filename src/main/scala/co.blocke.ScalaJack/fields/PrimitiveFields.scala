package co.blocke.scalajack
package fields

import com.fasterxml.jackson.core._

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
	override private[scalajack] def readValueDB[T]( src:Any, hint:String )(implicit m:Manifest[T]) : Any = src.toString
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
	override private[scalajack] def readValueDB[T]( src:Any, hint:String )(implicit m:Manifest[T]) : Any = src.asInstanceOf[Int]
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
	override private[scalajack] def readValueDB[T]( src:Any, hint:String )(implicit m:Manifest[T]) : Any = src.asInstanceOf[Char]
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
	override private[scalajack] def readValueDB[T]( src:Any, hint:String )(implicit m:Manifest[T]) : Any = src.asInstanceOf[Long]
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
	override private[scalajack] def readValueDB[T]( src:Any, hint:String )(implicit m:Manifest[T]) : Any = src.asInstanceOf[Float]
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
	override private[scalajack] def readValueDB[T]( src:Any, hint:String )(implicit m:Manifest[T]) : Any = src.asInstanceOf[Double]
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
	override private[scalajack] def readValueDB[T]( src:Any, hint:String )(implicit m:Manifest[T]) : Any = src.asInstanceOf[Boolean]
}
