package co.blocke.scalajack
package fields

import com.fasterxml.jackson.core._
import scala.util.Try
import org.joda.time.DateTime

object MongoPrimative {

	implicit class MongoStringField( f:StringField ) extends MongoField {
		override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = 
			target.toString
		override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = 
			Try(src.toString).toOption.getOrElse(throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_STRING and saw "+src.getClass.getName))
	}

	implicit class MongoUUIDField( f:UUIDField ) extends MongoField {
		override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = 
			target.toString
		override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
			val v = Try(src.asInstanceOf[String]).toOption.getOrElse(throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_STRING (UUID) and saw "+src.getClass.getName))
			java.util.UUID.fromString(v)
		}
	}

	implicit class MongoJodaField( f:JodaField ) extends MongoField {
		override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = 
			target.asInstanceOf[DateTime].getMillis
		override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
			val v = Try(src.asInstanceOf[Long]).toOption.getOrElse(throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_NUMBER_INT and saw "+src.getClass.getName))
			new DateTime(v)
		}
	}

	implicit class MongoIntField( f:IntField ) extends MongoField {
		override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = 
			target.asInstanceOf[Int]
		override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = 
			Try(src.asInstanceOf[Int]).toOption.getOrElse(throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_NUMBER_INT and saw "+src.getClass.getName))
	}

	implicit class MongoCharField( f:CharField ) extends MongoField {
		override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = 
			target.asInstanceOf[Char]
		override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = 
			Try(src.asInstanceOf[Char]).toOption.getOrElse(throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_CHAR and saw "+src.getClass.getName))
	}

	implicit class MongoLongField( f:LongField ) extends MongoField {
		override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = 
			target.asInstanceOf[Long]
		override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = 
			Try(src.asInstanceOf[Long]).toOption.getOrElse(throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_NUMBER_INT and saw "+src.getClass.getName))
	}

	implicit class MongoFloatField( f:FloatField ) extends MongoField {
		override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = 
			target.asInstanceOf[Float]
		override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = 
			Try(src.asInstanceOf[Float]).toOption.getOrElse(throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_FLOAT and saw "+src.getClass.getName))
	}

	implicit class MongoDoubleField( f:DoubleField ) extends MongoField {
		override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = 
			target.asInstanceOf[Double]
		override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]): Any = 
			Try(src.asInstanceOf[Double]).toOption.getOrElse(throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_FLOAT and saw "+src.getClass.getName))
	}

	implicit class MongoBoolField( f:BoolField ) extends MongoField {
		override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = 
			target.asInstanceOf[Boolean]
		override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = 
			Try(src.asInstanceOf[Boolean]).toOption.getOrElse(throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected boolean and saw "+src.getClass.getName))
	}
}
