package co.blocke.scalajack
package fields

import com.fasterxml.jackson.core._

object MongoEnum {

	implicit class MongoEnumField( f:EnumField ) extends MongoField {
		override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = 
			target.toString
		override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = 
			scala.util.Try( f.enum.withName( src.toString ) ).toOption.getOrElse( throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Given value of "+src.toString+" is not valid for this enum field." ) )
	}
}
