package co.blocke.scalajack
package fields

import com.fasterxml.jackson.core._
import scala.language.existentials // compiler-recommended include
import java.lang.reflect.Constructor

import mongo._

// This is rough!  The target value could be _either_ a value class or a base type, because Scala stores
// value class objects in a container (Option, List, etc.) and raw base types when a "naked" value, like a field in a
// case class.  Horrors!

object MongoValueClass {

	implicit class MongoValueClassFieldUnboxed( f:ValueClassFieldUnboxed ) extends MongoField {
		override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = 
			f.valueType.renderDB( target, label, hint )

		override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = 
			f.valueType.readValueDB(src,hint,cc)
	}

	implicit class MongoValueClassField( f:ValueClassField ) extends MongoField {
		override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = { 
			val valueFieldName = target.getClass.getDeclaredFields.head.getName
			val value = target.getClass.getDeclaredMethods.toList.find(m => m.getName == valueFieldName).get.invoke(target)
			f.valueType.renderDB( value, label, hint )
		}
		override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = 
			f.constructor.newInstance( f.valueType.readValueDB(src,hint,cc).asInstanceOf[Object] )
	}
}
