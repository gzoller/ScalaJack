package co.blocke.scalajack
package fields

import mongo._

object MongoOpt {

	implicit class MongoOptField( f:OptField ) extends MongoField {
		override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = {
			val optVal = target.asInstanceOf[Option[_]]
			if( optVal != None ) f.subField.renderDB( optVal.get, label, hint )
			else optVal
		}
		override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = 
			Some(f.subField.readValueDB(src,hint,cc))
	}
}
