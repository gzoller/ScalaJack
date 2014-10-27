package co.blocke.scalajack
package fields

import org.bson.types.ObjectId

object MongoObjectId {

	implicit class MongoObjectIdField( f:ObjectIdField ) extends MongoField {
		override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = 
			target.asInstanceOf[ObjectId]
		override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = src
	}
}
