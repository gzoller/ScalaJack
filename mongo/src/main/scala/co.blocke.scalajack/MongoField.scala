package co.blocke.scalajack

import com.mongodb.casbah.Imports._

trait MongoField {
	private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = { 0 }
	private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = { 0 }
}

trait MongoClassOrTrait {
	private[scalajack] def renderClassDB[T]( target:T, hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any
	private[scalajack] def readClassDB[T]( src:DBObject, hint:String )(implicit m:Manifest[T]) : Any
}