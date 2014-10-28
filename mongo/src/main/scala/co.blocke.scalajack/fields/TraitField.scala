package co.blocke.scalajack
package fields

import com.fasterxml.jackson.core._
import com.mongodb.casbah.Imports._

import mongo._ 

object MongoTrait {

	implicit class MongoTraitField( f:TraitField ) extends MongoField with MongoClassOrTrait {
		override private[scalajack] def renderClassDB[T]( target:T, hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = {
			val ccf:CaseClassField = Analyzer.inspect(target.getClass.getName, f.typeArgs).asInstanceOf[CaseClassField]
			MongoCCF.MongoCaseClassField(ccf).renderClassDB( target, hint, true )
		}

		override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = 
			Analyzer.inspect(target.getClass.getName, f.typeArgs).renderDB( target, label, hint, true )

		override private[scalajack] def readClassDB[T]( src:DBObject, hint:String )(implicit m:Manifest[T]) : Any = {
			val ccf = Analyzer.inspect( src.get( hint ).asInstanceOf[String], f.typeArgs ).asInstanceOf[CaseClassField]
			MongoCCF.MongoCaseClassField(ccf).readClassDB( src, hint )
		}

		override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
			val ccf = Analyzer.inspect( src.asInstanceOf[DBObject].get( hint ).asInstanceOf[String], f.typeArgs ).asInstanceOf[CaseClassField]
			MongoCCF.MongoCaseClassField(ccf).readClassDB( src.asInstanceOf[DBObject], hint )
		}
	}
}
