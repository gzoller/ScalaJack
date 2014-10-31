package co.blocke.scalajack
package fields

import scala.language.implicitConversions
import com.mongodb.casbah.Imports._

import mongo._

object MongoCCF {
	
	implicit class MongoCaseClassField( ccf:CaseClassField ) extends MongoField with MongoClassOrTrait {

		private def getFieldValue[T]( f:Field, target:T ) = {
			val cz = target.getClass
			val targetField = cz.getDeclaredField(f.name)
			targetField.setAccessible(true)
			val ftype = targetField.getType.getName
			targetField.get(target)
		}

		override private[scalajack] def renderClassDB[T]( target:T, hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = {
			val dbo = MongoDBObject()
			if( withHint )
				dbo.put( hint, ccf.dt.typeSymbol.fullName.toString )
			val (keys, rest) = ccf.fields.partition( _.hasDBKeyAnno )
			if( keys.size == 1 )
				dbo.put("_id", keys.head.renderDB(getFieldValue(keys.head,target),None,hint))
			else if( keys.size > 0 ) {
				val keydbo = MongoDBObject()
				keys.foreach( f => keydbo.put(f.name,f.renderDB(getFieldValue(f,target),None,hint) ) )
				dbo.put("_id", keydbo)
			}
			rest.map( oneField => {
				val fval = getFieldValue(oneField,target)
				if( fval != None ) 
					dbo.put( oneField.name, oneField.renderDB(fval, None, hint) ) 
				/* Needed only if we want to render MongoKeys for an embedded object -- don't think so for now.
				if( fval != None ) {
					oneField match {
						case ccf:CaseClassField => dbo.put( oneField.name, ccf.renderClassDB(fval, hint))
						case _ => dbo.put( oneField.name, oneField.renderDB(fval, None, hint) ) 
					}
				}
				*/
			})
			dbo
		}

		override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = {
			val dbo = MongoDBObject()
			val cz = target.getClass
			if( withHint )
				dbo.put( hint, ccf.dt.typeSymbol.fullName.toString )
			ccf.fields.map( oneField => {
				val targetField = cz.getDeclaredField(oneField.name)
				targetField.setAccessible(true)
				val ftype = targetField.getType.getName
				val fval = targetField.get(target)
				if( fval != None ) 
					dbo.put( oneField.name, oneField.renderDB(fval, None, hint) ) 
			})
			dbo
		}

		override private[scalajack] def readClassDB[T]( src:DBObject, hint:String )(implicit m:Manifest[T]) : Any = {
			val fieldData = scala.collection.mutable.Map[String,Any]()
			val cc = ClassContext(ccf.className,"")
			ccf.fields.map( oneField => {
				cc.fieldName = oneField.name
				val fd = ( oneField.name, {
					if( src.containsField(oneField.name) )
						oneField.readValueDB( src.get(oneField.name), hint, cc ) 
					else if( src.containsField("_id") && oneField.hasDBKeyAnno ) {
						val sval = src.get("_id")
						if( sval.isInstanceOf[java.util.Map[_,_]] ) 
							oneField.readValueDB( sval.asInstanceOf[java.util.Map[String,_]].get(oneField.name), hint, cc )
						else 
							oneField.readValueDB( sval, hint, cc )
					} else None
					})
				fieldData += fd
			})	
			ScalaJack.poof( ccf, fieldData.toMap )				
		}

		override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
			readClassDB( src.asInstanceOf[DBObject], hint )
		}
	}
}
