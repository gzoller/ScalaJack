package co.blocke.scalajack
package fields

//import org.bson.types.ObjectId

case class ObjectIdField( name:String ) extends Field 

/** 
 * Support for MongoDB's ObjectId data type
 */

/*
case class ObjectIdField( name:String ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false )(implicit m:Manifest[T]) : Boolean = {
		val oid = "{\"$oid\":\""+target.asInstanceOf[ObjectId].toString+"\"}"
		label.fold( {
				sb.append('"')
				sb.append(oid)
				sb.append('"')
			})((labelStr) => {
				sb.append('"')
				sb.append( labelStr )
				sb.append("\":"+oid+",")
			})
		true
	}
	override private[scalajack] def readValue[T]( jp:JsonEmitter, ext:Boolean, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
		jp.nextToken
		jp.nextToken
		val v = jp.getValueAsString
		jp.nextToken
		jp.nextToken
		new ObjectId( v )
	}
}
*/