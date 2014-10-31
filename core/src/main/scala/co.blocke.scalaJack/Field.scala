package co.blocke.scalajack

trait Field {
	private[scalajack] val name         : String
	private[scalajack] val hasDBKeyAnno : Boolean = false

	private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false )(implicit m:Manifest[T]) : Boolean = {
		label.fold( {
				sb.append(target)
			})((labelStr) => {
				sb.append('"')
				sb.append( labelStr )
				sb.append("\":")
				sb.append(target)
				sb.append(',')
			})
		true
	}
	private[scalajack] def readValue[T]( jp:JsonEmitter, ext:Boolean, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = { 0 }
}

trait ClassOrTrait {
	private[scalajack] def readClass[T]( jp:JsonEmitter, ext:Boolean, hint:String, fromTrait:Boolean = false )(implicit m:Manifest[T]) : Any
}