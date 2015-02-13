package co.blocke.scalajack

object PrimitiveTypes {
	private val primitiveTypes = List(
		"scala.Int",
		"java.lang.Integer",
		"scala.Boolean",
		"java.lang.String",
		"String",
		"scala.Float",
		"scala.Double",
		"scala.Long",
		"scala.Char",
		"scala.Null",
		"scala.Byte",
		"scala.Short",
		"scala.Any",
		"scala.Enumeration.Value"
		) 

	private val scalaCollections = List(
		"scala.Option",
		"scala.collection.immutable.List",
		"scala.collection.immutable.Map",
		"scala.collection.immutable.Set",
		"scala.collection.Seq"
		)

	def fixPolyCollection( polyName:String ) = scalaCollections.find( cn => polyName.startsWith(cn) ).orElse({
		if( polyName.equals("""scala.collection.immutable.$colon$colon""")) Some("scala.collection.immutable.List") else None
		})

	implicit class CollMaps( val symbol : scala.reflect.runtime.universe.Symbol ) extends AnyVal {
		def isCollection = scalaCollections.contains(symbol.fullName) || symbol.fullName.startsWith("scala.collection")
		def isPrimitive  = primitiveTypes.contains(symbol.asClass.fullName)
		def isPlaceholder( phs:List[String] ) = phs.contains(symbol.name.toString)
	}
}