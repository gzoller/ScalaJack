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
		"scala.Unit",
		"scala.Null",
		"scala.Any",
		"scala.Byte",
		"scala.Short"
		) 

	private val scalaCollections = List(
		"scala.Option",
		"scala.collection.immutable.List",
		"scala.collection.immutable.Map",
		"scala.collection.immutable.Set",
		"scala.collection.Seq"
		)

	implicit class CollMaps( val symbol : scala.reflect.runtime.universe.Symbol ) extends AnyVal {
		def isCollection = scalaCollections.contains(symbol.fullName)
		def isPrimitive  = primitiveTypes.contains(symbol.asClass.fullName)
		def isPlaceholder( phs:List[String] ) = phs.contains(symbol.name.toString)
	}
}