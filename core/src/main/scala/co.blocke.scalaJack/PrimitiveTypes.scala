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
		"scala.Enumeration.Value",
		"java.lang.Boolean",
		"java.util.UUID"
		) 

	// IDEA : For reading, could convert this to a map of [class_name -> builder_fn]
	private val scalaCollections = List(
		"scala.Option",
		"scala.collection.immutable.List",
		"scala.collection.immutable.Map",
		"scala.collection.immutable.Set",
		"scala.collection.immutable.HashMap",
		"scala.collection.immutable.HashSet",
		"scala.collection.immutable.ListMap",
		"scala.collection.immutable.ListSet",
		"scala.collection.immutable.Queue",
		"scala.collection.immutable.Seq",
		"scala.collection.immutable.TreeMap",
		"scala.collection.immutable.TreeSet",
		"scala.collection.immutable.Vector",
		"scala.collection.mutable.ArrayBuffer",
		"scala.collection.mutable.ArraySeq",
		"scala.collection.mutable.DoublyLinkedList",
		"scala.collection.mutable.HashMap",
		"scala.collection.mutable.HashSet",
		"scala.collection.mutable.IndexedSeq",
		"scala.collection.mutable.LinearSeq",
		"scala.collection.mutable.LinkedHashMap",
		"scala.collection.mutable.LinkedHashSet",
		"scala.collection.mutable.LinkedList",
		"scala.collection.mutable.ListBuffer",
		"scala.collection.mutable.ListMap",
		"scala.collection.mutable.Map",
		"scala.collection.mutable.MutableList",
		"scala.collection.mutable.OpenHashMap",
		"scala.collection.mutable.PriorityQueue",
		"scala.collection.mutable.Queue",
		"scala.collection.mutable.ResizableArray",
		"scala.collection.mutable.Seq",
		"scala.collection.mutable.Set",
		"scala.collection.mutable.SortedSet",
		"scala.collection.mutable.Stack",
		"scala.collection.mutable.TreeSet",
		"scala.collection.mutable.Seq",
		"scala.collection.mutable.WeakHashMap",
		"scala.collection.mutable.Seq"
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