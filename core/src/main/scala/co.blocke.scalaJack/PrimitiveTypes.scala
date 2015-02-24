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
	private val scalaCollections = Map(
		"scala.Option"                               -> { (a:Any) => Some(a) },
		"scala.collection.immutable.List"            -> { (a:Any) => scala.collection.immutable.List(a.asInstanceOf[Seq[_]]:_*) },
		"scala.collection.immutable.Map"             -> { (a:Any) => scala.collection.immutable.Map(a.asInstanceOf[Seq[(_,_)]]:_*) },
		"scala.collection.immutable.Set"             -> { (a:Any) => scala.collection.immutable.Set(a.asInstanceOf[Seq[_]]:_*) }, 
		"scala.collection.immutable.HashMap"         -> { (a:Any) => scala.collection.immutable.HashMap(a.asInstanceOf[Seq[(_,_)]]:_*) },
		"scala.collection.immutable.HashSet"         -> { (a:Any) => scala.collection.immutable.HashSet(a.asInstanceOf[Seq[_]]:_*) },
		"scala.collection.immutable.ListMap"         -> { (a:Any) => scala.collection.immutable.ListMap(a.asInstanceOf[Seq[(_,_)]]:_*) },
		"scala.collection.immutable.ListSet"         -> { (a:Any) => scala.collection.immutable.ListSet(a.asInstanceOf[Seq[_]]:_*) },
		"scala.collection.immutable.Queue"           -> { (a:Any) => scala.collection.immutable.Queue(a.asInstanceOf[Seq[_]]:_*) },
		"scala.collection.immutable.Seq"             -> { (a:Any) => a.asInstanceOf[Seq[_]] },
		"scala.collection.immutable.Vector"          -> { (a:Any) => scala.collection.immutable.Vector(a.asInstanceOf[Seq[_]]:_*) },
		"scala.collection.mutable.ArrayBuffer"       -> { (a:Any) => scala.collection.mutable.ArrayBuffer(a.asInstanceOf[Seq[_]]:_*) },
		"scala.collection.mutable.ArraySeq"          -> { (a:Any) => scala.collection.mutable.ArraySeq(a.asInstanceOf[Seq[_]]:_*) },
		"scala.collection.mutable.HashMap"           -> { (a:Any) => scala.collection.mutable.HashMap(a.asInstanceOf[Seq[(_,_)]]:_*) },
		"scala.collection.mutable.HashSet"           -> { (a:Any) => scala.collection.mutable.HashSet(a.asInstanceOf[Seq[_]]:_*) },
		"scala.collection.mutable.IndexedSeq"        -> { (a:Any) => scala.collection.mutable.IndexedSeq(a.asInstanceOf[Seq[_]]:_*) },
		"scala.collection.mutable.LinearSeq"         -> { (a:Any) => scala.collection.mutable.LinearSeq(a.asInstanceOf[Seq[_]]:_*) },
		"scala.collection.mutable.LinkedHashMap"     -> { (a:Any) => scala.collection.mutable.LinkedHashMap(a.asInstanceOf[Seq[(_,_)]]:_*) },
		"scala.collection.mutable.LinkedHashSet"     -> { (a:Any) => scala.collection.mutable.LinkedHashSet(a.asInstanceOf[Seq[_]]:_*) },
		"scala.collection.mutable.ListBuffer"        -> { (a:Any) => scala.collection.mutable.ListBuffer(a.asInstanceOf[Seq[_]]:_*) },
		"scala.collection.mutable.ListMap"           -> { (a:Any) => scala.collection.mutable.ListMap(a.asInstanceOf[Seq[(_,_)]]:_*) },
		"scala.collection.mutable.Map"               -> { (a:Any) => scala.collection.mutable.Map(a.asInstanceOf[Seq[(_,_)]]:_*) },
		"scala.collection.mutable.MutableList"       -> { (a:Any) => scala.collection.mutable.MutableList(a.asInstanceOf[Seq[_]]:_*) },
		"scala.collection.mutable.OpenHashMap"       -> { (a:Any) => scala.collection.mutable.OpenHashMap(a.asInstanceOf[Seq[(_,_)]]:_*) },
		"scala.collection.mutable.Queue"             -> { (a:Any) => scala.collection.mutable.Queue(a.asInstanceOf[Seq[_]]:_*) },
		"scala.collection.mutable.ResizableArray"    -> { (a:Any) => scala.collection.mutable.ResizableArray(a.asInstanceOf[Seq[_]]:_*) },
		"scala.collection.mutable.Seq"               -> { (a:Any) => scala.collection.mutable.Seq(a.asInstanceOf[Seq[_]]:_*) },
		"scala.collection.mutable.Set"               -> { (a:Any) => scala.collection.mutable.Set(a.asInstanceOf[Seq[_]]:_*) },
		"scala.collection.mutable.Stack"             -> { (a:Any) => scala.collection.mutable.Stack(a.asInstanceOf[Seq[_]]:_*) },
		"scala.collection.mutable.WeakHashMap"       -> { (a:Any) => scala.collection.mutable.WeakHashMap(a.asInstanceOf[Seq[(_,_)]]:_*) }
		)
	// The following collections are not supported because there's no way to infer the implicit Ordering:
		// "scala.collection.immutable.TreeMap"
		// "scala.collection.immutable.TreeSet"
		// "scala.collection.mutable.PriorityQueue"
		// "scala.collection.mutable.SortedSet"
		// "scala.collection.mutable.TreeSet"

	def fixPolyCollection( polyName:String ) = scalaCollections.keySet.find( cn => polyName.startsWith(cn) ).orElse({
		if( polyName.equals("""scala.collection.immutable.$colon$colon""")) Some("scala.collection.immutable.List") else None
		})

	implicit class CollMaps( val symbol : scala.reflect.runtime.universe.Symbol ) extends AnyVal {
		def isCollection = scalaCollections.contains(symbol.fullName) || symbol.fullName.startsWith("scala.collection")
		def isPrimitive  = primitiveTypes.contains(symbol.asClass.fullName)
		def isPlaceholder( phs:List[String] ) = phs.contains(symbol.name.toString)
	}
}