package co.blocke.scalajack

import org.joda.time.{DateTime,DateTimeZone}

object PrimitiveTypes {
	// Map of [class_name -> builder_fn]
	private[scalajack] val primitiveTypes = Map(
		"scala.Int"                 -> { (s:String) => s.toInt },
		"java.lang.Integer"         -> { (s:String) => java.lang.Integer.parseInt(s) }, 
		"scala.Boolean"             -> { (s:String) => s.toBoolean },
		"java.lang.String"          -> { (s:String) => s },
		"String"                    -> { (s:String) => s },
		"scala.Float"               -> { (s:String) => s.toFloat },
		"scala.Double"              -> { (s:String) => s.toDouble },
		"scala.Long"                -> { (s:String) => s.toLong },
		"scala.Char"                -> { (s:String) => if( !s.isEmpty ) s.charAt(0) else null },
		"scala.Byte"                -> { (s:String) => s.toByte },
		"scala.Short"               -> { (s:String) => s.toShort },
		"java.lang.Boolean"         -> { (s:String) => java.lang.Boolean.parseBoolean(s) },
		"java.util.UUID"            -> { (s:String) => java.util.UUID.fromString(s) },
		"org.joda.time.DateTime"    -> { (s:String) => (new DateTime(s.toLong)).toDateTime(DateTimeZone.forID("UTC")) },
		"scala.Any"					-> { (s:String) => s.toString }  // Not really used!  JSON code has type inference
		) 
	// Any type not supported -- to loose; Render is fine but unable to figure out what the "real" type is upon read.
		// "scala.Null"                -> { (s:String) => null },

	// Map of [class_name -> builder_fn]
	private[scalajack] val scalaCollections = Map(
		"scala.Option"                               -> { (a:Any) => Some(a) },
		"scala.collection.immutable.List"            -> { (a:Any) => scala.collection.immutable.List(a.asInstanceOf[Seq[_]]:_*)            },
		"scala.collection.immutable.Map"             -> { (a:Any) => scala.collection.immutable.Map(a.asInstanceOf[Seq[(_,_)]]:_*)         },
		"scala.collection.immutable.Set"             -> { (a:Any) => scala.collection.immutable.Set(a.asInstanceOf[Seq[_]]:_*)             }, 
		"scala.collection.immutable.HashMap"         -> { (a:Any) => scala.collection.immutable.HashMap(a.asInstanceOf[Seq[(_,_)]]:_*)     },
		"scala.collection.immutable.HashSet"         -> { (a:Any) => scala.collection.immutable.HashSet(a.asInstanceOf[Seq[_]]:_*)         },
		"scala.collection.immutable.ListMap"         -> { (a:Any) => scala.collection.immutable.ListMap(a.asInstanceOf[Seq[(_,_)]]:_*)     },
		"scala.collection.immutable.ListSet"         -> { (a:Any) => scala.collection.immutable.ListSet(a.asInstanceOf[Seq[_]]:_*)         },
		"scala.collection.immutable.Queue"           -> { (a:Any) => scala.collection.immutable.Queue(a.asInstanceOf[Seq[_]]:_*)           },
		"scala.collection.immutable.Seq"             -> { (a:Any) => a.asInstanceOf[Seq[_]]                                                },
		"scala.collection.immutable.Vector"          -> { (a:Any) => scala.collection.immutable.Vector(a.asInstanceOf[Seq[_]]:_*)          },
		"scala.collection.mutable.ArrayBuffer"       -> { (a:Any) => scala.collection.mutable.ArrayBuffer(a.asInstanceOf[Seq[_]]:_*)       },
		"scala.collection.mutable.ArraySeq"          -> { (a:Any) => scala.collection.mutable.ArraySeq(a.asInstanceOf[Seq[_]]:_*)          },
		"scala.collection.mutable.HashMap"           -> { (a:Any) => scala.collection.mutable.HashMap(a.asInstanceOf[Seq[(_,_)]]:_*)       },
		"scala.collection.mutable.HashSet"           -> { (a:Any) => scala.collection.mutable.HashSet(a.asInstanceOf[Seq[_]]:_*)           },
		"scala.collection.mutable.IndexedSeq"        -> { (a:Any) => scala.collection.mutable.IndexedSeq(a.asInstanceOf[Seq[_]]:_*)        },
		"scala.collection.mutable.LinearSeq"         -> { (a:Any) => scala.collection.mutable.LinearSeq(a.asInstanceOf[Seq[_]]:_*)         },
		"scala.collection.mutable.LinkedHashMap"     -> { (a:Any) => scala.collection.mutable.LinkedHashMap(a.asInstanceOf[Seq[(_,_)]]:_*) },
		"scala.collection.mutable.LinkedHashSet"     -> { (a:Any) => scala.collection.mutable.LinkedHashSet(a.asInstanceOf[Seq[_]]:_*)     },
		"scala.collection.mutable.ListBuffer"        -> { (a:Any) => scala.collection.mutable.ListBuffer(a.asInstanceOf[Seq[_]]:_*)        },
		"scala.collection.mutable.ListMap"           -> { (a:Any) => scala.collection.mutable.ListMap(a.asInstanceOf[Seq[(_,_)]]:_*)       },
		"scala.collection.mutable.Map"               -> { (a:Any) => scala.collection.mutable.Map(a.asInstanceOf[Seq[(_,_)]]:_*)           },
		"scala.collection.mutable.MutableList"       -> { (a:Any) => scala.collection.mutable.MutableList(a.asInstanceOf[Seq[_]]:_*)       },
		"scala.collection.mutable.OpenHashMap"       -> { (a:Any) => scala.collection.mutable.OpenHashMap(a.asInstanceOf[Seq[(_,_)]]:_*)   },
		"scala.collection.mutable.Queue"             -> { (a:Any) => scala.collection.mutable.Queue(a.asInstanceOf[Seq[_]]:_*)             },
		"scala.collection.mutable.ResizableArray"    -> { (a:Any) => scala.collection.mutable.ResizableArray(a.asInstanceOf[Seq[_]]:_*)    },
		"scala.collection.mutable.Seq"               -> { (a:Any) => scala.collection.mutable.Seq(a.asInstanceOf[Seq[_]]:_*)               },
		"scala.collection.mutable.Set"               -> { (a:Any) => scala.collection.mutable.Set(a.asInstanceOf[Seq[_]]:_*)               },
		"scala.collection.mutable.Stack"             -> { (a:Any) => scala.collection.mutable.Stack(a.asInstanceOf[Seq[_]]:_*)             },
		"scala.collection.mutable.WeakHashMap"       -> { (a:Any) => scala.collection.mutable.WeakHashMap(a.asInstanceOf[Seq[(_,_)]]:_*)   }
		)
	// The following collections are not supported because there's no way to infer the implicit Ordering:
		// "scala.collection.immutable.TreeMap"
		// "scala.collection.immutable.TreeSet"
		// "scala.collection.mutable.PriorityQueue"
		// "scala.collection.mutable.SortedSet"
		// "scala.collection.mutable.TreeSet"

	implicit class CollMaps( val symbol : scala.reflect.runtime.universe.Symbol ) extends AnyVal {
		def isCollection = scalaCollections.contains(symbol.fullName) || symbol.fullName.startsWith("scala.collection")
		def isPrimitive  = primitiveTypes.contains(symbol.asClass.fullName)
	}
}