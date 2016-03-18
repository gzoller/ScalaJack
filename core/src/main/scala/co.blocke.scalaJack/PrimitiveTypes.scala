package co.blocke.scalajack

import org.joda.time.{DateTime,DateTimeZone}

object PrimitiveTypes {
	private[scalajack] val primTypes = Map(
		1   -> { (s:String) => s.toInt },
		2   -> { (s:String) => java.lang.Integer.parseInt(s) }, 
		3   -> { (s:String) => s.toBoolean },
		4   -> { (s:String) => s },
		5   -> { (s:String) => s },
		6   -> { (s:String) => s.toFloat },
		7   -> { (s:String) => s.toDouble },
		8   -> { (s:String) => s.toLong },
		9   -> { (s:String) => if( !s.isEmpty ) s.charAt(0) else null },
		10  -> { (s:String) => s.toByte },
		11  -> { (s:String) => s.toShort },
		12  -> { (s:String) => java.lang.Boolean.parseBoolean(s) },
		13  -> { (s:String) => java.util.UUID.fromString(s) },
		14  -> { (s:String) => (new DateTime(s.toLong)).toDateTime(DateTimeZone.forID("UTC")) },
		15	-> { (s:String) => s.toString }  // Not really used!  JSON code has type inference
		) 

	private[scalajack] val primCodes = Map(
		"scala.Int"                                  -> 1,
		"java.lang.Integer"                          -> 2, 
		"scala.Boolean"                              -> 3,
		"java.lang.String"                           -> 4,
		"String"                                     -> 5,
		"scala.Float"                                -> 6,
		"scala.Double"                               -> 7,
		"scala.Long"                                 -> 8,
		"scala.Char"                                 -> 9,
		"scala.Byte"                                 -> 10,
		"scala.Short"                                -> 11,
		"java.lang.Boolean"                          -> 12,
		"java.util.UUID"                             -> 13,
		"org.joda.time.DateTime"                     -> 14,
		"scala.Any"					                 -> 15
		)
	private[scalajack] val collCodes = Map(
		"scala.Option"                               -> 16,
		"scala.collection.immutable.List"            -> 17,
		"scala.collection.immutable.Map"             -> 18,
		"scala.collection.immutable.Set"             -> 19, 
		"scala.collection.immutable.HashMap"         -> 20,
		"scala.collection.immutable.HashSet"         -> 21,
		"scala.collection.immutable.ListMap"         -> 22,
		"scala.collection.immutable.ListSet"         -> 23,
		"scala.collection.immutable.Queue"           -> 24,
		"scala.collection.immutable.Seq"             -> 25,
		"scala.collection.immutable.Vector"          -> 26,
		"scala.collection.mutable.ArrayBuffer"       -> 27,
		"scala.collection.mutable.ArraySeq"          -> 28,
		"scala.collection.mutable.HashMap"           -> 29,
		"scala.collection.mutable.HashSet"           -> 30,
		"scala.collection.mutable.IndexedSeq"        -> 31,
		"scala.collection.mutable.LinearSeq"         -> 32,
		"scala.collection.mutable.LinkedHashMap"     -> 33,
		"scala.collection.mutable.LinkedHashSet"     -> 34,
		"scala.collection.mutable.ListBuffer"        -> 35,
		"scala.collection.mutable.ListMap"           -> 36,
		"scala.collection.mutable.Map"               -> 37,
		"scala.collection.mutable.MutableList"       -> 38,
		"scala.collection.mutable.OpenHashMap"       -> 39,
		"scala.collection.mutable.Queue"             -> 40,
		"scala.collection.mutable.ResizableArray"    -> 41,
		"scala.collection.mutable.Seq"               -> 42,
		"scala.collection.mutable.Set"               -> 43,
		"scala.collection.mutable.Stack"             -> 44,
		"scala.collection.mutable.WeakHashMap"       -> 45,
		"scala.Tuple2"                               -> 46,
		"scala.Tuple3"                               -> 47,
		"scala.Tuple4"                               -> 48,
		"scala.Tuple5"                               -> 49,
		"scala.Tuple6"                               -> 50,
		"scala.Tuple7"                               -> 51,
		"scala.Tuple8"                               -> 52,
		"scala.Tuple9"                               -> 53,
		"scala.Tuple10"                              -> 54,
		"scala.Tuple11"                              -> 55,
		"scala.Tuple12"                              -> 56,
		"scala.Tuple13"                              -> 57,
		"scala.Tuple14"                              -> 58,
		"scala.Tuple15"                              -> 59,
		"scala.Tuple16"                              -> 60,
		"scala.Tuple17"                              -> 61,
		"scala.Tuple18"                              -> 62,
		"scala.Tuple19"                              -> 63,
		"scala.Tuple20"                              -> 64,
		"scala.Tuple21"                              -> 65,
		"scala.Tuple22"                              -> 66
		) 

	// Map of [class_name -> builder_fn]
	private[scalajack] val collTypes = Map(
		16 -> { (a:Any) => Some(a) },
		17 -> { (a:Any) => scala.collection.immutable.List(a.asInstanceOf[Seq[_]]:_*)            },
		18 -> { (a:Any) => scala.collection.immutable.Map(a.asInstanceOf[Seq[(_,_)]]:_*)         },
		19 -> { (a:Any) => scala.collection.immutable.Set(a.asInstanceOf[Seq[_]]:_*)             }, 
		20 -> { (a:Any) => scala.collection.immutable.HashMap(a.asInstanceOf[Seq[(_,_)]]:_*)     },
		21 -> { (a:Any) => scala.collection.immutable.HashSet(a.asInstanceOf[Seq[_]]:_*)         },
		22 -> { (a:Any) => scala.collection.immutable.ListMap(a.asInstanceOf[Seq[(_,_)]]:_*)     },
		23 -> { (a:Any) => scala.collection.immutable.ListSet(a.asInstanceOf[Seq[_]]:_*)         },
		24 -> { (a:Any) => scala.collection.immutable.Queue(a.asInstanceOf[Seq[_]]:_*)           },
		25 -> { (a:Any) => a.asInstanceOf[Seq[_]]                                                },
		26 -> { (a:Any) => scala.collection.immutable.Vector(a.asInstanceOf[Seq[_]]:_*)          },
		27 -> { (a:Any) => scala.collection.mutable.ArrayBuffer(a.asInstanceOf[Seq[_]]:_*)       },
		28 -> { (a:Any) => scala.collection.mutable.ArraySeq(a.asInstanceOf[Seq[_]]:_*)          },
		29 -> { (a:Any) => scala.collection.mutable.HashMap(a.asInstanceOf[Seq[(_,_)]]:_*)       },
		30 -> { (a:Any) => scala.collection.mutable.HashSet(a.asInstanceOf[Seq[_]]:_*)           },
		31 -> { (a:Any) => scala.collection.mutable.IndexedSeq(a.asInstanceOf[Seq[_]]:_*)        },
		32 -> { (a:Any) => scala.collection.mutable.LinearSeq(a.asInstanceOf[Seq[_]]:_*)         },
		33 -> { (a:Any) => scala.collection.mutable.LinkedHashMap(a.asInstanceOf[Seq[(_,_)]]:_*) },
		34 -> { (a:Any) => scala.collection.mutable.LinkedHashSet(a.asInstanceOf[Seq[_]]:_*)     },
		35 -> { (a:Any) => scala.collection.mutable.ListBuffer(a.asInstanceOf[Seq[_]]:_*)        },
		36 -> { (a:Any) => scala.collection.mutable.ListMap(a.asInstanceOf[Seq[(_,_)]]:_*)       },
		37 -> { (a:Any) => scala.collection.mutable.Map(a.asInstanceOf[Seq[(_,_)]]:_*)           },
		38 -> { (a:Any) => scala.collection.mutable.MutableList(a.asInstanceOf[Seq[_]]:_*)       },
		39 -> { (a:Any) => scala.collection.mutable.OpenHashMap(a.asInstanceOf[Seq[(_,_)]]:_*)   },
		40 -> { (a:Any) => scala.collection.mutable.Queue(a.asInstanceOf[Seq[_]]:_*)             },
		41 -> { (a:Any) => scala.collection.mutable.ResizableArray(a.asInstanceOf[Seq[_]]:_*)    },
		42 -> { (a:Any) => scala.collection.mutable.Seq(a.asInstanceOf[Seq[_]]:_*)               },
		43 -> { (a:Any) => scala.collection.mutable.Set(a.asInstanceOf[Seq[_]]:_*)               },
		44 -> { (a:Any) => scala.collection.mutable.Stack(a.asInstanceOf[Seq[_]]:_*)             },
		45 -> { (a:Any) => scala.collection.mutable.WeakHashMap(a.asInstanceOf[Seq[(_,_)]]:_*)   },
		46 -> { (a:Any) => { val t = a.asInstanceOf[Tuple2[_,_]]; (t._1,t._2)                   }},
		47 -> { (a:Any) => { val t = a.asInstanceOf[Tuple3[_,_,_]]; (t._1,t._2,t._3)              }},
		48 -> { (a:Any) => { val t = a.asInstanceOf[Tuple4[_,_,_,_]]; (t._1,t._2,t._3,t._4)         }},
		49 -> { (a:Any) => { val t = a.asInstanceOf[Tuple5[_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5)    }},
		50 -> { (a:Any) => { val t = a.asInstanceOf[Tuple6[_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6) }},
		51 -> { (a:Any) => { val t = a.asInstanceOf[Tuple7[_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._6) }},
		52 -> { (a:Any) => { val t = a.asInstanceOf[Tuple8[_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8) }},
		53 -> { (a:Any) => { val t = a.asInstanceOf[Tuple9[_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9) }},
		54 -> { (a:Any) => { val t = a.asInstanceOf[Tuple10[_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10) }},
		55 -> { (a:Any) => { val t = a.asInstanceOf[Tuple11[_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11) }},
		56 -> { (a:Any) => { val t = a.asInstanceOf[Tuple12[_,_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12) }},
		57 -> { (a:Any) => { val t = a.asInstanceOf[Tuple13[_,_,_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13) }},
		58 -> { (a:Any) => { val t = a.asInstanceOf[Tuple14[_,_,_,_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14) }},
		59 -> { (a:Any) => { val t = a.asInstanceOf[Tuple15[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15) }},
		60 -> { (a:Any) => { val t = a.asInstanceOf[Tuple16[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15,t._16) }},
		61 -> { (a:Any) => { val t = a.asInstanceOf[Tuple17[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15,t._16,t._17) }},
		62 -> { (a:Any) => { val t = a.asInstanceOf[Tuple18[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15,t._16,t._17,t._18) }},
		63 -> { (a:Any) => { val t = a.asInstanceOf[Tuple19[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15,t._16,t._17,t._18,t._19) }},
		64 -> { (a:Any) => { val t = a.asInstanceOf[Tuple20[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15,t._16,t._17,t._18,t._19,t._20) }},
		65 -> { (a:Any) => { val t = a.asInstanceOf[Tuple21[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15,t._16,t._17,t._18,t._19,t._20,t._21) }},
		66 -> { (a:Any) => { val t = a.asInstanceOf[Tuple22[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15,t._16,t._17,t._18,t._19,t._20,t._21,t._22) }}
		)

	// The following collections are not supported because there's no way to infer the implicit Ordering:
		// "scala.collection.immutable.TreeMap"
		// "scala.collection.immutable.TreeSet"
		// "scala.collection.mutable.PriorityQueue"
		// "scala.collection.mutable.SortedSet"
		// "scala.collection.mutable.TreeSet"

	implicit class CollMaps( val symbol : scala.reflect.runtime.universe.Symbol ) extends AnyVal {
		def isCollection = collCodes.contains(symbol.fullName) || symbol.fullName.startsWith("scala.collection")
		def isPrimitive  = primCodes.contains(symbol.asClass.fullName)
	}
}