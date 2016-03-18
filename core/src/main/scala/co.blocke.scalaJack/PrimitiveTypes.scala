package co.blocke.scalajack

import org.joda.time.{DateTime,DateTimeZone}

object PrimitiveTypes {
	private[scalajack] val primTypes:Map[Byte,(String)=>Any] = Map(
		1.toByte   -> { (s:String) => s.toInt },
		2.toByte   -> { (s:String) => java.lang.Integer.parseInt(s) }, 
		3.toByte   -> { (s:String) => s.toBoolean },
		4.toByte   -> { (s:String) => s },
		5.toByte   -> { (s:String) => s },
		6.toByte   -> { (s:String) => s.toFloat },
		7.toByte   -> { (s:String) => s.toDouble },
		8.toByte   -> { (s:String) => s.toLong },
		9.toByte   -> { (s:String) => if( !s.isEmpty ) s.charAt(0) else null },
		10.toByte  -> { (s:String) => s.toByte },
		11.toByte  -> { (s:String) => s.toShort },
		12.toByte  -> { (s:String) => java.lang.Boolean.parseBoolean(s) },
		13.toByte  -> { (s:String) => java.util.UUID.fromString(s) },
		14.toByte  -> { (s:String) => (new DateTime(s.toLong)).toDateTime(DateTimeZone.forID("UTC")) },
		15.toByte	-> { (s:String) => s.toString }  // Not really used!  JSON code has type inference
		) 

	private[scalajack] val primCodes:Map[String,Byte] = Map(
		"scala.Int"                                  -> 1.toByte,
		"java.lang.Integer"                          -> 2.toByte, 
		"scala.Boolean"                              -> 3.toByte,
		"java.lang.String"                           -> 4.toByte,
		"String"                                     -> 5.toByte,
		"scala.Float"                                -> 6.toByte,
		"scala.Double"                               -> 7.toByte,
		"scala.Long"                                 -> 8.toByte,
		"scala.Char"                                 -> 9.toByte,
		"scala.Byte"                                 -> 10.toByte,
		"scala.Short"                                -> 11.toByte,
		"java.lang.Boolean"                          -> 12.toByte,
		"java.util.UUID"                             -> 13.toByte,
		"org.joda.time.DateTime"                     -> 14.toByte,
		"scala.Any"					                 -> 15.toByte
		)
	private[scalajack] val collCodes:Map[String,Byte] = Map(
		"scala.Option"                               -> 16.toByte,
		"scala.collection.immutable.List"            -> 17.toByte,
		"scala.collection.immutable.Map"             -> 18.toByte,
		"scala.collection.immutable.Set"             -> 19.toByte, 
		"scala.collection.immutable.HashMap"         -> 20.toByte,
		"scala.collection.immutable.HashSet"         -> 21.toByte,
		"scala.collection.immutable.ListMap"         -> 22.toByte,
		"scala.collection.immutable.ListSet"         -> 23.toByte,
		"scala.collection.immutable.Queue"           -> 24.toByte,
		"scala.collection.immutable.Seq"             -> 25.toByte,
		"scala.collection.immutable.Vector"          -> 26.toByte,
		"scala.collection.mutable.ArrayBuffer"       -> 27.toByte,
		"scala.collection.mutable.ArraySeq"          -> 28.toByte,
		"scala.collection.mutable.HashMap"           -> 29.toByte,
		"scala.collection.mutable.HashSet"           -> 30.toByte,
		"scala.collection.mutable.IndexedSeq"        -> 31.toByte,
		"scala.collection.mutable.LinearSeq"         -> 32.toByte,
		"scala.collection.mutable.LinkedHashMap"     -> 33.toByte,
		"scala.collection.mutable.LinkedHashSet"     -> 34.toByte,
		"scala.collection.mutable.ListBuffer"        -> 35.toByte,
		"scala.collection.mutable.ListMap"           -> 36.toByte,
		"scala.collection.mutable.Map"               -> 37.toByte,
		"scala.collection.mutable.MutableList"       -> 38.toByte,
		"scala.collection.mutable.OpenHashMap"       -> 39.toByte,
		"scala.collection.mutable.Queue"             -> 40.toByte,
		"scala.collection.mutable.ResizableArray"    -> 41.toByte,
		"scala.collection.mutable.Seq"               -> 42.toByte,
		"scala.collection.mutable.Set"               -> 43.toByte,
		"scala.collection.mutable.Stack"             -> 44.toByte,
		"scala.collection.mutable.WeakHashMap"       -> 45.toByte,
		"scala.Tuple2"                               -> 46.toByte,
		"scala.Tuple3"                               -> 47.toByte,
		"scala.Tuple4"                               -> 48.toByte,
		"scala.Tuple5"                               -> 49.toByte,
		"scala.Tuple6"                               -> 50.toByte,
		"scala.Tuple7"                               -> 51.toByte,
		"scala.Tuple8"                               -> 52.toByte,
		"scala.Tuple9"                               -> 53.toByte,
		"scala.Tuple10"                              -> 54.toByte,
		"scala.Tuple11"                              -> 55.toByte,
		"scala.Tuple12"                              -> 56.toByte,
		"scala.Tuple13"                              -> 57.toByte,
		"scala.Tuple14"                              -> 58.toByte,
		"scala.Tuple15"                              -> 59.toByte,
		"scala.Tuple16"                              -> 60.toByte,
		"scala.Tuple17"                              -> 61.toByte,
		"scala.Tuple18"                              -> 62.toByte,
		"scala.Tuple19"                              -> 63.toByte,
		"scala.Tuple20"                              -> 64.toByte,
		"scala.Tuple21"                              -> 65.toByte,
		"scala.Tuple22"                              -> 66.toByte
		) 

	// Map of [class_name -> builder_fn]
	private[scalajack] val collTypes:Map[Byte,(Any)=>Any] = Map(
		16.toByte -> { (a:Any) => Some(a) },
		17.toByte -> { (a:Any) => scala.collection.immutable.List(a.asInstanceOf[Seq[_]]:_*)            },
		18.toByte -> { (a:Any) => scala.collection.immutable.Map(a.asInstanceOf[Seq[(_,_)]]:_*)         },
		19.toByte -> { (a:Any) => scala.collection.immutable.Set(a.asInstanceOf[Seq[_]]:_*)             }, 
		20.toByte -> { (a:Any) => scala.collection.immutable.HashMap(a.asInstanceOf[Seq[(_,_)]]:_*)     },
		21.toByte -> { (a:Any) => scala.collection.immutable.HashSet(a.asInstanceOf[Seq[_]]:_*)         },
		22.toByte -> { (a:Any) => scala.collection.immutable.ListMap(a.asInstanceOf[Seq[(_,_)]]:_*)     },
		23.toByte -> { (a:Any) => scala.collection.immutable.ListSet(a.asInstanceOf[Seq[_]]:_*)         },
		24.toByte -> { (a:Any) => scala.collection.immutable.Queue(a.asInstanceOf[Seq[_]]:_*)           },
		25.toByte -> { (a:Any) => a.asInstanceOf[Seq[_]]                                                },
		26.toByte -> { (a:Any) => scala.collection.immutable.Vector(a.asInstanceOf[Seq[_]]:_*)          },
		27.toByte -> { (a:Any) => scala.collection.mutable.ArrayBuffer(a.asInstanceOf[Seq[_]]:_*)       },
		28.toByte -> { (a:Any) => scala.collection.mutable.ArraySeq(a.asInstanceOf[Seq[_]]:_*)          },
		29.toByte -> { (a:Any) => scala.collection.mutable.HashMap(a.asInstanceOf[Seq[(_,_)]]:_*)       },
		30.toByte -> { (a:Any) => scala.collection.mutable.HashSet(a.asInstanceOf[Seq[_]]:_*)           },
		31.toByte -> { (a:Any) => scala.collection.mutable.IndexedSeq(a.asInstanceOf[Seq[_]]:_*)        },
		32.toByte -> { (a:Any) => scala.collection.mutable.LinearSeq(a.asInstanceOf[Seq[_]]:_*)         },
		33.toByte -> { (a:Any) => scala.collection.mutable.LinkedHashMap(a.asInstanceOf[Seq[(_,_)]]:_*) },
		34.toByte -> { (a:Any) => scala.collection.mutable.LinkedHashSet(a.asInstanceOf[Seq[_]]:_*)     },
		35.toByte -> { (a:Any) => scala.collection.mutable.ListBuffer(a.asInstanceOf[Seq[_]]:_*)        },
		36.toByte -> { (a:Any) => scala.collection.mutable.ListMap(a.asInstanceOf[Seq[(_,_)]]:_*)       },
		37.toByte -> { (a:Any) => scala.collection.mutable.Map(a.asInstanceOf[Seq[(_,_)]]:_*)           },
		38.toByte -> { (a:Any) => scala.collection.mutable.MutableList(a.asInstanceOf[Seq[_]]:_*)       },
		39.toByte -> { (a:Any) => scala.collection.mutable.OpenHashMap(a.asInstanceOf[Seq[(_,_)]]:_*)   },
		40.toByte -> { (a:Any) => scala.collection.mutable.Queue(a.asInstanceOf[Seq[_]]:_*)             },
		41.toByte -> { (a:Any) => scala.collection.mutable.ResizableArray(a.asInstanceOf[Seq[_]]:_*)    },
		42.toByte -> { (a:Any) => scala.collection.mutable.Seq(a.asInstanceOf[Seq[_]]:_*)               },
		43.toByte -> { (a:Any) => scala.collection.mutable.Set(a.asInstanceOf[Seq[_]]:_*)               },
		44.toByte -> { (a:Any) => scala.collection.mutable.Stack(a.asInstanceOf[Seq[_]]:_*)             },
		45.toByte -> { (a:Any) => scala.collection.mutable.WeakHashMap(a.asInstanceOf[Seq[(_,_)]]:_*)   },
		46.toByte -> { (a:Any) => { val t = a.asInstanceOf[Tuple2[_,_]]; (t._1,t._2)                   }},
		47.toByte -> { (a:Any) => { val t = a.asInstanceOf[Tuple3[_,_,_]]; (t._1,t._2,t._3)              }},
		48.toByte -> { (a:Any) => { val t = a.asInstanceOf[Tuple4[_,_,_,_]]; (t._1,t._2,t._3,t._4)         }},
		49.toByte -> { (a:Any) => { val t = a.asInstanceOf[Tuple5[_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5)    }},
		50.toByte -> { (a:Any) => { val t = a.asInstanceOf[Tuple6[_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6) }},
		51.toByte -> { (a:Any) => { val t = a.asInstanceOf[Tuple7[_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._6) }},
		52.toByte -> { (a:Any) => { val t = a.asInstanceOf[Tuple8[_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8) }},
		53.toByte -> { (a:Any) => { val t = a.asInstanceOf[Tuple9[_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9) }},
		54.toByte -> { (a:Any) => { val t = a.asInstanceOf[Tuple10[_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10) }},
		55.toByte -> { (a:Any) => { val t = a.asInstanceOf[Tuple11[_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11) }},
		56.toByte -> { (a:Any) => { val t = a.asInstanceOf[Tuple12[_,_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12) }},
		57.toByte -> { (a:Any) => { val t = a.asInstanceOf[Tuple13[_,_,_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13) }},
		58.toByte -> { (a:Any) => { val t = a.asInstanceOf[Tuple14[_,_,_,_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14) }},
		59.toByte -> { (a:Any) => { val t = a.asInstanceOf[Tuple15[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15) }},
		60.toByte -> { (a:Any) => { val t = a.asInstanceOf[Tuple16[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15,t._16) }},
		61.toByte -> { (a:Any) => { val t = a.asInstanceOf[Tuple17[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15,t._16,t._17) }},
		62.toByte -> { (a:Any) => { val t = a.asInstanceOf[Tuple18[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15,t._16,t._17,t._18) }},
		63.toByte -> { (a:Any) => { val t = a.asInstanceOf[Tuple19[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15,t._16,t._17,t._18,t._19) }},
		64.toByte -> { (a:Any) => { val t = a.asInstanceOf[Tuple20[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15,t._16,t._17,t._18,t._19,t._20) }},
		65.toByte -> { (a:Any) => { val t = a.asInstanceOf[Tuple21[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15,t._16,t._17,t._18,t._19,t._20,t._21) }},
		66.toByte -> { (a:Any) => { val t = a.asInstanceOf[Tuple22[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]; (t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15,t._16,t._17,t._18,t._19,t._20,t._21,t._22) }}
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