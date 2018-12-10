package co.blocke.series4

import org.joda.time.{ DateTime, DateTimeZone }

object PrimitiveTypes {
  private[series4] val primCodes: Map[String, Byte] = Map(
    "scala.Int" -> 0,
    "java.lang.Integer" -> 1,
    "scala.Boolean" -> 2,
    "java.lang.String" -> 3,
    "String" -> 3,
    "scala.Float" -> 4,
    "scala.Double" -> 5,
    "scala.Long" -> 6,
    "scala.Char" -> 7,
    "scala.Byte" -> 8,
    "scala.Short" -> 9,
    "java.lang.Boolean" -> 10,
    "java.util.UUID" -> 11,
    "org.joda.time.DateTime" -> 12,
    "scala.Any" -> 13
  )

  val INT = 0
  val STRING = 3
  val DOUBLE = 5
  val ANY = 13
  val BOOLEAN = 10

  private[series4] val collCodes: Map[String, Byte] = Map(
    "scala.Option" -> 0,
    "scala.collection.immutable.Map" -> 1,
    "scala.collection.immutable.HashMap" -> 2,
    "scala.collection.immutable.ListMap" -> 3,
    "scala.collection.mutable.HashMap" -> 4,
    "scala.collection.mutable.LinkedHashMap" -> 5,
    "scala.collection.mutable.ListMap" -> 6,
    "scala.collection.mutable.Map" -> 7,
    "scala.collection.mutable.OpenHashMap" -> 8,
    "scala.collection.mutable.WeakHashMap" -> 9,
    "scala.collection.immutable.List" -> 10,
    "scala.collection.immutable.Set" -> 11,
    "scala.collection.immutable.HashSet" -> 12,
    "scala.collection.immutable.ListSet" -> 13,
    "scala.collection.immutable.Queue" -> 14,
    "scala.collection.immutable.Seq" -> 15,
    "scala.collection.immutable.Vector" -> 16,
    "scala.collection.mutable.ArrayBuffer" -> 17,
    "scala.collection.mutable.ArraySeq" -> 18,
    "scala.collection.mutable.HashSet" -> 19,
    "scala.collection.mutable.IndexedSeq" -> 20,
    "scala.collection.mutable.LinearSeq" -> 21,
    "scala.collection.mutable.LinkedHashSet" -> 22,
    "scala.collection.mutable.ListBuffer" -> 23,
    "scala.collection.mutable.MutableList" -> 24,
    "scala.collection.mutable.Queue" -> 25,
    "scala.collection.mutable.ResizableArray" -> 26,
    "scala.collection.mutable.Seq" -> 27,
    "scala.collection.mutable.Set" -> 28,
    "scala.collection.mutable.Stack" -> 29,
    "scala.Tuple2" -> 30,
    "scala.Tuple3" -> 31,
    "scala.Tuple4" -> 32,
    "scala.Tuple5" -> 33,
    "scala.Tuple6" -> 34,
    "scala.Tuple7" -> 35,
    "scala.Tuple8" -> 36,
    "scala.Tuple9" -> 37,
    "scala.Tuple10" -> 38,
    "scala.Tuple11" -> 39,
    "scala.Tuple12" -> 40,
    "scala.Tuple13" -> 41,
    "scala.Tuple14" -> 42,
    "scala.Tuple15" -> 43,
    "scala.Tuple16" -> 44,
    "scala.Tuple17" -> 45,
    "scala.Tuple18" -> 46,
    "scala.Tuple19" -> 47,
    "scala.Tuple20" -> 48,
    "scala.Tuple21" -> 49,
    "scala.Tuple22" -> 50
  )

  private[series4] val primTypes: List[(String) => Any] = List(
    { (s: String) => s.toInt },
    { (s: String) => java.lang.Integer.parseInt(s) },
    { (s: String) => s.toBoolean },
    { (s: String) => s },
    { (s: String) => s.toFloat },
    { (s: String) => s.toDouble },
    { (s: String) => s.toLong },
    { (s: String) => if (!s.isEmpty) s.charAt(0) else null },
    { (s: String) => s.toByte },
    { (s: String) => s.toShort },
    { (s: String) => java.lang.Boolean.parseBoolean(s) },
    { (s: String) => java.util.UUID.fromString(s) },
    { (s: String) => (new DateTime(s.toLong)).toDateTime(DateTimeZone.forID("UTC")) },
    { (s: String) => s.toString } // Not really used!  JSON code has type inference
  )

  // Map of [class_name -> builder_fn]
  private[series4] val collTypes: List[(Any) => Any] = List(
    { (a: Any) => Some(a) },
    { (a: Any) => scala.collection.immutable.Map(a.asInstanceOf[Seq[(_, _)]]: _*) },
    { (a: Any) => scala.collection.immutable.HashMap(a.asInstanceOf[Seq[(_, _)]]: _*) },
    { (a: Any) => scala.collection.immutable.ListMap(a.asInstanceOf[Seq[(_, _)]]: _*) },
    { (a: Any) => scala.collection.mutable.HashMap(a.asInstanceOf[Seq[(_, _)]]: _*) },
    { (a: Any) => scala.collection.mutable.LinkedHashMap(a.asInstanceOf[Seq[(_, _)]]: _*) },
    { (a: Any) => scala.collection.mutable.ListMap(a.asInstanceOf[Seq[(_, _)]]: _*) },
    { (a: Any) => scala.collection.mutable.Map(a.asInstanceOf[Seq[(_, _)]]: _*) },
    { (a: Any) => scala.collection.mutable.OpenHashMap(a.asInstanceOf[Seq[(_, _)]]: _*) },
    { (a: Any) => scala.collection.mutable.WeakHashMap(a.asInstanceOf[Seq[(_, _)]]: _*) },
    { (a: Any) => scala.collection.immutable.List(a.asInstanceOf[Seq[_]]: _*) },
    { (a: Any) => scala.collection.immutable.Set(a.asInstanceOf[Seq[_]]: _*) },
    { (a: Any) => scala.collection.immutable.HashSet(a.asInstanceOf[Seq[_]]: _*) },
    { (a: Any) => scala.collection.immutable.ListSet(a.asInstanceOf[Seq[_]]: _*) },
    { (a: Any) => scala.collection.immutable.Queue(a.asInstanceOf[Seq[_]]: _*) },
    { (a: Any) => a.asInstanceOf[Seq[_]] },
    { (a: Any) => scala.collection.immutable.Vector(a.asInstanceOf[Seq[_]]: _*) },
    { (a: Any) => scala.collection.mutable.ArrayBuffer(a.asInstanceOf[Seq[_]]: _*) },
    { (a: Any) => scala.collection.mutable.ArraySeq(a.asInstanceOf[Seq[_]]: _*) },
    { (a: Any) => scala.collection.mutable.HashSet(a.asInstanceOf[Seq[_]]: _*) },
    { (a: Any) => scala.collection.mutable.IndexedSeq(a.asInstanceOf[Seq[_]]: _*) },
    { (a: Any) => scala.collection.mutable.LinearSeq(a.asInstanceOf[Seq[_]]: _*) },
    { (a: Any) => scala.collection.mutable.LinkedHashSet(a.asInstanceOf[Seq[_]]: _*) },
    { (a: Any) => scala.collection.mutable.ListBuffer(a.asInstanceOf[Seq[_]]: _*) },
    { (a: Any) => scala.collection.mutable.MutableList(a.asInstanceOf[Seq[_]]: _*) },
    { (a: Any) => scala.collection.mutable.Queue(a.asInstanceOf[Seq[_]]: _*) },
    { (a: Any) => scala.collection.mutable.ResizableArray(a.asInstanceOf[Seq[_]]: _*) },
    { (a: Any) => scala.collection.mutable.Seq(a.asInstanceOf[Seq[_]]: _*) },
    { (a: Any) => scala.collection.mutable.Set(a.asInstanceOf[Seq[_]]: _*) },
    { (a: Any) => scala.collection.mutable.Stack(a.asInstanceOf[Seq[_]]: _*) },
    { (a: Any) => { val t = a.asInstanceOf[Tuple2[_, _]]; (t._1, t._2) } },
    { (a: Any) => { val t = a.asInstanceOf[Tuple3[_, _, _]]; (t._1, t._2, t._3) } },
    { (a: Any) => { val t = a.asInstanceOf[Tuple4[_, _, _, _]]; (t._1, t._2, t._3, t._4) } },
    { (a: Any) => { val t = a.asInstanceOf[Tuple5[_, _, _, _, _]]; (t._1, t._2, t._3, t._4, t._5) } },
    { (a: Any) => { val t = a.asInstanceOf[Tuple6[_, _, _, _, _, _]]; (t._1, t._2, t._3, t._4, t._5, t._6) } },
    { (a: Any) => { val t = a.asInstanceOf[Tuple7[_, _, _, _, _, _, _]]; (t._1, t._2, t._3, t._4, t._5, t._6, t._6) } },
    { (a: Any) => { val t = a.asInstanceOf[Tuple8[_, _, _, _, _, _, _, _]]; (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8) } },
    { (a: Any) => { val t = a.asInstanceOf[Tuple9[_, _, _, _, _, _, _, _, _]]; (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9) } },
    { (a: Any) => { val t = a.asInstanceOf[Tuple10[_, _, _, _, _, _, _, _, _, _]]; (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10) } },
    { (a: Any) => { val t = a.asInstanceOf[Tuple11[_, _, _, _, _, _, _, _, _, _, _]]; (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11) } },
    { (a: Any) => { val t = a.asInstanceOf[Tuple12[_, _, _, _, _, _, _, _, _, _, _, _]]; (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12) } },
    { (a: Any) => { val t = a.asInstanceOf[Tuple13[_, _, _, _, _, _, _, _, _, _, _, _, _]]; (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13) } },
    { (a: Any) => { val t = a.asInstanceOf[Tuple14[_, _, _, _, _, _, _, _, _, _, _, _, _, _]]; (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14) } },
    { (a: Any) => { val t = a.asInstanceOf[Tuple15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]; (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15) } },
    { (a: Any) => { val t = a.asInstanceOf[Tuple16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]; (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16) } },
    { (a: Any) => { val t = a.asInstanceOf[Tuple17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]; (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17) } },
    { (a: Any) => { val t = a.asInstanceOf[Tuple18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]; (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18) } },
    { (a: Any) => { val t = a.asInstanceOf[Tuple19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]; (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19) } },
    { (a: Any) => { val t = a.asInstanceOf[Tuple20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]; (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20) } },
    { (a: Any) => { val t = a.asInstanceOf[Tuple21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]; (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21) } },
    { (a: Any) => { val t = a.asInstanceOf[Tuple22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]; (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22) } }
  )

  // The following collections are not supported because there's no way to infer the implicit Ordering:
  // "scala.collection.immutable.TreeMap"
  // "scala.collection.immutable.TreeSet"
  // "scala.collection.mutable.PriorityQueue"
  // "scala.collection.mutable.SortedSet"
  // "scala.collection.mutable.TreeSet"

  implicit class CollMaps(val symbol: scala.reflect.runtime.universe.Symbol) extends AnyVal {
    def isCollection = collCodes.contains(symbol.fullName) || symbol.fullName.startsWith("scala.collection")
    def isPrimitive = primCodes.contains(symbol.asClass.fullName)
  }
}