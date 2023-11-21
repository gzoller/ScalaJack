package co.blocke.scalajack
package run

import co.blocke.scala_reflection.*
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag

object RunMe extends App:

  given json.JsonConfig = json
    .JsonConfig()
    .copy(noneAsNull = true)
    .copy(writeNonConstructorFields = true)
  // .copy(enumsAsIds = '*')

  try

    import json.*
    import ScalaJack.*

    // co.blocke.scalajack.internal.CodePrinter.code {
    //   sj[Record]
    // }

    val v = Foo("Hey", "Boo")
    // println(sj[Foo].toJson(v))
    println(sj[Record].toJson(record))

    // println(sj[Record].toJson(record))

    // println("------")

    // println(sj[Record].fromJson(jsData))

    // import internal.*

    // val root = TreeNode("1A", List(TreeNode("2A", List(TreeNode("3A", Nil))), TreeNode("2B", List(TreeNode("3B", Nil))), TreeNode("2C", List(TreeNode("3C", Nil)))))

    // val m = Map(
    //   "1A" -> "Report",
    //   "2A" -> "Person",
    //   "2B" -> "Seq[Friend]",
    //   "2C" -> "Seq[Pet]",
    //   "3A" -> "Address",
    //   "3B" -> "Friend",
    //   "3C" -> "Pet"
    // )

    // println(TreeNode.inverted(root).map(p => m(p.payload)))

  catch {
    case t: Throwable =>
      println(s"BOOM ($t): " + t.getMessage)
      t.printStackTrace
  }
