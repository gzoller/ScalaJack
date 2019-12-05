package co.blocke.scalajack
package json

import org.json4s.JsonAST.{ JNothing, JObject, JValue }

object JsonDiff {

  def compare(
      left:       JValue,
      right:      JValue,
      leftLabel:  String = "left",
      rightLabel: String = "right"): Seq[JsonDiff] = {
    (left, right) match {
      case (JObject(leftFields), JObject(rightFields)) =>
        val allFieldNames =
          (leftFields.map(_._1) ++ rightFields.map(_._1)).distinct
        allFieldNames.sorted flatMap { fieldName =>
          val leftFieldValue = leftFields
            .collectFirst({ case (`fieldName`, fieldValue) => fieldValue })
            .getOrElse(JNothing)
          val rightFieldValue = rightFields
            .collectFirst({ case (`fieldName`, fieldValue) => fieldValue })
            .getOrElse(JNothing)
          compare(leftFieldValue, rightFieldValue, leftLabel, rightLabel)
        }

      // ---- Not used/needed at present, and I have questions about the correct behavior here.  Exactly how do you
      //      "diff" two arrays (not necessarily homogeneous typed)?
      //
      //      case (JArray(leftElements), JArray(rightElements)) =>
      //        (0 until (leftElements.size max rightElements.size)) flatMap { elementIndex =>
      //          val leftElement = leftElements.applyOrElse(elementIndex, (_: Int) => JNothing)
      //          val rightElement = rightElements.applyOrElse(elementIndex, (_: Int) => JNothing)
      //          compare(path \ elementIndex, leftElement, rightElement, leftLabel, rightLabel)
      //        }

      case _ =>
        if (left == right) {
          Seq.empty
        } else {
          val outerLeft = left
          val outerRight = right
          Seq(new JsonDiff {
            override val left: JValue = outerLeft
            override val right: JValue = outerRight
            override def toString: String =
              s"JsonDiff($leftLabel: $left, $rightLabel: $right)"
          })
        }
    }
  }

}

trait JsonDiff {
  val left: JValue
  val right: JValue
}
