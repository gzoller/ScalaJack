package co.blocke.scalajack

import model._
import json._

object ScalaJack:
  def apply()                                      = JsonFlavor()
  def apply[S](kind: JackFlavor[S]): JackFlavor[S] = kind


class ScalaJackError(msg: String)                           extends Exception(msg)
class ScalaJackValueError(val value: Any, cause: Throwable) extends Exception(cause.getMessage)

type HintBijective = util.BijectiveFunction[String, String]
val CHANGE_ANNO = "co.blocke.scalajack.Change"
val OPTIONAL_ANNO = "co.blocke.scalajack.Optional"
val IGNORE = "co.blocke.scalajack.Ignore"
val DB_KEY = "co.blocke.scalajack.DBKey"
val SJ_CAPTURE  = "co.blocke.scalajack.SJCapture"