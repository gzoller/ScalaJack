package co.blocke.scalajack
package model

import util.Path

sealed trait ErrorReason
case object Unexpected extends ErrorReason
case object Invalid extends ErrorReason
case object Missing extends ErrorReason

// related is for testing mainly--avoids the need for string comparison of msg, which may vary with Scala version
class SJError(reason: ErrorReason, msg: String, related: List[String] = List.empty[String]) extends Exception(msg)

class SJReadError(path: Path, reason: ErrorReason, msg: String, related: List[String] = List.empty[String], captured: Option[Throwable] = None)
  extends SJError(reason, msg, related)
