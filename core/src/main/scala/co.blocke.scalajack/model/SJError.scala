package co.blocke.scalajack
package model

import util.Path

// related is for testing mainly--avoids the need for string comparison of msg, which may vary with Scala version
class SJError(msg: String, related: List[String]) extends Exception(msg)

class ReadUnexpectedError(path: Path, msg: String, related: List[String] = List.empty[String]) extends SJError(msg, related)
class ReadMalformedError(path: Path, msg: String, related: List[String], wrappedException: Throwable) extends SJError(msg, related)
class ReadInvalidError(path: Path, msg: String, related: List[String] = List.empty[String]) extends SJError(msg, related)
class ReadMissingError(path: Path, msg: String, related: List[String] = List.empty[String]) extends SJError(msg, related)

//class SJReadError(path: Path, reason: ErrorReason, msg: String, related: List[String] = List.empty[String], captured: Option[Throwable] = None)
//  extends SJError(reason, msg, related)
