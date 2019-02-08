package co.blocke.scalajack
package model

import util.Path

// related is for testing mainly--avoids the need for string comparison of msg, which may vary with Scala version
class SJError(msg: String, related: List[String]) extends Exception(msg)

class ReadUnexpectedError(val path: Path, val msg: String, val related: List[String] = List.empty[String]) extends SJError(s"[$path]: " + msg, related) {
  override def toString() = s"[$path]: $msg"
}
class ReadMalformedError(val path: Path, val msg: String, val related: List[String], val wrappedException: Throwable) extends SJError(s"[$path]: " + msg, related) {
  override def toString() = s"[$path] (wrapping ${wrappedException.getClass.getName}): $msg"
}
class ReadInvalidError(val path: Path, val msg: String, val related: List[String] = List.empty[String]) extends SJError(s"[$path]: " + msg, related) {
  override def toString() = s"[$path]: $msg"
}
class ReadMissingError(val path: Path, val msg: String, val related: List[String] = List.empty[String]) extends SJError(s"[$path]: " + msg, related) {
  override def toString() = s"[$path]: $msg"
}
