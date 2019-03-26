package co.blocke.scalajack
package model

import util.Path

// related is for testing mainly--avoids the need for string comparison of msg, which may vary with Scala version
class SJError(msg: String) extends Exception(msg)

class ReadUnexpectedError(val msg: String, val nullFound: Boolean = false) extends SJError(msg)
class ReadMalformedError(val msg: String) extends SJError(msg)
class ReadInvalidError(val msg: String) extends SJError(msg)
class ReadMissingError(val msg: String) extends SJError(msg)
