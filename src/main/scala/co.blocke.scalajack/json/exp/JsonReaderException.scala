package co.blocke.scalajack
package json
package exp

class JsonReaderException private[json] (msg: String, cause: Throwable, withStackTrace: Boolean) extends RuntimeException(msg, cause, true, withStackTrace)
