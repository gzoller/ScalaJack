package co.blocke.scalajack

class JsonWriterException (msg: String, cause: Throwable, withStackTrace: Boolean)
  extends RuntimeException(msg, cause, true, withStackTrace)
