package co.blocke.scalajack

import co.blocke.scalajack.model.{ ReadInvalidError, ReadMalformedError, ReadMissingError, ReadUnexpectedError }
import co.blocke.scalajack.util.Path

object TestUtil {
  def expectUnexpected(fn: () => Any, path: Path, related: List[String]): Boolean = {
    try {
      fn()
      throw new Exception("fn() worked--it shoudln't have!")
    } catch {
      case t: ReadUnexpectedError =>
        if (t.path != path)
          throw new Exception("Exeption path " + t.path + " didn't match expected path " + path)
        if (t.related != related)
          throw new Exception("Exeption related " + t.related.mkString("(", ",", ")") + " didn't match expected related " + related.mkString("(", ",", ")"))
      case x =>
        throw x
    }
    true
  }

  def expectInvalid(fn: () => Any, path: Path, related: List[String]): Boolean = {
    try {
      fn()
      throw new Exception("fn() worked--it shoudln't have!")
    } catch {
      case t: ReadInvalidError =>
        if (t.path != path)
          throw new Exception("Exeption path " + t.path + " didn't match expected path " + path)
        if (t.related != related) {
          throw new Exception("Exeption related " + t.related.mkString("(", ",", ")") + " didn't match expected related " + related.mkString("(", ",", ")"))
        }
      case x =>
        throw x
    }
    true
  }

  def expectMissing(fn: () => Any, path: Path, related: List[String]): Boolean = {
    try {
      fn()
      throw new Exception("fn() worked--it shoudln't have!")
    } catch {
      case t: ReadMissingError =>
        if (t.path != path)
          throw new Exception("Exeption path " + t.path + " didn't match expected path " + path)
        if (t.related != related) {
          throw new Exception("Exeption related " + t.related.mkString("(", ",", ")") + " didn't match expected related " + related.mkString("(", ",", ")"))
        }
      case x =>
        throw x
    }
    true
  }

  def expectMalformed[W](fn: () => Any, path: Path, related: List[String])(implicit tt: TypeTag[W]): Boolean = {
    try {
      fn()
      throw new Exception("fn() worked--it shoudln't have!")
    } catch {
      case t: ReadMalformedError =>
        if (t.path != path)
          throw new Exception("Exeption path " + t.path + " didn't match expected path " + path)
        if (t.related != related)
          throw new Exception("Exeption related " + t.related.mkString("(", ",", ")") + " didn't match expected related " + related.mkString("(", ",", ")"))
        if (tt.tpe.typeSymbol.fullName != t.wrappedException.getClass.getCanonicalName)
          throw new Exception("Expected a wrapped exception " + tt.tpe.typeSymbol.fullName + " but found " + t.wrappedException.getClass.getCanonicalName)
      case x =>
        throw x
    }
    true
  }

  def hexStringToByteArray(s: String): Array[Byte] = {
    val len = s.length
    val data = new Array[Byte](len / 2)
    var i = 0
    while ({
      i < len
    }) {
      data(i / 2) = ((Character.digit(s.charAt(i), 16) << 4) + Character.digit(s.charAt(i + 1), 16)).toByte

      i += 2
    }
    data
  }
}