package co.blocke.scalajack
package json

import scala.util.Failure

val BUFFER_EXCEEDED: Char = 7 // Old "BELL" ASCII value, used as a marker when we've run off the end of the known world
val END_OF_STRING: Char = 3

inline def lastPart(n: String) = n.split('.').last.stripSuffix("$")

// Tests whether we should write something or not--mainly in the case of Option, or wrapped Option
def isOkToWrite(a: Any, cfg: JsonConfig) =
  a match
    case None if !cfg.noneAsNull                                    => false
    case o: java.util.Optional[?] if o.isEmpty && !cfg.noneAsNull   => false
    case Left(None) if !cfg.noneAsNull                              => false
    case Right(None) if !cfg.noneAsNull                             => false
    case Failure(_) if cfg.tryFailureHandling == TryOption.NO_WRITE => false
    case _                                                          => true
