package co.blocke.scalajack
package json

import scala.util.Failure

val BUFFER_EXCEEDED: Char = 7 // Old "BELL" ASCII value, used as a marker when we've run off the end of the known world
val END_OF_STRING: Char = 3

inline def lastPart(n: String) = n.split('.').last.stripSuffix("$")
