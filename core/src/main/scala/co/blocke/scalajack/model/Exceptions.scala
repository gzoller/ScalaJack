package co.blocke.scalajack
package model

import util.Path

class UnexpectedException(ta: TypeAdapter[_], msg: String, index: Int = -1) extends Exception(msg)

class ReadException(path: Path, msg: String) extends Exception(msg)