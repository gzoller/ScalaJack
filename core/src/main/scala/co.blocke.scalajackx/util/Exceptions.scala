package co.blocke.scalajackx
package util

case class UnexpectedException(path: TypeAdapter[_], msg: String, index: Int = -1) extends Exception(msg)
