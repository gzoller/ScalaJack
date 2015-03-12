package co.blocke.scalajack

import scala.reflect.runtime.universe._
import scala.collection.mutable.LinkedHashMap

class ReflectException(msg:String) extends Exception(msg)

trait AType {
  val name   : String
}
case class CCType(
    name     : String, 
    members  : LinkedHashMap[String,AType], 
    paramMap : LinkedHashMap[String,AType] = LinkedHashMap.empty[String,AType],
    isTrait  : Boolean = false
) extends AType {
  override def toString() = s"[$name -> $members]"
}
case class PrimType(name:String) extends AType
case class CollType(name:String, colTypes:List[AType]) extends AType {
  def isOptional = name == "scala.Option"
}
case class EnumType(name:String, enum:Enumeration) extends AType
case class ValueClassType(name:String, vcType:AType, vFieldName:String, isTypeParam:Boolean) extends AType
case class TraitType(name:String, paramMap:LinkedHashMap[String,AType] = LinkedHashMap.empty[String,AType]) extends AType
case class ErrType(name:String = "Error") extends AType
