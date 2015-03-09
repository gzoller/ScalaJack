package co.blocke.scalajack

import scala.reflect.runtime.universe._
import scala.collection.mutable.LinkedHashMap

trait SjItem
trait SjType extends SjItem {
  val name : String
  def tag = name
}
trait SjParam extends SjItem{
  val fieldName : String
  val ftype : SjType
}
trait ParamProxy // marker for proxies for parameterized classes

case class SjField( fieldName:String, ftype:SjType ) extends SjParam

// SjTypes -- name == scala class name except for SjTypesymbol where it = the type symbol placeholder
case class SjCaseClass( name:String, fields:List[SjField], isTrait:Boolean = false ) extends SjType
case class SjTrait( name:String, resolvedParamFields:LinkedHashMap[String,SjType] = LinkedHashMap.empty[String,SjType]) extends SjType { //, mappedParamMembers:Map[String,SjType] ) extends SjType {
  override def tag = name+resolvedParamFields.values.map(_.name).mkString("[",",","]")
}
case class SjCollection( name:String, collectionType:List[SjType] ) extends SjType {
  def isOptional = name == "scala.Option"
}
case class SjPrimitive( name:String ) extends SjType
case class SjTypeSymbol( name:String ) extends SjType
case class SjValueClass( name:String, vcType:SjType, vFieldName:String ) extends SjType
case class SjEnum( name:String, enum:Enumeration ) extends SjType

class ReflectException(msg:String) extends Exception(msg)
