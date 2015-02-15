package co.blocke.scalajack

import scala.reflect.runtime.universe._

trait SjItem
trait SjType extends SjItem {
	val name : String
}
trait SjParam extends SjItem{
	val fieldName : String
	val ftype : SjType
}

case class SjField( fieldName:String, ftype:SjType ) extends SjParam

// SjTypes -- name == scala class name except for SjTypesymbol where it = the type symbol placeholder
case class SjCaseClass( name:String, params:List[String], fields:List[SjField], isTrait:Boolean=false ) extends SjType
case class SjTrait( name:String, params:List[String] ) extends SjType
case class SjCollection( name:String, collectionType:List[SjType] ) extends SjType
case class SjPrimitive( name:String ) extends SjType
case class SjTypeSymbol( name:String ) extends SjType
case class SjValueClass( name:String, vcType:SjType, vFieldName:String ) extends SjType

class ReflectException(msg:String) extends Exception(msg)
