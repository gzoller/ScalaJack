package co.blocke
package scalajack

import scala.reflect.runtime.universe._
import scala.collection.mutable.LinkedHashMap

class ReflectException(msg:String) extends Exception(msg)

trait AType {
	val name    : String
	protected[scalajack] var _isDbKey = false
	def isDbKey : Boolean = _isDbKey
	def dup : AType  // Allows type-safe copying of AType for modifying _isDbKey flag.
	// (A type may be a dbKey in one context but not another so it's not a universal property of a type.)
}
case class CCType(
	name       : String, 
	members    : LinkedHashMap[String,AType], 
	paramMap   : LinkedHashMap[String,AType] = LinkedHashMap.empty[String,AType],
	superTrait : Option[TraitType] = None,
	collAnno   : Option[String] = None  // db collumn annotation 
) extends AType {
	override def toString() = s"[$name -> ${members.map(_._1)}]"
	def dup = this.copy()
}
case class PrimType(name:String) extends AType { def dup = this.copy() }
case class CollType(name:String, colTypes:List[AType]) extends AType {
	def isOptional = name == "scala.Option"
	def dup = this.copy()
}
case class EnumType(name:String, enum:Enumeration) extends AType { def dup = this.copy() }
case class ValueClassType(name:String, vcType:AType, vFieldName:String, isTypeParam:Boolean) extends AType { def dup = this.copy() }
case class TraitType(name:String, paramMap:LinkedHashMap[String,AType] = LinkedHashMap.empty[String,AType]) extends AType { def dup = this.copy() }

trait CustomType extends AType {
	val readers   : Map[String, (Any => Any)]
	val renderers : Map[String, (Any => Any)]
}

case class ErrType(name:String = "Error") extends AType { def dup = this.copy() }
