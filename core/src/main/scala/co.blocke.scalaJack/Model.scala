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
	members    : LinkedHashMap[String,(AType,Option[Any])],   // Map[memberName -> (MemberType,Optional Default Value)]
	paramMap   : LinkedHashMap[String,AType] = LinkedHashMap.empty[String,AType],
	superTrait : Option[TraitType] = None,
	collAnno   : Option[String] = None  // db collumn annotation 
) extends AType {
	private val constructor = Class.forName(name).getConstructors()(0)
	private lazy val defaults = members.collect {
		case (mname, (mtype,mdefault)) if(mdefault.isDefined) => (mname,mdefault.get)
		case (mname, (mtype:CollType,_)) if(mtype.isOptional) => (mname,None)
	}.toMap
	override def toString() = s"[$name -> ${members.map(_._1)}]"
	def dup = this.copy()
	def materialize( content:Map[String,Any] ) = {
		val cv = members.map{ case(mname,_) => content.get(mname).orElse(defaults.get(mname)).get}.toArray.asInstanceOf[Array[AnyRef]]//.asInstanceOf[List[AnyRef]]
		constructor.newInstance(cv:_*)
	}
}

case class PrimType(name:String, alias:Option[String]=None) extends AType { 
	val primCode = PrimitiveTypes.primCodes(name)
	def dup = this.copy() 
}

case class CollType(name:String, colTypes:List[AType]) extends AType {
	val collCode = PrimitiveTypes.collCodes(name)
	def isOptional = {collCode == 0}
	def dup = this.copy()
}

case class JavaType(name:String) extends AType {
	def dup = this.copy()
}

case class EnumType(name:String, enum:Enumeration) extends AType { def dup = this.copy() }
case class ValueClassType(name:String, vcType:AType, vFieldName:String, isTypeParam:Boolean, custom:Option[VCCustomMethods]) extends AType { def dup = this.copy() }

trait ValueClassCustom extends Any {
	def read:PartialFunction[(KindMarker,_), Any]    // [(JackFlavor[S],S),ValueClassInstance]
	def render:PartialFunction[(KindMarker,_), Any]  // [(JackFlavor[S],ValueClassInstance), S]
}
case class VCCustomMethods(
	read:PartialFunction[(KindMarker,_), Any],
	render:PartialFunction[(KindMarker,_), Any]
	)

trait KindMarker

case class TraitType(
	name     : String, 
	members  : LinkedHashMap[String,AType] = LinkedHashMap.empty[String,AType],
	paramMap : LinkedHashMap[String,AType] = LinkedHashMap.empty[String,AType], 
	default  : Option[Any]=None
) extends AType { 
	def dup = this.copy() 
}

case class ErrType(name:String = "Error") extends AType { 
	def dup = this.copy() 
}
