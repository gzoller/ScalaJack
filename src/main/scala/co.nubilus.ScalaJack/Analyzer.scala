package co.nubilus.ScalaJack

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._
import scala.reflect._
import scala.reflect.runtime.{ currentMirror => cm }
import scala.collection.concurrent.TrieMap
import scala.reflect.NameTransformer._

object Analyzer {

	private val classRepo    = new TrieMap[String,Field]()
	private val typeRepo     = new TrieMap[String,FieldMirror]()

	val ru        = scala.reflect.runtime.universe
	val m         = ru.runtimeMirror(getClass.getClassLoader)
	val mongoType = ru.typeOf[MongoKey]
	
	def apply[T]( cname:String ) : Field = 
		classRepo.get(cname).fold({	
			val v = inspect[T](cname)
			classRepo.put(cname, v)
			v
		})((ccf) => ccf)

	private def inspect[T]( cname:String ) : Field = {
		val clazz  = Class.forName(cname)
		val symbol = currentMirror.classSymbol(clazz)
		if( symbol.isCaseClass ) {
			// Find and save the apply method of the companion object
			val companionClazz = Class.forName(cname+"$")
			val caseObj = companionClazz.getField(MODULE_INSTANCE_NAME).get(null)
			val applyMethod = companionClazz.getMethods.find( _.getName == "apply" ).get
			
			// Build the field list
		    val constructor = symbol.toType.members.collectFirst {
		      case method: MethodSymbol
		        if method.isPrimaryConstructor && method.isPublic && !method.paramss.isEmpty && !method.paramss.head.isEmpty => method
		    }.getOrElse( throw new IllegalArgumentException("Case class must have at least 1 public constructor having more than 1 parameters."))
		    val fields = constructor.paramss.head.map( c => inspectField(c,symbol) )

		    CaseClassField( "", symbol.toType, cname, applyMethod, fields, caseObj )
		} else if( symbol.isTrait ) 
			TraitField( "", symbol.toType )
		else 
			throw new IllegalArgumentException("Only case classes are parsable.  Sorry. ("+symbol+")")
	}
	
	private def inspectField[T]( sym:Symbol, classSymbol:Symbol ) : Field = {		
		val cname = sym.name.toString
		val cType = sym.typeSignature
		val fullName = cType.typeSymbol.fullName.toString
		if( fullName == "scala.Enumeration.Value" ) {
			val erasedEnumClass = Class.forName(cType.asInstanceOf[TypeRef].toString.replace(".Value","$"))
			val enum = erasedEnumClass.getField(MODULE_INSTANCE_NAME).get(null).asInstanceOf[Enumeration]
			EnumField( cname, cType, enum)
		} else if( fullName == "scala.Option" ) {
			val subtype = cType.asInstanceOf[TypeRef].args(0)
			OptField( cname, cType, inspectField(subtype.typeSymbol, classSymbol) )
		} else if( fullName == "scala.collection.immutable.List" ) {
			val subtype = cType.asInstanceOf[TypeRef].args(0)
			ListField( cname, cType, inspectField(subtype.typeSymbol, classSymbol) )
		} else if( fullName == "scala.collection.immutable.Map" ) {
			val keytype = cType.asInstanceOf[TypeRef].args(0)
			val valuetype = cType.asInstanceOf[TypeRef].args(1)
			MapField( cname, cType, inspectField(keytype.typeSymbol, classSymbol), inspectField(valuetype.typeSymbol, classSymbol) )
		} else {
    		val sym = currentMirror.classSymbol(Class.forName(fullName))
    		if( sym.isTrait && !fullName.startsWith("scala"))
    			TraitField( cname, cType )
    		else if( sym.isCaseClass ) {
    			val cc = inspect(fullName).asInstanceOf[CaseClassField]
    			cc.copy( name = cname )
    		}
    		else { 
				// See if there's a MongoKey annotation on any of the class' fields
				val mongoAnno = classSymbol.companionSymbol.typeSignature.members.collectFirst {
					case method:MethodSymbol if( method.name.toString == "apply") => method.paramss.head.collect{ case p if( p.annotations.find(a => a.tpe == Analyzer.mongoType).isDefined) => p.name.toString }
				}.getOrElse(List[String]())
    			BaseField( cname, cType, mongoAnno.contains(cname))//sym.name.toString))
    		}
		}
	}
}

trait Field {
	val name         : String
	val dt           : Type
	val hasMongoAnno : Boolean = false
}
case class BaseField     ( name:String, dt:Type, override val hasMongoAnno:Boolean ) extends Field
case class EnumField     ( name:String, dt:Type, enum:Enumeration ) extends Field
case class TraitField    ( name:String, dt:Type ) extends Field
case class OptField      ( name:String, dt:Type, subField:Field ) extends Field
case class ListField     ( name:String, dt:Type, subField:Field ) extends Field
case class MapField      ( name:String, dt:Type, keyField:Field, valueField:Field ) extends Field
case class CaseClassField( name:String, dt:Type, className:String, applyMethod:java.lang.reflect.Method, fields:List[Field], caseObj:Object ) extends Field {
	val iFields = fields.map( f => (f.name, f)).toMap
}
