package co.blocke.scalajack

import reflect.runtime.currentMirror
import reflect.runtime.universe._
import scala.collection.concurrent.TrieMap
import scala.reflect.NameTransformer._

object Analyzer {

	private val classRepo    = new TrieMap[String,Field]()
	private val typeRepo     = new TrieMap[String,FieldMirror]()

	private val ru        = scala.reflect.runtime.universe
	private val m         = ru.runtimeMirror(getClass.getClassLoader)
	private val mongoType = ru.typeOf[MongoKey]
	
	def apply[T]( cname:String ) : Field = 
		classRepo.get(cname).fold({	
			val clazz  = Class.forName(cname)
			val symbol = currentMirror.classSymbol(clazz)
			val symbolType = symbol.typeSignature
			val v = inspect[T]("", symbolType)
			classRepo.put(cname, v)
			v
		})((ccf) => ccf)

	private def inspect[T]( fieldName:String, ctype:Type, classCompanionSymbol:Option[Symbol] = None ) : Field = {

		val fullName = ctype.typeSymbol.fullName.toString

		if( fullName.toString == "scala.collection.immutable.List" ) {
			ctype match {
				case TypeRef(pre, sym, args) =>
					ListField(fieldName, inspect( fieldName, args(0) ))
			}
		} else if( fullName == "scala.Enumeration.Value" ) {
			val erasedEnumClass = Class.forName(ctype.asInstanceOf[TypeRef].toString.replace(".Value","$"))
			val enum = erasedEnumClass.getField(MODULE_INSTANCE_NAME).get(null).asInstanceOf[Enumeration]
			EnumField( fieldName, enum)
		} else if( fullName == "scala.Option" ) {
			val subtype = ctype.asInstanceOf[TypeRef].args(0)
			OptField( fieldName, inspect(fieldName, subtype) )
		} else if( fullName == "scala.collection.immutable.Map" ) {
			val valuetype = ctype.asInstanceOf[TypeRef].args(1)
			MapField( fieldName, inspect(fieldName, valuetype) )
		} else {
			val sym = currentMirror.classSymbol(Class.forName(fullName))
			if( sym.isTrait && !fullName.startsWith("scala"))
				TraitField( fieldName )
			else if( sym.isCaseClass ) {
				// Find and save the apply method of the companion object
				val companionClazz = Class.forName(fullName+"$")
				val companionSymbol = currentMirror.classSymbol(companionClazz)
				val caseObj = companionClazz.getField(MODULE_INSTANCE_NAME).get(null)
				val applyMethod = companionClazz.getMethods.find( _.getName == "apply" ).get
				
				// Build the field list
				val constructor = ctype.members.collectFirst {
					case method: MethodSymbol
						if method.isPrimaryConstructor && method.isPublic && !method.paramss.isEmpty && !method.paramss.head.isEmpty => method
				}.getOrElse( throw new IllegalArgumentException("Case class must have at least 1 public constructor having more than 1 parameters."))
				val fields = constructor.paramss.head.map( c => { inspect(c.name.toString, c.typeSignature, Some(companionSymbol)) })

				CaseClassField( fieldName, ctype, fullName, applyMethod, fields, caseObj )
			} else {
				// See if there's a MongoKey annotation on any of the class' fields
				val mongoAnno = classCompanionSymbol.fold(List[String]())( (cs) => {
					cs.typeSignature.members.collectFirst {
						case method:MethodSymbol if( method.name.toString == "apply") => method.paramss.head.collect{ case p if( p.annotations.find(a => a.tpe == Analyzer.mongoType).isDefined) => p.name.toString }
					}.getOrElse(List[String]())
				})
				fullName match {
					case "java.lang.String" => StringField( fieldName, mongoAnno.contains(fieldName))
					case "scala.Int"        => IntField( fieldName, mongoAnno.contains(fieldName))
					case "scala.Char"       => CharField( fieldName, mongoAnno.contains(fieldName))
					case "scala.Long"       => LongField( fieldName, mongoAnno.contains(fieldName))
					case "scala.Float"      => FloatField( fieldName, mongoAnno.contains(fieldName))
					case "scala.Double"     => DoubleField( fieldName, mongoAnno.contains(fieldName))
					case "scala.Boolean"    => BoolField( fieldName, mongoAnno.contains(fieldName))
					case _                  => {
						if( isValueClass(sym) ) {
							// Class name transformation so Analyzer will work
							val className = Class.forName(fullName).getDeclaredFields.head.getType.getName match {
								case "int"     => "scala.Int"
								case "char"    => "scala.Char"
								case "long"    => "scala.Long"
								case "float"   => "scala.Flaot"
								case "double"  => "scala.Double"
								case "boolean" => "scala.Boolean"
							}
							ValueClassField( fieldName, mongoAnno.contains(fieldName), Analyzer( className ) )
						} else
							throw new IllegalArgumentException("Unknown/unsupported data type: "+fullName)
					}
				} 
			}
		}
	}

	// Pulled this off Stackoverflow... Not sure if it's 100% effective, but seems to work!
	private def isValueClass( sym:ClassSymbol ) = sym.asType.companionSymbol.typeSignature.members.exists(_.name.toString.endsWith("$extension"))
}
