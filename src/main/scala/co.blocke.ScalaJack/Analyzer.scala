package co.blocke.scalajack

import reflect.runtime.currentMirror
import reflect.runtime.universe._
import scala.collection.concurrent.TrieMap
import scala.reflect.NameTransformer._
import fields._

object Analyzer {

	val readyToEat = new TrieMap[String,Field]()
	val protoRepo  = new TrieMap[String,Field]()

	private val ru           = scala.reflect.runtime.universe
	private val mongoType    = ru.typeOf[MongoKey]
	private	val xtractTypes  = """.*\[(.*)\]""".r

	private val typeList = List("String","Int","Long","Float","Double","Boolean","Char")
	private def typeMap( fManifest:Manifest[_] ) = {
		val ftype = fManifest.toString
		ftype match {
			case "java.lang.String"  => (n:String) => StringField( n, false )
			case "Int"     => (n:String) => IntField( n, false    )
			case "Long"    => (n:String) => LongField( n, false   )
			case "Float"   => (n:String) => FloatField( n, false  )
			case "Double"  => (n:String) => DoubleField( n, false )
			case "Boolean" => (n:String) => BoolField( n, false   )
			case "Char"    => (n:String) => CharField( n, false   )
			case t         => (n:String) => Analyzer(t)(fManifest).asInstanceOf[CaseClassField].copy(name = n)
		}
	}

	private[scalajack] def apply[T]( cname:String )(implicit m:Manifest[T]) : Field = resolve( _apply( cname ) )

	private def _apply( cname:String ) : Field = {
		val symbol   = currentMirror.classSymbol(Class.forName(cname))
		val typeArgs = symbol.typeParams.map( tp => tp.name.toString)
		val staticName  = cname + typeArgs.mkString("[",",","]")
		readyToEat.get( staticName ).orElse( protoRepo.get( staticName ) ).orElse({
			val v = staticReflect( "", symbol.typeSignature )
			v match {
				case ccp:CaseClassProto => protoRepo.put(staticName, v) // Parameterized type goes into protoRepo
				case ccF:CaseClassField => readyToEat.put(staticName, v) // Simple case: No type args
			}
			Some(v)
		}).get
	}

	private def resolve[T]( field:Field )(implicit m:Manifest[T]) : Field = {
		field match {
			case ccf : CaseClassField => ccf
			case ccp : CaseClassProto => {
				// Get runtime type parameters
				val rtArgs = m.typeArguments
				// See if this proto has already been resolved
				val rtKeyName = ccp.className + rtArgs.map(_.toString).mkString("[",",","]")
				readyToEat.get( rtKeyName ).fold( {
					val argMap = ccp.typeArgs.zip( rtArgs ).toMap
println("ArgMap: "+argMap)
					// Not there... resolve it
					val fields = ccp.fields.map( _ match {
						case tf:TypeField      => typeMap( argMap(tf.symbol) )(tf.name)
						case cp:CaseClassProxy => {
							val xtractTypes(terms) = ccp.dt.typeSymbol.typeSignature.member(currentMirror.universe.newTermName(cp.name)).typeSignature.toString
							val runtimeTypes = terms.split(",").toList
//							println("RT: "+rtArgs+ "  "+ccp.typeArgs) // java.lang.String, Int, Boolean
							println("R2: "+runtimeTypes)  // String, scala.Int, ...
							// println("Proto Types: "+cp.proto.typeArgs)
							// println("Proto Fields: "+cp.proto.fields)
//							println("Mystery: "+ru..RuntimeMirror.reflectClass(cp.symbol))
							cp//resolve( cp.proto )(argMap(tf.symbol))
						}
						case f                 => f
						})
					val cf = CaseClassField( "", ccp.dt, ccp.className, ccp.applyMethod, fields, ccp.caseObj )
					readyToEat.put( rtKeyName, cf )
					cf
				})( c => c.asInstanceOf[CaseClassField] )
			}
		}
	}

	private def staticReflect( fieldName:String, ctype:Type, inContainer:Boolean = false, classCompanionSymbol:Option[Symbol] = None) : Field = {

		val fullName = ctype.typeSymbol.fullName.toString

		fullName match {
			case "scala.collection.immutable.List" =>
				ctype match {
					case TypeRef(pre, sym, args) => ListField(fieldName, staticReflect( fieldName, args(0), true ))
				}

			case "scala.Enumeration.Value" =>
				val erasedEnumClass = Class.forName(ctype.asInstanceOf[TypeRef].toString.replace(".Value","$"))
				val enum = erasedEnumClass.getField(MODULE_INSTANCE_NAME).get(null).asInstanceOf[Enumeration]
				EnumField( fieldName, enum)

			case "scala.Option" =>
				val subtype = ctype.asInstanceOf[TypeRef].args(0)
				// Facilitate an Option as a Mongo key part (a very bad idea unless you are 100% sure the value is non-None!!!)
				val subField = staticReflect(fieldName, subtype, true, classCompanionSymbol)
				OptField( fieldName, subField, subField.hasMongoAnno )

			case "scala.collection.immutable.Map" =>
				val valuetype = ctype.asInstanceOf[TypeRef].args(1)
				MapField( fieldName, staticReflect(fieldName, valuetype, true) )

			case _ =>
				val sym = currentMirror.classSymbol(Class.forName(fullName))
				// --------------- Trait
				if( sym.isTrait && !fullName.startsWith("scala"))
					TraitField( fieldName )
				// --------------- Case Class
				else if( sym.isCaseClass ) {
					val typeArgs = { 
						if( ctype.takesTypeArgs ) {
							val poly = ctype.asInstanceOf[PolyType].typeParams
							poly.map( p => p.name.toString )
						} else List[String]()
					}
// println("TA: "+typeArgs)
// println("Ctype: "+ctype)
//println("Member: "+ctype.member(TermName.stringToTermName("w")))
// ctype.members.foreach(t => println(t.getClass.getName))
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
					val fields = constructor.paramss.head.map( c => {
						val fieldTypeName = c.typeSignature.typeConstructor.toString
						if( typeArgs.contains( fieldTypeName ) ) {
							// Placeholder for simple type having type of class' parameter, e.g. case class[X]( stuff:X )
							TypeField( c.name.toString, fieldTypeName ) 
						} else {
							val symbol = {
								if( !typeList.contains(fieldTypeName) ) {
									val clazz = Class.forName(fieldTypeName)
									currentMirror.classSymbol(clazz)
// 	println("Clazz: "+clazz.getFields.toList)
// // Somewhere in here I need to know about Wrap[V,String] so I can build a map associating V -> T, String -> U for later use in resolve!
// // Make this map part of the Proxy object.
// 				println("Types: "+s.typeParams)
// 				println("C: "+c)
								} else c
							}
							staticReflect(c.name.toString, symbol.typeSignature, false, Some(companionSymbol)) match {
								case ccf:CaseClassField => ccf
								case ccp:CaseClassProto => 
									val staticName = fullName + typeArgs.mkString("[",",","]")
									val useProto = protoRepo.get( staticName ).fold( { protoRepo.put(staticName,ccp); ccp } )( p => p.asInstanceOf[CaseClassProto] )
									CaseClassProxy(c.name.toString, useProto, symbol.asInstanceOf[ClassSymbol])
								case f => f
							}
						}
					})
					if( typeArgs.size > 0 )
						CaseClassProto( ctype, fullName, applyMethod, fields, caseObj, typeArgs )
					else
						CaseClassField( fieldName, ctype, fullName, applyMethod, fields, caseObj )
				// --------------- Simple Types
				} else {
					// See if there's a MongoKey annotation on any of the class' fields
					val mongoAnno = classCompanionSymbol.fold(List[String]())( (cs) => {
						cs.typeSignature.members.collectFirst {
							case method:MethodSymbol if( method.name.toString == "apply") => method.paramss.head.collect{ case p if( p.annotations.find(a => a.tpe == Analyzer.mongoType).isDefined) => p.name.toString }
						}.getOrElse(List[String]())
					})
					fullName match {
						case "java.lang.String" => StringField( fieldName, mongoAnno.contains(fieldName) )
						case "scala.Int"        => IntField(    fieldName, mongoAnno.contains(fieldName) )
						case "scala.Char"       => CharField(   fieldName, mongoAnno.contains(fieldName) )
						case "scala.Long"       => LongField(   fieldName, mongoAnno.contains(fieldName) )
						case "scala.Float"      => FloatField(  fieldName, mongoAnno.contains(fieldName) )
						case "scala.Double"     => DoubleField( fieldName, mongoAnno.contains(fieldName) )
						case "scala.Boolean"    => BoolField(   fieldName, mongoAnno.contains(fieldName) )
						case "org.bson.types.ObjectId" => ObjectIdField( fieldName )
						case _                  => {
							if( isValueClass(sym) ) {
								val clazz = Class.forName(fullName)
								// Class name transformation so Analyzer will work
								val className = clazz.getDeclaredFields.head.getType.getName match {
									case "int"     => "scala.Int"
									case "char"    => "scala.Char"
									case "long"    => "scala.Long"
									case "float"   => "scala.Float"
									case "double"  => "scala.Double"
									case "boolean" => "scala.Boolean"
									case t         => t
								}
								if( inContainer )
									ValueClassField( fieldName, mongoAnno.contains(fieldName), Analyzer._apply( className ), clazz.getConstructors()(0), findExtJson(fullName) ) //, clazz.getConstructors.toList.head )
								else 
									ValueClassFieldUnboxed( fieldName, mongoAnno.contains(fieldName), Analyzer._apply( className ), findExtJson(fullName) ) //, clazz.getConstructors.toList.head )
							} else
								throw new IllegalArgumentException("Unknown/unsupported data type: "+fullName)
						}
					} 
				}
		}
	}
	
	// Pulled this off Stackoverflow... Not sure if it's 100% effective, but seems to work!
	private def isValueClass( sym:ClassSymbol ) = sym.asType.companionSymbol.typeSignature.members.exists(_.name.toString.endsWith("$extension"))

	//--------------- Extended JSON support

	private val classLoaders = List(this.getClass.getClassLoader)
	private val ModuleFieldName = "MODULE$"

	private def findExtJson(cname:String) : Option[ExtJson] = {
		val clazz = Class.forName(cname)
		val path = if (clazz.getName.endsWith("$")) clazz.getName else "%s$".format(clazz.getName)
		val c = resolveClass(path, classLoaders)
		if (c.isDefined) {
			val co = c.get.getField(ModuleFieldName).get(null)
			if( co.isInstanceOf[ExtJson] ) Some(co.asInstanceOf[ExtJson])
			else None
		}
		else None
	}

	private def resolveClass[X <: AnyRef](c: String, classLoaders: Iterable[ClassLoader]): Option[Class[X]] = classLoaders match {
		case Nil      => sys.error("resolveClass: expected 1+ classloaders but received empty list")
		case List(cl) => Some(Class.forName(c, true, cl).asInstanceOf[Class[X]])
		case many => {
			try {
				var clazz: Class[_] = null
				val iter = many.iterator
				while (clazz == null && iter.hasNext) {
					try {
						clazz = Class.forName(c, true, iter.next())
					} catch {
						case e: ClassNotFoundException => 
					}
				}
				if (clazz != null) Some(clazz.asInstanceOf[Class[X]]) else None
			} catch {
				case _: Throwable => None
			}
		}
	}
}