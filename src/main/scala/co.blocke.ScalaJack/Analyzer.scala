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
	private	val xtractTypes  = """.*?\[(.*?)\]""".r
	private val xtractSingle = """[+-]*([a-zA-Z\.\[\]]+).*""".r
	private def typeSplit( raw:String ) = {
		val xtractTypes(stg2) = raw
		stg2.split(",").toList.map( x => {
			val xtractSingle(stg3) = x.trim
			stg3
		})
	}

	private val typeList = List("String","Int","Long","Float","Double","Boolean","Char",
		"scala.List","Map","scala.Map","scala.Option", "scala.Enumeration.Value")
	private def convertType( t:String ) = t match {
		case "Boolean" => "scala.Boolean"
		case "Int"     => "scala.Int"
		case "Long"    => "scala.Long"
		case "Float"   => "scala.Float"
		case "Double"  => "scala.Double"
		case "Char"    => "scala.Char"
		case "String"  => "java.lang.String"
		case x         => x
	}
	private def typeMap( dt:String ) = { 
		dt match {
			case "String"        | "java.lang.String" => (n:String) => StringField( n, false )
			case "scala.Int"     | "Int"              => (n:String) => IntField( n, false    )
			case "scala.Long"    | "Long"             => (n:String) => LongField( n, false   )
			case "scala.Float"   | "Float"            => (n:String) => FloatField( n, false  )
			case "scala.Double"  | "Double"           => (n:String) => DoubleField( n, false )
			case "scala.Boolean" | "Boolean"          => (n:String) => BoolField( n, false   )
			case "scala.Char"    | "Char"             => (n:String) => CharField( n, false   )
			case t         => (n:String) => {
					val pos = t.indexOf('[')
					val applied = { 
						if( pos < 0 )
							_apply( convertType(t) ) 
						else 
							_apply( convertType(t.take(pos)), Some(t))
					} 
					applied match {
						case vc :ValueClassField        => vc.copy(name = n)
						case vc2:ValueClassFieldUnboxed => vc2.copy(name = n)
						case cc :CaseClassField         => cc.copy(name = n)
						case op :OptField               => op.copy(name = n)
						case tt :TraitField             => tt.copy(name = n)

						// OK, this one's wierd... It supports a parameter that is itself a parameterized type Foo[Bar[Int]].  Sick, right?
						// Note one limitation: The parameter parsing only goes 1-level, so Foo[Bar[T]] wouldn't likely work.
						case cp :CaseClassProto         => {
							val argMap = cp.typeArgs.zip( typeSplit( t ) ).toMap
							resolve(cp, argMap, t, Some(n))  // resolve returns CaseClassField
						}
						case tp :TraitProto         => {
							val argMap = tp.typeArgs.zip( typeSplit( t ) ).toMap
							resolve(tp, argMap, t, Some(n))  // resolve returns TraitField
						}
					}
				}
		}
	}

	private[scalajack] def apply[T]( cname:String, args:List[String] = List[String]() )(implicit m:Manifest[T]) : Field = {
		// Normally we get the runtime type args from the manifest...except for a parameterized trait.  In this case
		// there is another level of type param indirection, so we pass in the runtime types, which were already resolved
		// when the TypeField was resolved.
		val rtArgs = {
			if( args.length == 0 )
				m.typeArguments.map(_.toString)
			else 
				args
		}
		val rtKeyName = cname + rtArgs.mkString("[",",","]")
		_apply( cname, Some(rtKeyName) ) match {
			case ccp : CaseClassProto => 
				val argMap = ccp.typeArgs.zip( rtArgs ).toMap
				resolve( ccp, argMap, rtKeyName )
			case ttp : TraitProto =>
				val argMap = ttp.typeArgs.zip( rtArgs ).toMap
				resolve( ttp, argMap, rtKeyName )
			case f                     => f
		}
	}

	private def _apply( cname:String, rtName:Option[String] = None ) : Field = {
		val symbol   = currentMirror.classSymbol(Class.forName(cname))
		val typeArgs = symbol.typeParams.map( tp => tp.name.toString)
		val staticName  = cname + typeArgs.mkString("[",",","]")
 		readyToEat.get( rtName.getOrElse(staticName) ).orElse( protoRepo.get( staticName ) ).orElse({
			val v = staticReflect( "", symbol.typeSignature, List[String](), { if(isValueClass(symbol)) true else false } )
			v match {
				case ccp:CaseClassProto => protoRepo.put(staticName, v) // Parameterized type goes into protoRepo
				case ccf:CaseClassField => readyToEat.put(rtName.getOrElse(staticName), v)    // Simple case: No type args
				case ttp:TraitProto     => protoRepo.put(staticName, v) // Parameterized type goes into protoRepo
				case ttf:TraitField     => readyToEat.put(rtName.getOrElse(staticName), v)    // Simple case: No type args
				case _ =>
			}
			Some(v)
		}).get
	}

	private def resolve( field:Field, argMap:Map[String,String], keyName:String, fieldName:Option[String] = None ) : Field = {
		field match {
			//case ccf : CaseClassField => ccf
			case ccp : CaseClassProto => {
				readyToEat.get( keyName ).fold( {
					// Not there... resolve it
					val fields = ccp.fields.map( _ match {
						case tf:TypeField      => resolveTypeField( tf, argMap )
						case cp:CaseClassProxy => 
							val runtimeTypes = typeSplit( ccp.dt.typeSymbol.typeSignature.member(currentMirror.universe.newTermName(cp.name)).typeSignature.toString )
							val symMap = cp.proto.typeArgs.zip( runtimeTypes.map( rtt => argMap.get(rtt).fold(rtt)(c => c.toString) ) ).toMap
							resolve( cp.proto, symMap, "_bogus_", Some(cp.name))
						case lf:ListField      => 
							lf.subField match {
								case tf:TypeField => ListField( lf.name, resolveTypeField( tf, argMap ) )
								case _            => lf
							}
						case mf:MapField       => 
							mf.valueField match {
								case tf:TypeField => MapField( mf.name, resolveTypeField( tf, argMap ) )
								case _            => mf
							}
						case of:OptField       =>
							of.subField match {
								case tf:TypeField => OptField( of.name, resolveTypeField( tf, argMap ) )
								case _            => of
							}
						case tt:TraitProxy     => 
							val runtimeTypes = typeSplit( ccp.dt.typeSymbol.typeSignature.member(currentMirror.universe.newTermName(tt.name)).typeSignature.toString )
							val symMap = tt.proto.typeArgs.zip( runtimeTypes.map( rtt => argMap.get(rtt).fold(rtt)(c => c.toString) ) ).toMap
							resolve( tt.proto, symMap, "_bogus_", Some(tt.name))
						case f                 => f
						})
					val cf = CaseClassField( fieldName.getOrElse(""), ccp.dt, ccp.className, ccp.applyMethod, fields, ccp.caseObj )
					if( keyName != "_bogus_")
						readyToEat.put( keyName, cf )
					cf
				})( c => c.asInstanceOf[CaseClassField] )
			}
			case tt : TraitProto => 
				readyToEat.get( keyName ).fold (
					TraitField( fieldName.getOrElse(""), tt.typeArgs.map( a => argMap(a)) )
				)( c => c.asInstanceOf[TraitField] )
			case f => f
		}
	}

	private def resolveTypeField( tf:TypeField, argMap:Map[String,String] ) = {
		val mappedType = argMap(tf.symbol)
		// Do something cool here if argMap(tf.symbol) is a collection, otherwise by default it tries to handle
		// things as a case class, which is _not_ what we want!
		if( mappedType startsWith "scala.collection.immutable.List" ) {
			val xtractTypes(innerSymbol) = mappedType
			ListField( tf.name, typeMap( innerSymbol )("") )
		}
		else if( mappedType startsWith "scala.collection.immutable.Map" ) {
			val mapTypes = typeSplit( mappedType )
			MapField( tf.name, typeMap(mapTypes(1))("") )
		}
		else if( mappedType.startsWith("scala.Some") || mappedType.startsWith("scala.Option") ) {
			val xtractTypes(innerSymbol) = mappedType
			OptField( tf.name, typeMap( innerSymbol )("") )
		}
		else
			typeMap( mappedType )(tf.name)
	}

	private def staticReflect( 
		fieldName:String,   // Name of the field.  "" for top-level case classes and sometimes things inside a container
		ctype:Type,         // Reflected Type of this field
		caseClassParams:List[String] = List[String](), // top-level case class' parameter labels
		inContainer:Boolean = false,  // Set true when object lives inside a container (List, Map, etc.) -- supresses field name
		classCompanionSymbol:Option[Symbol] = None // if case class, needed to determine if fields have mongo key annotation
	) : Field = {

		val fullName = ctype.typeSymbol.fullName.toString

		fullName match {
			case "scala.collection.immutable.List" =>
				ctype match {
					case TypeRef(pre, sym, args) => 
						if( caseClassParams.contains(args(0).toString) )
							ListField(fieldName, TypeField("", args(0).toString))
						else
							ListField(fieldName, staticReflect( fieldName, args(0), caseClassParams, true ))
				}

			case "scala.Enumeration.Value" =>
				val erasedEnumClass = Class.forName(ctype.asInstanceOf[TypeRef].toString.replace(".Value","$"))
				val enum = erasedEnumClass.getField(MODULE_INSTANCE_NAME).get(null).asInstanceOf[Enumeration]
				EnumField( fieldName, enum)

			case "scala.Option" =>
				if( ctype.getClass.getName == "scala.reflect.internal.Types$PolyType" ) {  // support parameterized Option
					val subtype = ctype.asInstanceOf[PolyType].typeParams(0).name.toString
					OptField( fieldName, TypeField("", subtype ) )
				} else {
					val subtype = ctype.asInstanceOf[TypeRef].args(0)
					// Facilitate an Option as a Mongo key part (a very bad idea unless you are 100% sure the value is non-None!!!)
					if( caseClassParams.contains(subtype.toString) )
						OptField( fieldName, TypeField("", subtype.toString ) )
					else {
						val subField = staticReflect(fieldName, subtype, caseClassParams, true, classCompanionSymbol)
						OptField( fieldName, subField, subField.hasMongoAnno )
					}
				}

			case "scala.collection.immutable.Map" => 
				if( caseClassParams.contains( ctype.asInstanceOf[TypeRef].args(1).toString ) )
					MapField( fieldName, TypeField("",ctype.asInstanceOf[TypeRef].args(1).toString) )
				else
					MapField( fieldName, staticReflect(fieldName, ctype.asInstanceOf[TypeRef].args(1), caseClassParams, true) )

			case _ =>
				val sym = currentMirror.classSymbol(Class.forName(fullName))
				// --------------- Trait
				if( sym.isTrait && !fullName.startsWith("scala")) {
					val typeArgs = { 
						if( ctype.takesTypeArgs ) {
							val poly = ctype.asInstanceOf[PolyType].typeParams
							poly.map( p => p.name.toString )
						} else List[String]()
					}
					if( typeArgs.size == 0) 
						TraitField( fieldName )
					else if( classCompanionSymbol.isEmpty )
						TraitProto( typeArgs )
					else {
						val staticName = fullName + typeArgs.mkString("[",",","]")
						val tp = TraitProto( typeArgs )
						val proto = protoRepo.get( staticName ).fold( { protoRepo.put(staticName,tp); tp } )( p => p.asInstanceOf[TraitProto] )
						TraitProxy( fieldName, proto )
					}
				}
				// --------------- Case Class
				else if( sym.isCaseClass ) {
					val typeArgs = { 
						if( ctype.takesTypeArgs ) {
							val poly = ctype.asInstanceOf[PolyType].typeParams
							poly.map( p => p.name.toString )
						} else List[String]()
					}
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
								if( typeList.contains(fieldTypeName) || fieldTypeName.endsWith(".Value") ) 
									c
								else {
									val clazz = Class.forName(fieldTypeName)
									currentMirror.classSymbol(clazz)
								}
							}
							staticReflect(c.name.toString, symbol.typeSignature, typeArgs, false, Some(companionSymbol)) match {
								case ccf:CaseClassField => ccf
								case ccp:CaseClassProto => 
									val staticName = fullName + typeArgs.mkString("[",",","]")
									val useProto = protoRepo.get( staticName ).fold( { protoRepo.put(staticName,ccp); ccp } )( p => p.asInstanceOf[CaseClassProto] )
									CaseClassProxy(c.name.toString, useProto)
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