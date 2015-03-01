package co.blocke.scalajack

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._
import PrimitiveTypes._
import scala.collection.concurrent.TrieMap

object Analyzer {

	private val readyToEat = TrieMap.empty[String,SjType]  // a cache, sir, a cache
	private val resolved   = TrieMap.empty[String,SjType]  // case classe proxies with type params resolved

	// Used when we have an actual instance of a class to inspect
	def inspect[T]( c:T )(implicit tt:TypeTag[T]) : SjType = _inspect[T]( c.getClass )

	// Used when we only have the class name
	def inspect[T]( className:String )(implicit tt:TypeTag[T]) : SjType = _inspect[T]( Class.forName(className) )

	private def _inspect[T]( clazz:Class[_] )(implicit tt:TypeTag[T]) : SjType = {
		val ctype = currentMirror.classSymbol(clazz).typeSignature
		readyToEat.getOrElse(clazz.getName, {
			staticScan(ctype).asInstanceOf[SjType]
		}) match {
			case cc:SjCaseClassProxy  => resolveTypeParams(cc, tt.tpe)
			case cc:SjValueClassProxy => resolveTypeParams(cc, tt.tpe)
			case cc                   => cc
		}
	}

	// For inspecting naked collections (type args must be captured top-level or be lost!)
	def nakedInspect[T](typeArgs:List[Type]) = typeArgs.map( staticScan(_).asInstanceOf[SjType] )

	private def staticScan( ctype:Type, typePlaceholders:List[String] = List.empty[String] ) : SjItem = 
		ctype.typeSymbol match {
			case s if(s.isPlaceholder(typePlaceholders)) => SjTypeSymbol( s.name.toString )
			case s if(s.isPrimitive)                     => SjPrimitive( s.fullName )
			case s if(s.isCollection)                    =>
				ctype match {
					case p:TypeRef => // embedded collections (e.g. members of a case class)
						val collArgTypes = p.args.map( a => staticScan(a,typePlaceholders) )
						SjCollection( s.fullName, collArgTypes.asInstanceOf[List[SjType]] )
					case p => // naked collections--we can't know the type...caller will have to fill it in!
						SjCollection( PrimitiveTypes.fixPolyCollection(s.fullName).get, List.empty[SjType] )
				}
			case s if(s.asClass.isTrait)                 => 
				val resolvedTypeArgs = ctype.asInstanceOf[TypeRef].args.map( _.toString )
				SjTrait(s.fullName, resolvedTypeArgs)
			case s if(s.asClass.isCaseClass)             =>
				val symbol        = s.asClass
				readyToEat.getOrElse(symbol.fullName, {
					val typeParamArgs = symbol.typeParams.map( tp => tp.name.toString)
					val ctor          = symbol.primaryConstructor
					val fields        = ctor.typeSignature.paramLists.head
					val sjfields      = fields.map( f => 
						SjField(f.name.toString, staticScan( f.typeSignature, typeParamArgs ).asInstanceOf[SjType]) )
					val built = {
						if( typeParamArgs.size == 0 )  // no type parameters to resolve 
							SjCaseClass( s.fullName,  sjfields )
						else // unresolved type parameters
							SjCaseClassProxy( s.fullName, typeParamArgs, sjfields )
					}
					readyToEat.put(symbol.fullName,built)
					built
				}) match {
					case noParams:SjCaseClass      => noParams
					case wParams:SjCaseClassProxy  => resolveTypeParams(wParams, ctype)
					case cc                        => cc  // error!
				}

			case s if(s.asClass.isDerivedValueClass)     => // value class support
				val symbol        = s.asClass
				val typeParamArgs = symbol.typeParams.map( tp => tp.name.toString)
				// Get field name of value member
				val vField = symbol.primaryConstructor.typeSignature.paramLists.head.head.name.toString
				// Get type of the value member (only constructor parameter)
				Analyzer.staticScan(symbol.primaryConstructor.asMethod.paramLists.head.head.info, typeParamArgs).asInstanceOf[SjType] match {
					case vcType:SjTypeSymbol => SjValueClassProxy(symbol.fullName,vcType,vField)
					case vcType              => SjValueClass(symbol.fullName,vcType,vField)
				}

			case s if(s.asClass.fullName == "scala.Enumeration.Value") =>
				val valueName = {
				val raw = ctype.asInstanceOf[TypeRef].toString
				if( raw.endsWith(".Value") )
					raw.replace(".Value","$")
				else
					raw.dropRight(raw.length - raw.lastIndexOf('.')) + "$"
				}
				val erasedEnumClass = Class.forName(valueName)
				val enum = erasedEnumClass.getField(scala.reflect.NameTransformer.MODULE_INSTANCE_NAME).get(null).asInstanceOf[Enumeration]
				SjEnum(s.fullName, enum)

			case s                                       => 
				throw new ReflectException(s"Static reflection failed for symbol ${s.fullName}.")
	} // match
  
	private def resolveTypeParams(sjt:SjType, ctype:Type, params:Map[String,SjType] = Map.empty[String,SjType]) : SjType =
		sjt match {
			case sj:SjCaseClassProxy => 
				ctype match {
					case p:TypeRef => 
						val resolvedTag = sj.name+p.args.map(_.typeSymbol.fullName).mkString("[",",","]")
						resolved.getOrElse(resolvedTag, {
							var allTypesResolved = true
							val resolvedFields = sj.fields.map( f => f.ftype match {
								case ft:SjTypeSymbol =>
									val typos = sj.params.indexOf(ft.name)
									if(p.args(typos).toString() == ft.name) {
										allTypesResolved = false
										f  // still-unresolved type parameter
									} else 
										f.copy(ftype = staticScan(p.args(typos), sj.params).asInstanceOf[SjType])
								case ft =>
									f.copy(ftype = resolveTypeParams(ft, ctype, params))
							})
							if( allTypesResolved ) {
								val cc = SjCaseClass(sj.name, resolvedFields)
								resolved.put(resolvedTag,cc)
								cc
							} else
								sj.copy(fields = resolvedFields)
						})
					case nope      => sj
				}
			case sj:SjValueClassProxy =>
				ctype match {
					case p:TypeRef => 
						val resolvedTag = sj.name+"["+p.args.head.typeSymbol.fullName+"]"
						resolved.getOrElse(resolvedTag, 
							SjValueClass(sj.name, staticScan(p.args(0)).asInstanceOf[SjType], sj.vFieldName)
							)
					case nope      => sj
				}
			case sj:SjCollection => sj.copy( collectionType = sj.collectionType.map( ct => resolveTypeParams(ct, ctype, params) ) )
			case sj => sj
		}
}