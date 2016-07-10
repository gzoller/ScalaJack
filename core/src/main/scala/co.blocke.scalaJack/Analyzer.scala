package co.blocke
package scalajack

import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.universe._
import scala.reflect.NameTransformer._
import scala.collection.mutable.LinkedHashMap
import scala.collection.concurrent.TrieMap
import PrimitiveTypes._

object Analyzer {
  
	private val readyToEat = TrieMap.empty[String,AType]  // a cache, sir, a cache

	// Simple way to allow extension modules like Mongo to hook in custom types.
	private[scalajack] def addType( name:String, newType:AType ) = readyToEat.put(name+"[]", newType)

	// pre-populate cache with all the primitives
	primCodes.foreach( p => readyToEat.put(p._1+"[]",PrimType(p._1)))

	private val ru           = scala.reflect.runtime.universe
	private val dbType       = ru.typeOf[DBKey]
	private val collectType  = ru.typeOf[Collection]

	def inspect[T]( c:T, relativeToTrait:Option[TraitType] = None )(implicit tt:TypeTag[T]) : AType = _inspect[T]( c.getClass, relativeToTrait )

	// Used when we only have the class name
	def inspectByName[T]( className:String, relativeToTrait:Option[TraitType] = None )(implicit tt:TypeTag[T]) : AType = _inspect[T]( Class.forName(className), relativeToTrait )

	private def getParamSymbols( t:Type ) = 
		LinkedHashMap.empty[String,AType] ++= t.typeSymbol.asClass.typeParams.map(_.name.toString).zip( t.typeArgs.map(ta => know(ta.dealias,None, true)) ).toMap
  
	private def _inspect[T]( clazz:Class[_], relativeToTrait:Option[TraitType] )(implicit tt:TypeTag[T]) : AType = {
		val ctype = if( relativeToTrait.isDefined ) 
			cm.classSymbol(clazz).typeSignature  // no trait given--use the implied context
		else 
			tt.tpe  // no relative trait given--use the implied context
		know(ctype,relativeToTrait,false)
	}

	// Used to create unique tag name for a type.  Explodes all type param arguments where applicable.
	private def argExplode( a:AType ) : String = a match {
		case atype:PrimType       => atype.name
		case atype:CollType       => atype.name+atype.colTypes.map(argExplode(_)).mkString("[",",","]")
		case atype:CCType         => atype.name+atype.paramMap.values.map(argExplode(_)).mkString("[",",","]")
		case atype:EnumType       => atype.name
		case atype:ValueClassType => atype.name
		case atype:TraitType      => atype.name+atype.paramMap.values.map(argExplode(_)).mkString("[",",","]")
	}
  
	private[scalajack] def know( 
		t:Type, 
		relativeToTrait:Option[TraitType] = None, 
		isParamType:Boolean = false, 
		preResolved:LinkedHashMap[String,AType] = LinkedHashMap.empty[String,AType]
	) : AType = this.synchronized {
		// NOTE: Had to add synchronized here because we're adding an "empty" case class type into readyToEat first,
		// then building case class method field lists as we go.  THis is to support self-referencing.
		// If other threads see a not-yet-complete in readyToEat it will blow up.  This sync ensures we wait
		// until any other threads that may be working to analyze a case class has finished and the type is complete.

		// Shortcut--in case there's a 0-parameter version we save a few cycles not computing parameters & tag
		readyToEat.getOrElse(t.typeSymbol.fullName+"[]",{
			val (args,argMap) = t.dealias match {
				case ty:TypeRef =>
					val buildingArgMap = 
						LinkedHashMap.empty[String,AType] ++= ty.typeSymbol.typeSignature.typeParams
							.map(_.name.toString)
							.zip(ty.args.map(ta => preResolved.getOrElse(ta.toString,know(ta.dealias,None,true,preResolved))))
					(ty.args, buildingArgMap)
				case ty => // generally a case class that's implementing a trait
					(List.empty[Type], relativeToTrait.get.paramMap)
			}
			val tag = t.typeSymbol.fullName+argMap.values.map(argExplode(_)).mkString("[",",","]")
			readyToEat.getOrElse(tag,{
				t.typeSymbol match {
					// case sym if(sym.isPrimitive)         =>  // Should Never Happen(tm)  -- Primitives are pre-loaded into readyToEat!
					// 	val pt = PrimType(sym.fullName)
					// 	readyToEat.put(tag,pt)
					// 	pt

					case sym if(sym.isCollection)        =>
						CollType( sym.fullName, argMap.values.toList )

					case sym if(sym.asClass.isTrait)     =>
						val members  = t.members.filter(_.isTerm).map(_.asMethod).filter(_.isGetter)
						// mappedParams = Map[ field_name -> field_type (AType) ]
						val mappedParams = LinkedHashMap.empty[String,AType] ++= 
							members.map(_.name.toString).zip( members.map(_.typeSignature.resultType) )
								.collect{
									case (item,itemType) if(argMap.contains(itemType.toString)) => (item,argMap(itemType.toString)) 
									case (item,itemType) => (item,know(itemType.dealias))
								}.toList
						val tty = TraitType(sym.fullName, mappedParams, argMap)
						readyToEat.put(tag, tty)
						tty
		        
					case sym if(sym.asClass.isCaseClass) =>
						val collAnnoName = sym.asClass.annotations.find(_.tree.tpe =:= typeOf[Collection]).map(_.tree.children.tail.head.productElement(1).toString)

						// Create (and possibly cache) cc here in case we have self-referencing members or derivatives (e.g. collections)
						// That way we don't spin out of control with stack overflow.  We then add members to the created cc.
						val cc = CCType( sym.fullName, LinkedHashMap.empty[String,(AType,Option[Any])], argMap, None, collAnnoName.map(_.filterNot(_ == '"')) )
						readyToEat.put(tag, cc)

						val mod     = sym.asClass.companion.asModule
						val im      = cm.reflect(cm.reflectModule(mod).instance)
						val ts      = im.symbol.typeSignature
						val mApply  = ts.members.toList.find(m => m.name.toString == "apply" && m.isSynthetic).get.asMethod
						val syms    = mApply.paramLists.flatten
						val members = syms.zipWithIndex.map { case (p, i) =>
							val fType = relativeToTrait.flatMap( _.members.get(p.name.toString) )
								.orElse( {
									if(p.typeSignature.toString == cc.name)
										Some(cc)
									else 
										Some(argMap.getOrElse(p.typeSignature.toString, know(p.typeSignature.dealias,None,false,argMap) )) 
									})
							val defaultVal = {
								val found = ts.member(TermName("apply$default$"+(i+1)))
								if(found.isMethod) 
									Some(im.reflectMethod(found.asMethod)())
								else
									None
							}
							val finalFtype = fType.get match {
								case ft:AType if(p.annotations.find(_.tree.tpe =:= typeOf[DBKey]).isDefined) => {
									val ft2 = ft.dup
									ft2._isDbKey = true
									ft2
								}
								case ft => ft
							}
          					(p.name.toString, (finalFtype,defaultVal))
						}
						cc.members ++= members
						cc

					case sym if(sym.asClass.fullName == "scala.Enumeration.Value") =>
						val erasedEnumClassName = t.toString match {
							case raw if(raw.endsWith(".Value")) => raw.replace(".Value","$")
							case raw                            => raw.dropRight(raw.length - raw.lastIndexOf('.')) + "$"
						}
						EnumType(sym.fullName, 
							Class.forName(erasedEnumClassName).getField(scala.reflect.NameTransformer.MODULE_INSTANCE_NAME).get(null).asInstanceOf[Enumeration])

					case sym if(sym.asClass.isDerivedValueClass) =>
						val vField = sym.asClass.primaryConstructor.typeSignature.paramLists.head.head
						val valSjType = args match {
							case pa if( pa.isEmpty ) => know(sym.asClass.primaryConstructor.asMethod.paramLists.head.head.info.dealias) // No type params on VC
							case pa => 
								val vTerm = sym.asClass.primaryConstructor.typeSignature.paramLists.head.head.asTerm.info.resultType // Don't ask...its magic.
								val g = sym.asClass.primaryConstructor.asMethod.paramLists.head.head.info
								if( g.typeSymbol.isClass && g.typeSymbol.asClass.isCollection )
									CollType( g.typeSymbol.asClass.fullName, argMap.values.toList )
								else
									argMap.getOrElse(vTerm.toString, know(sym.asClass.primaryConstructor.asMethod.paramLists.head.head.info.dealias))
						}
						ValueClassType(sym.fullName, valSjType, vField.name.toString, isParamType)

					case sym                             => ErrType()
				}
			})
		})
	}
}