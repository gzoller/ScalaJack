package co.blocke
package scalajack

import scala.reflect.runtime.currentMirror
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
	primitiveTypes.foreach( p => readyToEat.put(p._1+"[]",PrimType(p._1)) )

	private val ru           = scala.reflect.runtime.universe
	private val dbType       = ru.typeOf[DBKey]
	private val collectType  = ru.typeOf[Collection]

	def inspect[T]( c:T, relativeToTrait:Option[TraitType] = None )(implicit tt:TypeTag[T]) : AType = _inspect[T]( c.getClass, relativeToTrait )

	// Used when we only have the class name
	def inspectByName[T]( className:String, relativeToTrait:Option[TraitType] = None )(implicit tt:TypeTag[T]) : AType = _inspect[T]( Class.forName(className), relativeToTrait )

	private def getParamSymbols( t:Type ) = 
		LinkedHashMap.empty[String,AType] ++= t.typeSymbol.asClass.typeParams.map(_.name.toString).zip( t.typeArgs.map(ta => know(ta,None, true)) ).toMap
  
	private def _inspect[T]( clazz:Class[_], relativeToTrait:Option[TraitType] )(implicit tt:TypeTag[T]) : AType = {
		val ctype = if( relativeToTrait.isDefined ) 
			currentMirror.classSymbol(clazz).typeSignature  // no trait given--use the implied context
		else 
			tt.tpe  // no relative trait given--use the implied context
		know(ctype,relativeToTrait,false)
	}
  
	private[scalajack] def know( 
		t:Type, 
		relativeToTrait:Option[TraitType] = None, 
		isParamType:Boolean = false, 
		preResolved:LinkedHashMap[String,AType] = LinkedHashMap.empty[String,AType]
	) : AType = {
		// Shortcut--in case there's a 0-parameter version we save a few cycles not computing parameters & tag
		readyToEat.getOrElse(t.typeSymbol.fullName+"[]",{
			val (args,argMap) = t match {
				case ty:TypeRef =>
					val buildingArgMap = 
						LinkedHashMap.empty[String,AType] ++= ty.typeSymbol.typeSignature.typeParams
							.map(_.name.toString)
							.zip(ty.args.map(ta => preResolved.getOrElse(ta.toString,know(ta,None,true,preResolved))))
					(ty.args, buildingArgMap)
				case ty => // generally a case class that's implementing a trait
					(List.empty[Type], relativeToTrait.get.paramMap)
			}
			val tag = t.typeSymbol.fullName+argMap.values.map(_.name).mkString("[",",","]")
			readyToEat.getOrElse(tag,{
				t.typeSymbol match {
					case sym if(sym.isPrimitive)         =>  // Should Never Happen(tm)
						val pt = PrimType(sym.fullName)
						readyToEat.put(tag,pt)
						pt

					case sym if(sym.isCollection)        =>
						CollType( sym.fullName, argMap.values.toList)

					case sym if(sym.asClass.isTrait)     =>
						val members  = t.members.filter(_.isTerm).map(_.asMethod).filter(_.isGetter)
						// mappedParams = Map[ field_name -> field_type (AType) ]
						val mappedParams = LinkedHashMap.empty[String,AType] ++= 
							members.map(_.name.toString).zip( members.map(_.typeSignature.resultType) )
								.collect{
									case (item,itemType) if(argMap.contains(itemType.toString)) => (item,argMap(itemType.toString)) 
									case (item,itemType) => (item,know(itemType))
								}.toList
						val tty = TraitType(sym.fullName, mappedParams)
						readyToEat.put(tag, tty)
						tty
		        
					case sym if(sym.asClass.isCaseClass) =>
						val ctor         = sym.asClass.primaryConstructor
						val collAnnoName = sym.asClass.annotations.find(_.tree.tpe =:= typeOf[Collection]).map(_.tree.children.tail.head.productElement(1).toString)
						val members      = ctor.typeSignature.paramLists.head.map( f => {
							val fType = relativeToTrait.flatMap( _.paramMap.get(f.name.toString) )
								.orElse( Some(argMap.getOrElse(f.typeSignature.toString, know(f.typeSignature,None,false,argMap) )) )
							val finalFtype = fType.get match {
								case ft:AType if(f.annotations.find(_.tree.tpe =:= typeOf[DBKey]).isDefined) => {
									val ft2 = ft.dup
									ft2._isDbKey = true
									ft2
								}
								case ft => ft
							}
							(f.name.toString, finalFtype)
						})
						val cc = CCType( sym.fullName, LinkedHashMap.empty[String,AType] ++= members, argMap, None, collAnnoName.map(_.filterNot(_ == '"')) )
						if( cc.paramMap.size == 0 )  // For simplicity's sake, don't cache cc's having parameters.  Too many nuances, e.g. parameterized types
							readyToEat.put(tag, cc)
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
							case pa if( pa.isEmpty ) => know(sym.asClass.primaryConstructor.asMethod.paramLists.head.head.info) // No type params on VC
							case pa => 
								val vTerm = sym.asClass.primaryConstructor.typeSignature.paramLists.head.head.asTerm.info.resultType // Don't ask...its magic.
								val g = sym.asClass.primaryConstructor.asMethod.paramLists.head.head.info
								if( g.typeSymbol.isClass && g.typeSymbol.asClass.isCollection )
									CollType( g.typeSymbol.asClass.fullName, argMap.values.toList )
								else
									argMap.getOrElse(vTerm.toString, know(sym.asClass.primaryConstructor.asMethod.paramLists.head.head.info))
						}
						ValueClassType(sym.fullName, valSjType, vField.name.toString, isParamType)

					case sym                             => ErrType()
				}
			})
		})
	}
}