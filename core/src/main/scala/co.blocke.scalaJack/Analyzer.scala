package co.blocke
package scalajack

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._
import scala.reflect.NameTransformer._
import scala.collection.mutable.LinkedHashMap
import scala.collection.concurrent.TrieMap
import PrimitiveTypes._

trait AType {
  val name   : String
}
case class CCType(
    name     : String, 
    members  : LinkedHashMap[String,AType], 
    paramMap : LinkedHashMap[String,AType] = LinkedHashMap.empty[String,AType],
    isTrait  : Boolean = false
) extends AType {
  override def toString() = s"[$name -> $members]"
}
case class PrimType(name:String) extends AType
case class CollType(name:String, colTypes:List[AType]) extends AType {
  def isOptional = name == "scala.Option"
}
case class EnumType(name:String, enum:Enumeration) extends AType
case class ValueClassType(name:String, vcType:AType, vFieldName:String, isTypeParam:Boolean) extends AType
case class TraitType(name:String, paramMap:LinkedHashMap[String,AType] = LinkedHashMap.empty[String,AType]) extends AType
case class ErrType(name:String = "Error") extends AType

object Analyzer {
  
  private val readyToEat = TrieMap.empty[String,AType]  // a cache, sir, a cache

  def inspect[T]( c:T, relativeToTrait:Option[TraitType] = None )(implicit tt:TypeTag[T]) : AType = _inspect[T]( c.getClass, relativeToTrait )

  // Used when we only have the class name
  def inspectByName[T]( className:String, relativeToTrait:Option[TraitType] = None )(implicit tt:TypeTag[T]) : AType = _inspect[T]( Class.forName(className), relativeToTrait )

   private def getParamSymbols( t:Type ) = 
     LinkedHashMap.empty[String,AType] ++=
      t.typeSymbol.asClass.typeParams.map(_.name.toString).zip( t.typeArgs.map(ta => know(ta,None, true)) ).toMap
  
  private def _inspect[T]( clazz:Class[_], relativeToTrait:Option[TraitType] )(implicit tt:TypeTag[T]) : AType = {
    val ctype = if( relativeToTrait.isDefined ) 
        currentMirror.classSymbol(clazz).typeSignature  // no trait given--use the implied context
      else 
        tt.tpe  // no relative trait given--use the implied context
    know(ctype,relativeToTrait,false)
   }
  
   def know( 
       t:Type, 
       relativeToTrait:Option[TraitType] = None, 
       isParamType:Boolean = false, 
       preResolved:LinkedHashMap[String,AType] = LinkedHashMap.empty[String,AType]
   ) : AType = {
    val (args,argMap) = t match {
      case ty:TypeRef =>
        val buildingArgMap = 
            LinkedHashMap.empty[String,AType] ++= 
              ty.typeSymbol.typeSignature.typeParams
                .map(_.name.toString)
                .zip(ty.args.map(ta => preResolved.getOrElse(ta.toString,know(ta,None,true,preResolved))))
        (ty.args, buildingArgMap)
      case ty => // generally a case class that's implementing a trait
        (List.empty[Type], relativeToTrait.get.paramMap)
    }
    val tag    = t.typeSymbol.fullName+argMap.values.map(_.name).mkString("[",",","]")
    readyToEat.getOrElse(tag,{

      t.typeSymbol match {
        case sym if(sym.isPrimitive)         => PrimType(sym.fullName)
        
        case sym if(sym.isCollection)        =>
          CollType( sym.fullName, argMap.values.toList) //args.map(a => know(a,None,true)) )
          
        case sym if(sym.asClass.isTrait)     =>
          val members  = t.members.filter(_.isTerm).map(_.asMethod).filter(_.isGetter)
          // mappedParams = Map[ field_name -> field_type (AType) ]
          val mappedParams = LinkedHashMap.empty[String,AType] ++= 
              members.map(_.name.toString).zip( members.map(_.typeSignature.resultType) )
              .collect{
                case (item,itemType) if(argMap.contains(itemType.toString)) => (item,argMap(itemType.toString)) 
                case (item,itemType) => (item,know(itemType))
              }.toList
          TraitType(sym.fullName, mappedParams)
        
        case sym if(sym.asClass.isCaseClass) =>
          val ctor    = sym.asClass.primaryConstructor
          val members = ctor.typeSignature.paramLists.head.map( f => {
            val fType = relativeToTrait.flatMap( _.paramMap.get(f.name.toString) )
              .orElse( Some(argMap.getOrElse(f.typeSignature.toString, {
//                val greg:List[AType] = f.typeSignature.typeArgs.map(ta => argMap.getOrElse(ta.toString,know(ta,None,true)))
//                know(f.typeSignature, f.typeSignature.typeArgs.map(ta => argMap.getOrElse(ta.toString,know(ta,None,true))))
                if(f.typeSignature.typeSymbol.isClass && f.typeSignature.typeSymbol.asClass.isCollection ) {
                  know(f.typeSignature,None,false,argMap)
//                  CollType( f.typeSignature.typeSymbol.fullName, argMap.values.toList )
//                  CollType( f.typeSignature.typeSymbol.fullName, argMap.values.toList )
                } else
                  know(f.typeSignature,None,false,argMap)
              })) )
            (f.name.toString, fType.get)
          })
          CCType( 
              sym.fullName, 
              LinkedHashMap.empty[String,AType] ++= members,
              argMap ) 
              
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
            case pa if( pa.length == 0 ) => know(sym.asClass.primaryConstructor.asMethod.paramLists.head.head.info) // No type params on VC
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
  }
}