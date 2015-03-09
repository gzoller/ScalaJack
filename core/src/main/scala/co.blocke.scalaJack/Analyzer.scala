package co.blocke
package scalajack

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._
import PrimitiveTypes._
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.LinkedHashMap

object Analyzer {

  private val readyToEat = TrieMap.empty[String,SjType]  // a cache, sir, a cache
  private val resolved   = TrieMap.empty[String,SjType]  // case classe proxies with type params resolved

  // Used when we have an actual instance of a class to inspect
  def inspect[T]( c:T, relativeToTrait:Option[SjTrait] = None )(implicit tt:TypeTag[T]) : SjType = _inspect[T]( c.getClass, relativeToTrait )

  // Used when we only have the class name
  def inspectByName[T]( className:String, relativeToTrait:Option[SjTrait] = None )(implicit tt:TypeTag[T]) : SjType = _inspect[T]( Class.forName(className), relativeToTrait )

  private def getParamSymbols( t:Type ) = 
    LinkedHashMap.empty[String,SjType] ++=
      t.typeSymbol.asClass.typeParams.map(_.name.toString).zip( t.typeArgs.map(ta => staticScan(ta).asInstanceOf[SjType]) ).toMap
  
  private def _inspect[T]( clazz:Class[_], relativeToTrait:Option[SjTrait] )(implicit tt:TypeTag[T]) : SjType = {
    val (ctype, paramMap) = 
      if( relativeToTrait.isDefined )
        (currentMirror.classSymbol(clazz).typeSignature, LinkedHashMap.empty[String,SjType])  // no trait given--use the implied context
      else 
        (tt.tpe, getParamSymbols(tt.tpe))  // no trait given--use the implied context
    
    readyToEat.getOrElse(clazz.getName, {
      staticScan(ctype,relativeToTrait,paramMap).asInstanceOf[SjType]
    }) match {
      case cc:SjCaseClassProxy  => 
        if( relativeToTrait.isDefined )
          preRezolve_trait(cc, cc.mappedTraitParams)
        else
          preRezolve_nonTrait( cc, ctype )
      case cc                   => cc
    }
  }

  // For inspecting naked collections (type args must be captured top-level or be lost!)
  def nakedInspect[T](typeArgs:List[Type]) = typeArgs.map( staticScan(_).asInstanceOf[SjType] )

  // Statically scan (reflect) given type and product a SjType object "explaining" the given reflected type.
  // If this type is parameterized (e.g. Foo[A,B]) the parameters will either come from 1) the resolved
  // ctype params of the top-level object (carried down through the descention tree via paramSym2Type),
  // or 2) a given (resolved) trait's parameters.  Typically either relativeToTrait or paramSym2Type
  // will be used for parameterized case classes but not both.  (relativeToTrait if this cc implements
  // a trait and paramSym2Type if not.)
  //
  def staticScan( 
      ctype:Type,                                // Reflected type to be scanned
      relativeToTrait:Option[SjTrait] = None,    // Optional trait this object implements
      paramSym2Type:LinkedHashMap[String,SjType] = LinkedHashMap.empty[String,SjType] // Map (possibly empty) of parameter symbol -> SjType (resolved)
//      xtypePlaceholders:List[String] = List.empty[String] 
  ) : SjItem = 
    ctype.typeSymbol match {
      case s if(s.isPlaceholder(paramSym2Type.keySet.toList)) => SjTypeSymbol( s.name.toString )
      case s if(s.isPrimitive)                     => SjPrimitive( s.fullName )
      case s if(s.isCollection)                    =>
        ctype match {
          case p:TypeRef => // embedded collections (e.g. members of a case class)
            val collTypeParamSyms = p.args.map(_.typeSymbol.asClass.typeParams.map(_.name.toString)) // List(B,C)
            val resolvedParamTypes = p.args.map(_.typeArgs.map(ta => paramSym2Type.getOrElse(ta.toString,{
              val psym2Type = if( paramSym2Type.size > 0 ) paramSym2Type else getParamSymbols(ta)
              staticScan(ta,relativeToTrait,psym2Type).asInstanceOf[SjType]
              })))
            val resolveParamMaps = collTypeParamSyms.zip(resolvedParamTypes).map({case(sym,types) => LinkedHashMap.empty[String,SjType] ++= sym.zip(types).toMap })

            val collArgTypes = p.args.zipWithIndex.map( { case(a,i) => preRezolve_nonTrait(staticScan(a,relativeToTrait,resolveParamMaps(i)).asInstanceOf[SjType],a )})

            SjCollection( s.fullName, collArgTypes.asInstanceOf[List[SjType]] )
          case p => // naked collections--we can't know the type...caller will have to fill it in!
            println("BAD")
            SjCollection( PrimitiveTypes.fixPolyCollection(s.fullName).get, List.empty[SjType] )
        }
      case s if(s.asClass.isTrait)                 => 
        val paramSymbols = s.asClass.typeParams.map(_.name.toString)  // e.g. Foo[A,B]  A & B are the type symbols
        //val typeArgs = ctype.typeParams.map( _.asType.toType.toString )
        val members  = ctype.members.filter(_.isTerm).map(_.asMethod).filter(_.isGetter)
        val mappedMembers = members.map(_.name.toString).zip( members.map(_.typeSignature.resultType.toString) ) // a -> A
        ctype match {
          case p:TypeRef => // embedded collections (e.g. members of a case class)
            var allResolved = true
            val resolvedParams = p.args.map( pa => {
              val inspected = staticScan(pa, relativeToTrait, paramSym2Type).asInstanceOf[SjType]
              if( inspected.isInstanceOf[SjTypeSymbol] ) allResolved = false
              inspected
            })
            val rere = s.asClass.typeParams.map(_.name.toString).zip(resolvedParams).toMap  // A -> <some resolved type>
            // Now associate the field 'a' with the resolved type using the type symbol to hook them up
            val resolvedFields = LinkedHashMap.empty[String,SjType] ++= mappedMembers.collect{  
              case (item,itemType) if(rere.contains(itemType)) => (item,rere(itemType)) 
            }
            if( allResolved )
              SjTrait(s.fullName,resolvedFields)
           else {
             println("BOOM 1!")
             null.asInstanceOf[SjType]
           }
        }
      case s if(s.asClass.isCaseClass)             =>
        val symbol        = s.asClass
      // OK, type params came in the right order (positionally), but will have parent's symbols (except for top-level cc).
      // We must re-map these positionally to this classes reflected symbols.
      val myParamSymbols = LinkedHashMap.empty[String,SjType] ++= symbol.typeParams.map(_.name.toString).zip(paramSym2Type.values).toMap
      val tag = s.fullName+myParamSymbols.values.map(_.tag).mkString("[",",","]")
        readyToEat.getOrElse(tag, {
          val ctor          = symbol.primaryConstructor
          val fields        = ctor.typeSignature.paramLists.head
          val sjfields      = fields.map( f => {
            val preMappedField = relativeToTrait.flatMap( _.resolvedParamFields.get(f.name.toString) )
            val ftype = preMappedField.getOrElse( 
                myParamSymbols.getOrElse(f.typeSignature.toString,
                  staticScan( f.typeSignature, relativeToTrait, myParamSymbols ).asInstanceOf[SjType]
                ))
            SjField(f.name.toString, ftype)
          })
          val built = SjCaseClass( s.fullName, sjfields )
          readyToEat.put(tag,built)
          built
        }) 

        /*
         * 3 cases:
         * 1) No parameters
         * 2) Simple parameter (param type is the value type)
         * 3) Complex parameter (param is a likewise a param of a case class, which is the value type)
         */
      case s if(s.asClass.isDerivedValueClass)     => // value class support
        val symbol        = s.asClass
        ctype match {
          case p:TypeRef => // embedded collections (e.g. members of a case class)
            val vField = symbol.primaryConstructor.typeSignature.paramLists.head.head
            val vFieldResult = vField.asTerm.info.resultType
            val valSjType = p.args match {
              case pa if( pa.length == 0 ) => // Case 1
                staticScan(symbol.primaryConstructor.asMethod.paramLists.head.head.info, relativeToTrait, paramSym2Type).asInstanceOf[SjType]
              case pa => // Cases 2 & 3
                val vTerm = symbol.primaryConstructor.typeSignature.paramLists.head.head.asTerm.info.resultType // Don't ask...its magic.
                // problem--need to map multiple params to a single SjType???
                paramSym2Type.getOrElse( vTerm.toString, {  // Case 2
                  // Case 3
                  val pMap = LinkedHashMap.empty[String,SjType]
                  pMap ++= vTerm.typeArgs.zip(vTerm.typeSymbol.asClass.typeParams.map(_.name.toString)).map({ case(mySym,memberSym) => 
                    (memberSym,paramSym2Type(mySym.toString))
                    })
                 staticScan(symbol.primaryConstructor.asMethod.paramLists.head.head.info, relativeToTrait, pMap).asInstanceOf[SjType]
                })
            }
        SjValueClass(symbol.fullName, valSjType, vField.name.toString)
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
 
  // Goal: Build a map of symbols->SjType
  def preRezolve_nonTrait( sjT:SjType, ctype:Type ) : SjType = sjT match {
    case sj:SjCaseClassProxy =>
     ctype match {
        case p:TypeRef => 
          val mapped = LinkedHashMap.empty[String,SjType]
          sj.params.zip(p.args).map( { case(psym,parg) => mapped.put(psym,staticScan(parg).asInstanceOf[SjType] ) })
//          sj.params.zip(p.args).map( { case(psym,parg) => mapped.put(psym,staticScan(parg,None,sj.params).asInstanceOf[SjType] ) })
          rezolve(sj, mapped)
        case nope      =>
          println("BOOM! CC Proxy resolution failed")
          sj
      }
    case sj => sj
  }
  
  def preRezolve_trait( sjT:SjType, mappedSymbols:LinkedHashMap[String,SjType] ) : SjType = sjT match {
    case sj:SjCaseClassProxy =>
      rezolve( sj, sj.mappedTraitParams )
    case sj => sj
  }
  
    def rezolve( sjT:SjType, mappedSymbols:LinkedHashMap[String,SjType] ) : SjType = {
      sjT match {
        case sj:SjCaseClassProxy  =>
          val tag = sj.name+mappedSymbols.values.map(_.tag).mkString("[",",","]")
          resolved.getOrElse(tag, {
            val r = SjCaseClass( sj.name, sj.fields.map( f => f.ftype match {
              case ft:SjTypeSymbol => f.copy( ftype = mappedSymbols(ft.name) )
              case ft:ParamProxy   => 
                f.copy( ftype = rezolve(ft, mappedSymbols) )
              case ft =>
                f // no mapping
            }))
            resolved.put(tag, r)
            r
          })
       case sj:SjValueClassProxy  => 
          val appliedParam = mappedSymbols(sj.vcType.name)
          val resolvedTag = s"sj.name[${appliedParam.name}]"
          resolved.getOrElse(resolvedTag, SjValueClass(sj.name, appliedParam, sj.vFieldName))
       case sj:SjCollection      => sj.copy( collectionType = sj.collectionType.map( ct => rezolve(ct, mappedSymbols) ) )
       case sj => sj
      }
    }
}
 