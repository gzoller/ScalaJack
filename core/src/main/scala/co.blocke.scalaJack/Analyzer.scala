package co.blocke.scalajack

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._
import PrimitiveTypes._
import scala.collection.concurrent.TrieMap

object Analyzer {

	private val readyToEat = TrieMap.empty[String,SjType]  // a cache, sir, a cache

	def inspect[T]( c:T )(implicit tt:TypeTag[T]) = 
		readyToEat.get(c.getClass.getName).getOrElse({
			val t = staticScan(currentMirror.classSymbol(c.getClass).typeSignature).asInstanceOf[SjType]
			readyToEat.put(c.getClass.getName,t)
			t      
		})

	def nakedInspect[T](typeArgs:List[Type]) = typeArgs.map( staticScan(_).asInstanceOf[SjType] )

	private def staticScan( 
		ctype:Type, 
		typePlaceholders:List[String] = List.empty[String] 
	) : SjItem = 
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
				val typeParamArgs = symbol.typeParams.map( tp => tp.name.toString)
				val fields        = symbol.primaryConstructor.typeSignature.paramLists.head
				val sjfields      = fields.map( f => SjField(f.name.toString, staticScan( f.typeSignature, typeParamArgs ).asInstanceOf[SjType]) )
				SjCaseClass( s.fullName, typeParamArgs, sjfields )
			case s if(s.asClass.isDerivedValueClass)     => // value class support
				val symbol        = s.asClass
				val typeParamArgs = symbol.typeParams.map( tp => tp.name.toString)
				// Get type of the value member (only constructor parameter)
				val vcType = Analyzer.staticScan(symbol.primaryConstructor.asMethod.paramLists.head.head.info, typeParamArgs).asInstanceOf[SjType]
				// Get field name of value member
				val vField = symbol.primaryConstructor.typeSignature.paramLists.head.head.name.toString
				SjValueClass(symbol.fullName,vcType,vField)
			case s                                       => 
			println("BOOM: "+s.asClass.fullName)
			throw new ReflectException(s"Static reflection failed for symbol ${s.fullName}.")
		}
}