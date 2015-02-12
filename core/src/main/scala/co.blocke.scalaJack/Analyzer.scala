package co.blocke.scalajack

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._
import PrimitiveTypes._
import scala.collection.concurrent.TrieMap

object Analyzer {

	private val readyToEat = TrieMap.empty[String,SjType]  // a cache, sir, a cache

	def inspect( c:Any ) = 
		readyToEat.get(c.getClass.getName).getOrElse({
			val t = staticScan(currentMirror.classSymbol(c.getClass).typeSignature).asInstanceOf[SjType]
			readyToEat.put(c.getClass.getName,t)
			t      
		})

	private def staticScan( 
		ctype:Type, 
		typePlaceholders:List[String] = List.empty[String] 
	) : SjItem = 
		ctype.typeSymbol match {
			case s if(s.isPlaceholder(typePlaceholders)) => SjTypeSymbol( s.name.toString )
			case s if(s.isPrimitive)                     => SjPrimitive( s.fullName )
			case s if(s.isCollection)                    => 
				val collArgTypes = ctype.asInstanceOf[TypeRef].args.map( a => staticScan(a,typePlaceholders) )
				SjCollection( s.fullName, collArgTypes.asInstanceOf[List[SjType]] )
			case s if(s.asClass.isTrait)                 => 
				val resolvedTypeArgs = ctype.asInstanceOf[TypeRef].args.map( _.toString )
				SjTrait(s.fullName, resolvedTypeArgs)
			case s if(s.asClass.isCaseClass)             =>
				val symbol        = s.asClass
				val typeParamArgs = symbol.typeParams.map( tp => tp.name.toString)
				val fields        = symbol.primaryConstructor.typeSignature.paramLists.head
				val sjfields      = fields.map( f => SjField(f.name.toString, staticScan( f.typeSignature, typeParamArgs ).asInstanceOf[SjType]) )
				SjCaseClass( s.fullName, typeParamArgs, sjfields )
			case s                                       => throw new ReflectException(s"Static reflection failed for symbol ${s.fullName}.")
		}
}