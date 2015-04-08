package co.blocke.scalajack
package mongo

import scala.reflect.runtime.universe._
import scala.collection.mutable.{Map => MMap,ListBuffer => MList}
import scala.collection.JavaConversions._
import json._
import JsonTokens._
import org.joda.time.DateTime
import com.mongodb.casbah.Imports._
import com.mongodb.casbah.commons.MongoDBListBuilder

/*
	Mongo serializes to MongoDBObject, so there are some built-in constraints.
	1) Top-level object must be a case class.
	2) Keys to any Map-like data must be of type String
*/

class MongoParseException(msg:String) extends Exception(msg)

trait MongoReadRenderFrame extends ReadRenderFrame[MongoDBObject] { 
	def renderer = new MongoReadRender()

	class MongoReadRender() extends ReadRender {

		def read[T](src:MongoDBObject)(implicit tt:TypeTag[T], vc:VisitorContext=VisitorContext()) : T = 
			parse(Analyzer.inspectByName(tt.tpe.typeSymbol.fullName), src, true)

		def render[T](instance:T)(implicit tt:TypeTag[T], vc:VisitorContext) : MongoDBObject = 
			_render(Analyzer.inspect(instance), instance).get.asInstanceOf[MongoDBObject]

		private def _makeClass[U]( ccTypeFn : ()=>CCType, t:AType, src:Any )(implicit ty:TypeTag[U]) = {
			val sjT = ccTypeFn()
			if( !src.isInstanceOf[MongoDBObject] ) throw new MongoParseException(s"Wrong type for $src.  Expected a MongoDBObject.")
			val mo = src.asInstanceOf[MongoDBObject]
			val build = 
			scala.collection.mutable.Map.empty[String,Any]
			val fieldData = mo.collect{ 
				case(k,v) if(k == "_id") => 
					v match {
						case vObj:DBObject =>
							vObj.foreach{ case(fname,koV) => 
								sjT.members.get(fname).map( ft => 
									build.put(fname,parse(ft,koV))
									)
							}
						case _ =>
							sjT.members.find( { case(fname,ftype) => ftype match {
								case p:AType if(p.isDbKey) => true 
								case x => false
								}}).map( f => build.put(f._1, parse(f._2,v)) )
					}
				case(k,v) if(sjT.members.contains(k)) => 
					build.put(k,parse(sjT.members(k),v))
				case _ => null.asInstanceOf[U]
			}
			Util.poof( sjT, build.toMap ).asInstanceOf[U]
		}

		private def parse[T](t:AType, src:AnyRef, topLevel:Boolean = false)(implicit tt:TypeTag[T], vc:VisitorContext=VisitorContext()) : T = t match {
			case sj:CCType =>
				_makeClass( ()=>{sj}, t, src )
			case sj:TraitType =>
				_makeClass( ()=>{
					// Look-ahead and find type hint--figure out what kind of object his is and inspect it.
					val mo = src.asInstanceOf[MongoDBObject]
					val hintClass = mo.get(vc.hintMap.getOrElse(sj.name,vc.hintMap("default")))
					if( hintClass.isEmpty ) throw new MongoParseException(s"No type hint given for trait ${sj.name}")
					val sjObjType = Analyzer.inspectByName(hintClass.get.toString,Some(sj))
					if( !sjObjType.isInstanceOf[CCType] ) throw new MongoParseException(s"Type hint $hintClass does not specify a case class")
					sjObjType.asInstanceOf[CCType]
					}, t, src)
			case sj:PrimType =>
				PrimitiveTypes.primitiveTypes(sj.name)( Unicode.unescape_perl_string(src.toString) ).asInstanceOf[T]
			case sj:EnumType =>
				try {
					sj.enum.withName( src.toString ).asInstanceOf[T]
				} catch {
					case w:Throwable => throw new MongoParseException(s"Value ${src} is not a valid for enum ${sj.enum.toString}")
				}
			case sj:CollType =>
				if( sj.isOptional ) {
					Some(parse(sj.colTypes(0), src)).asInstanceOf[T]
				} else if(sj.name.endsWith("Map")) {
					src match {
						case dbo:DBObject => 
							PrimitiveTypes.scalaCollections(sj.name)( dbo.keySet.map( key => (key,parse(sj.colTypes(1),dbo(key))) ).toSeq ).asInstanceOf[T]
						case dbo:MongoDBObject => 
							PrimitiveTypes.scalaCollections(sj.name)( dbo.keySet.map( key => (key,parse(sj.colTypes(1),dbo(key))) ).toSeq ).asInstanceOf[T]
						case _ => 
							throw new MongoParseException(s"Expected MongoDBObject or DBObject and found ${src.getClass.getName}")
					}
				} else {
					// Dumb down to BasicDBList to avoid double-creation/wrapping of value from database
					val resolved = (src match {
						case mdbl:MongoDBList => mdbl.underlying
						case bdbl:BasicDBList => bdbl
						case _ => throw new MongoParseException(s"Expected MongoDBList or BasicDBList and found ${src.getClass.getName}")
					}).iterator
					val resList = MList.empty[Any]
					while( resolved.hasNext ) {
						resList += parse(sj.colTypes(0), resolved.next)
					}
					PrimitiveTypes.scalaCollections(sj.name)(resList.toList).asInstanceOf[T]
				}
			case sj:ValueClassType =>
				{if( sj.isTypeParam || topLevel ) 
					makeValueClass(sj, parseValueClassPrimitive(src, sj, vc, topLevel))
				else 
					parseValueClassPrimitive(src, sj, vc, topLevel)
				}.asInstanceOf[T]
			case g:CustomType =>
				g.readers("mongo")(src).asInstanceOf[T]
		}
		private def parseValueClassPrimitive( src:AnyRef, vc:ValueClassType, visitor:VisitorContext, topLevel:Boolean ) = 
			visitor.valClassMap.get(vc.name).fold(src)( handler => 
				handler.read( src.toString ).asInstanceOf[AnyRef]
			)
		private def makeValueClass( vc:ValueClassType, primitive:AnyRef ) = 
			Class.forName(vc.name).getConstructors()(0).newInstance(primitive)

		private def _render[T](
			graph    : AType, 
			instance : T, 
			typeArgs : List[Type]=List.empty[Type]
		)(implicit tt:TypeTag[T], vc:VisitorContext):Option[Any] = {
			graph match {
				case g:CCType  => 
					val mdbo = new MongoDBObject()
					g.superTrait.map( superTrait => mdbo.put(vc.hintMap.getOrElse(superTrait.name,vc.hintMap("default")), g.name) )
					val (dbIds, regFields) = g.members.partition{ // separate out the db key fields (may be 0, 1, or >1)
						case(fname,ftype) => ftype.isDbKey
					}
					val cz = instance.getClass()
					val getField = (fname:String) => {
						val targetField = cz.getDeclaredField(fname)
						targetField.setAccessible(true)
						targetField.get(instance)
					}
					dbIds.size match {
						case 0 => // nothing
						case 1 => 
							val (fname,ftype) = dbIds.head
							_render(ftype, getField(fname), tt.tpe.typeArgs).map( fval => mdbo.put("_id",fval) )
						case _ => 
							val keyObj = new MongoDBObject()
							dbIds.map{ case(fname,ftype) => _render(ftype, getField(fname), tt.tpe.typeArgs).map( fval => keyObj.put(fname,fval) ) }
							mdbo.put("_id",keyObj)
					}
					regFields.map{ case(fname,ftype) =>
						_render(ftype, getField(fname), tt.tpe.typeArgs).map( fval => mdbo.put(fname,fval) )
					}
					Some(mdbo)
				case g:PrimType  => 
					g.name match {
						case "String" | "java.lang.String" | "scala.Char" | "scala.Enumeration.Value" | "java.util.UUID" if(instance != null) => 
							Some(clean(instance.toString))
						case "org.joda.time.DateTime" =>
							Some(instance.asInstanceOf[DateTime].getMillis.asInstanceOf[Long])
						case _ => 
							Some(instance)
					}
				case g:CollType => 
					g.name match {
						case "scala.Option" => 
							val optVal = instance.asInstanceOf[Option[_]]
							optVal.flatMap( ov => _render(g.colTypes.head, ov, tt.tpe.typeArgs) )
						case n if(n.endsWith("Map")) => 
							val mdbo = new MongoDBObject()
							val mapVal = instance.asInstanceOf[Map[_,_]]
							mapVal.map({ case (k,v) => 
								_render(g.colTypes(1), v, tt.tpe.typeArgs).map( item => mdbo.put(k.asInstanceOf[String],item))
							})
							Some(mdbo)
						case _ => 
							val collVal = instance.asInstanceOf[Iterable[_]]
							val builder = new MongoDBListBuilder()
							collVal.map( item => 
								_render(g.colTypes.head, item, tt.tpe.typeArgs).map( item => builder += item )
							)
							Some(builder.result)
					}
				case g:TraitType => 
					val cc = Analyzer.inspect(instance,Some(g)).asInstanceOf[CCType]
					_render(cc.copy(superTrait = Some(g)),instance,tt.tpe.typeArgs)
				case g:ValueClassType =>
					val renderVal = {
						if( g.name != instance.getClass.getName ) // raw/unwrapped value
							instance
						else {
							val targetField = instance.getClass.getDeclaredField(g.vFieldName)
							targetField.setAccessible(true)
							targetField.get(instance)
						}
					}
					vc.valClassMap.get(g.name).map( vcHand =>
							Some(vcHand.render(renderVal))
						).orElse(
							_render(g.vcType,renderVal,tt.tpe.typeArgs)
						)
				case g:EnumType =>
					Some(s"${instance.toString}")
				case g:CustomType =>
					Some(g.renderers("mongo")(instance))
			}
		}
	}
}