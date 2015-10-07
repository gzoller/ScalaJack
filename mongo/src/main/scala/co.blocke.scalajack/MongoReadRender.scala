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
			case sj:PrimType if( sj.name == "scala.Any" ) =>
				inferType( src ).asInstanceOf[T]
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
				} else if( sj.name.startsWith("scala.Tuple") ) {
					val arity = """\d+""".r.findFirstIn(sj.name).get.toInt
					val resolved = (src match {
						case mdbl:MongoDBList => mdbl.underlying
						case bdbl:BasicDBList => bdbl
						case _ => throw new MongoParseException(s"Expected MongoDBList or BasicDBList and found ${src.getClass.getName}")
					}).iterator
					val res = MList.empty[Any]
					var c = 0
					while( resolved.hasNext ) {
						res += parse(sj.colTypes(c), resolved.next)
						c += 1
					}
					arity match {
						case 2  => PrimitiveTypes.scalaCollections(sj.name)( (res(0),res(1)) ).asInstanceOf[T]
						case 3  => PrimitiveTypes.scalaCollections(sj.name)( (res(0),res(1),res(2)) ).asInstanceOf[T]
						case 4  => PrimitiveTypes.scalaCollections(sj.name)( (res(0),res(1),res(2),res(3)) ).asInstanceOf[T]
						case 5  => PrimitiveTypes.scalaCollections(sj.name)( (res(0),res(1),res(2),res(3),res(4)) ).asInstanceOf[T]
						case 6  => PrimitiveTypes.scalaCollections(sj.name)( (res(0),res(1),res(2),res(3),res(4),res(5)) ).asInstanceOf[T]
						case 7  => PrimitiveTypes.scalaCollections(sj.name)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6)) ).asInstanceOf[T]
						case 8  => PrimitiveTypes.scalaCollections(sj.name)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7)) ).asInstanceOf[T]
						case 9  => PrimitiveTypes.scalaCollections(sj.name)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8)) ).asInstanceOf[T]
						case 10 => PrimitiveTypes.scalaCollections(sj.name)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9)) ).asInstanceOf[T]
						case 11 => PrimitiveTypes.scalaCollections(sj.name)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10)) ).asInstanceOf[T]
						case 12 => PrimitiveTypes.scalaCollections(sj.name)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10),res(11)) ).asInstanceOf[T]
						case 13 => PrimitiveTypes.scalaCollections(sj.name)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10),res(11),res(12)) ).asInstanceOf[T]
						case 14 => PrimitiveTypes.scalaCollections(sj.name)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10),res(11),res(12),res(13)) ).asInstanceOf[T]
						case 15 => PrimitiveTypes.scalaCollections(sj.name)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10),res(11),res(12),res(13),res(14)) ).asInstanceOf[T]
						case 16 => PrimitiveTypes.scalaCollections(sj.name)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10),res(11),res(12),res(13),res(14),res(15)) ).asInstanceOf[T]
						case 17 => PrimitiveTypes.scalaCollections(sj.name)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10),res(11),res(12),res(13),res(14),res(15),res(16)) ).asInstanceOf[T]
						case 18 => PrimitiveTypes.scalaCollections(sj.name)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10),res(11),res(12),res(13),res(14),res(15),res(16),res(17)) ).asInstanceOf[T]
						case 19 => PrimitiveTypes.scalaCollections(sj.name)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10),res(11),res(12),res(13),res(14),res(15),res(16),res(17),res(18)) ).asInstanceOf[T]
						case 20 => PrimitiveTypes.scalaCollections(sj.name)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10),res(11),res(12),res(13),res(14),res(15),res(16),res(17),res(18),res(19)) ).asInstanceOf[T]
						case 21 => PrimitiveTypes.scalaCollections(sj.name)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10),res(11),res(12),res(13),res(14),res(15),res(16),res(17),res(18),res(19),res(20)) ).asInstanceOf[T]
						case 22 => PrimitiveTypes.scalaCollections(sj.name)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10),res(11),res(12),res(13),res(14),res(15),res(16),res(17),res(18),res(19),res(20),res(21)) ).asInstanceOf[T]
					}
//zzz
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
						case "scala.Any" if(instance.isInstanceOf[List[_]] || instance.isInstanceOf[Map[_,_]]) =>
							Some( explodeAny(instance) )
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
						case t if( t startsWith("scala.Tuple") ) => 
							val arity = """\d+""".r.findFirstIn(t).get.toInt
							val builder = new MongoDBListBuilder()
							arity match {
								case 2  =>
									val cv = instance.asInstanceOf[Tuple2[_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder += item )
								case 3  =>
									val cv = instance.asInstanceOf[Tuple3[_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder += item )
								case 4  =>
									val cv = instance.asInstanceOf[Tuple4[_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder += item )
								case 5  =>
									val cv = instance.asInstanceOf[Tuple5[_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder += item )
								case 6  =>
									val cv = instance.asInstanceOf[Tuple6[_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder += item )
								case 7  =>
									val cv = instance.asInstanceOf[Tuple7[_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder += item )
								case 8  =>
									val cv = instance.asInstanceOf[Tuple8[_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder += item )
								case 9  =>
									val cv = instance.asInstanceOf[Tuple9[_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder += item )
								case 10  =>
									val cv = instance.asInstanceOf[Tuple10[_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder += item )
								case 11  =>
									val cv = instance.asInstanceOf[Tuple11[_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder += item )
								case 12  =>
									val cv = instance.asInstanceOf[Tuple12[_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(11), cv._12, tt.tpe.typeArgs).map( item => builder += item )
								case 13  =>
									val cv = instance.asInstanceOf[Tuple13[_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(11), cv._12, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(12), cv._13, tt.tpe.typeArgs).map( item => builder += item )
								case 14  =>
									val cv = instance.asInstanceOf[Tuple14[_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(11), cv._12, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(12), cv._13, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(13), cv._14, tt.tpe.typeArgs).map( item => builder += item )
								case 15  =>
									val cv = instance.asInstanceOf[Tuple15[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(11), cv._12, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(12), cv._13, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(13), cv._14, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(14), cv._15, tt.tpe.typeArgs).map( item => builder += item )
								case 16  =>
									val cv = instance.asInstanceOf[Tuple16[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(11), cv._12, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(12), cv._13, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(13), cv._14, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(14), cv._15, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(15), cv._16, tt.tpe.typeArgs).map( item => builder += item )
								case 17  =>
									val cv = instance.asInstanceOf[Tuple17[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(11), cv._12, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(12), cv._13, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(13), cv._14, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(14), cv._15, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(15), cv._16, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(16), cv._17, tt.tpe.typeArgs).map( item => builder += item )
								case 18  =>
									val cv = instance.asInstanceOf[Tuple18[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(11), cv._12, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(12), cv._13, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(13), cv._14, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(14), cv._15, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(15), cv._16, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(16), cv._17, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(17), cv._18, tt.tpe.typeArgs).map( item => builder += item )
								case 19  =>
									val cv = instance.asInstanceOf[Tuple19[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(11), cv._12, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(12), cv._13, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(13), cv._14, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(14), cv._15, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(15), cv._16, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(16), cv._17, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(17), cv._18, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(18), cv._19, tt.tpe.typeArgs).map( item => builder += item )
								case 20  =>
									val cv = instance.asInstanceOf[Tuple20[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(11), cv._12, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(12), cv._13, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(13), cv._14, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(14), cv._15, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(15), cv._16, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(16), cv._17, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(17), cv._18, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(18), cv._19, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(19), cv._20, tt.tpe.typeArgs).map( item => builder += item )
								case 21  =>
									val cv = instance.asInstanceOf[Tuple21[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(11), cv._12, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(12), cv._13, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(13), cv._14, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(14), cv._15, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(15), cv._16, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(16), cv._17, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(17), cv._18, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(18), cv._19, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(19), cv._20, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(20), cv._21, tt.tpe.typeArgs).map( item => builder += item )
								case 22  =>
									val cv = instance.asInstanceOf[Tuple22[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(11), cv._12, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(12), cv._13, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(13), cv._14, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(14), cv._15, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(15), cv._16, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(16), cv._17, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(17), cv._18, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(18), cv._19, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(19), cv._20, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(20), cv._21, tt.tpe.typeArgs).map( item => builder += item )
									_render(g.colTypes(21), cv._22, tt.tpe.typeArgs).map( item => builder += item )
							}
							// val collVal = instance.asInstanceOf[Iterable[_]]
							// collVal.map( item => 
							// 	_render(g.colTypes.head, item, tt.tpe.typeArgs).map( item => builder += item )
							// )
							Some(builder.result)
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
			
		private def explodeAny( inst:Any ) : Any = inst match {
			case s:String   => s
			case l:List[_]  => 
				val builder = new MongoDBListBuilder()
				l.map( item => builder += explodeAny(item) )
				builder.result
			case m:Map[_,_] => 
				val mdbo = new MongoDBObject()
				m.map({ case (k,v) => 
					mdbo.put(k.asInstanceOf[String],explodeAny(v))
				})
				mdbo
			case x          => x 
		}

		private def inferType( src:AnyRef ) : Any = src match {
			case l:MongoDBList => 
				val lbuf = new scala.collection.mutable.ListBuffer[Any]()
				val iter = l.iterator
				while( iter.hasNext )
					lbuf += inferType(iter.next)
				lbuf.toList
			case dbo:DBObject => 
				val mbuf = new scala.collection.mutable.HashMap[String,Any]()
				val iter = dbo.keySet.iterator
				while( iter.hasNext ) {
					val key = iter.next
					mbuf += (key -> inferType(dbo.get(key)))
				}
				mbuf.toMap
			case x => x
		}
	}
}