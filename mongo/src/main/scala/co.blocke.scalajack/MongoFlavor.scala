package co.blocke.scalajack
package mongo

import co.blocke.scalajack.json.JsonFlavor.MemberName
import co.blocke.scalajack.json.typeadapter.{FallbackTypeAdapter, PolymorphicTypeAdapter, PolymorphicTypeAdapterFactory}

import scala.reflect.runtime.universe._
import scala.collection.mutable.{ListBuffer => MList, Map => MMap}
import scala.collection.JavaConversions._
import json._

import scala.collection.mutable
import scala.reflect.runtime._
//import JsonTokens._
import org.joda.time.DateTime

import org.mongodb.scala._
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson._

/*
	Mongo serializes to Document, so there are some built-in constraints.
	1) Top-level object must be a case class.
	2) Keys to any Map-like data must be of type String
*/

case class MongoKind() extends KindMarker  // For custom value class read/render (ValueClassCustom)

class MongoParseException(msg:String) extends Exception(msg)

case class MongoFlavor() extends FlavorKind[Document] {
	// Analyzer.addType(OBJECT_ID,ObjectIdType(OBJECT_ID))
	def makeScalaJack : ScalaJack[Document] = new MongoScalaJack()  
	class MongoScalaJack() extends ScalaJack[Document] with MongoJackFlavor
}

trait MongoJackFlavor extends JackFlavor[Document] {

	val bsonParser = new BsonParser

	val context = Context.StandardContext

	val contextCache = new mutable.WeakHashMap[VisitorContext, Context]

	def context(vc: VisitorContext): Context =
		contextCache.getOrElseUpdate(vc, {
			import BijectiveFunction.Implicits._
			import BijectiveFunctions._

			val polymorphicFullNames: Set[String] = Set() ++
				vc.hintValueRead.keySet ++
				vc.hintValueRender.keySet ++
				vc.hintMap.keySet.filter(_ != "default")

			val defaultHintFieldName: String = vc.hintMap.getOrElse("default", "_hint")

			val customHandlerTypeAdapterFactories = vc.customAdapters

			val polymorphicTypeAdapterFactories = polymorphicFullNames map { polymorphicFullName ⇒
				val polymorphicType = fullNameToType(polymorphicFullName)

				val hintFieldName = vc.hintMap.getOrElse(polymorphicFullName, defaultHintFieldName)

				val hintToType: BijectiveFunction[String, Type] = {
					val optionalCustomApply: Option[String ⇒ Type] = vc.hintValueRead.get(polymorphicFullName).map(f ⇒ f andThen fullNameToType)
					val optionalCustomUnapply: Option[Type ⇒ String] = vc.hintValueRender.get(polymorphicFullName).map(f ⇒ typeToFullName andThen f)

					if (optionalCustomApply.isDefined || optionalCustomUnapply.isDefined) {
						val customApply: (String ⇒ Type) = optionalCustomApply.getOrElse(_ ⇒ throw new Exception(s"""Cannot serialize ${typeToFullName(polymorphicType)} because the visitor context's hintValueReader lacks an entry whose key is "$polymorphicFullName""""))
						val customUnapply: (Type ⇒ String) = optionalCustomUnapply.getOrElse(_ ⇒ throw new Exception(s"""Cannot deserialize ${typeToFullName(polymorphicType)} because the visitor context's hintValueRender lacks an entry whose key is "$polymorphicFullName""""))

						customApply ⇄ customUnapply
					} else {
						fullNameToType
					}
				}

				val polymorphicTypeAdapterFactory = new TypeAdapterFactory {

					override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] = {
						// FIXME              if (tpe =:= polymorphicType) {
						//              if (tpe =:= polymorphicType) {
						if (tpe.typeSymbol == polymorphicType.typeSymbol) {
							// print("NAME: " + tpe.typeSymbol.fullName + "  -->  " + polymorphicFullName)
							// if (tpe.typeSymbol.fullName == polymorphicFullName) {
							val stringTypeAdapter = context.typeAdapterOf[String]

							Some(PolymorphicTypeAdapter(hintFieldName, stringTypeAdapter andThen hintToType.memoized, context.typeAdapterOf[MemberName], context, tpe))
						} else {
							None
						}
					}

				}

				polymorphicTypeAdapterFactory
			}

			val intermediateContext = context.copy(
				factories = customHandlerTypeAdapterFactories.toList ::: polymorphicTypeAdapterFactories.toList ::: context.factories ::: List(PolymorphicTypeAdapterFactory(defaultHintFieldName))
			)

			val fallbackFactories = vc.parseOrElse.map({
				case (attemptedFullName, fallbackFullName) ⇒
					val attemptedType = currentMirror.staticClass(attemptedFullName).asType.toType
					val attemptedTypeAdapter = intermediateContext.typeAdapter(attemptedType)

					val fallbackType = currentMirror.staticClass(fallbackFullName).asType.toType
					val fallbackTypeAdapter = intermediateContext.typeAdapter(fallbackType)

					new TypeAdapterFactory {
						override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
							if (tpe =:= attemptedType) {
								Some(FallbackTypeAdapter[Any](attemptedTypeAdapter.asInstanceOf[TypeAdapter[Any]], fallbackTypeAdapter.asInstanceOf[TypeAdapter[Any]]))
							} else {
								None
							}
					}
			})

			intermediateContext.copy(
				factories = fallbackFactories.toList ::: intermediateContext.factories
			)
		})

	def rr = new MongoReadRenderer()
	class MongoReadRenderer() extends ReadRenderer {
		def read[T](src: Document)(implicit tt: TypeTag[T], vc: VisitorContext = VisitorContext()): T = {
			val c = context(vc)
			val bsonDocument: BsonDocument = src.toBsonDocument
			val bsonReader = bsonParser.parse(bsonDocument)
			val typeAdapter = c.typeAdapter(tt.tpe).asInstanceOf[TypeAdapter[Any]]
			typeAdapter.read(bsonReader).asInstanceOf[T]
		}

		//			parse(Analyzer.inspectByName(tt.tpe.typeSymbol.fullName), src.toBsonDocument, true)

		def render[T](instance: T)(implicit tt: TypeTag[T], vc: VisitorContext = VisitorContext()): Document = {
			val c = context(vc)
			val bsonWriter = new BsonWriter
			val typeAdapter = c.typeAdapter(tt.tpe).asInstanceOf[TypeAdapter[Any]]
			typeAdapter.write(instance, bsonWriter)
			Document(bsonWriter.value.asDocument)
		}
	}
//			_render(Analyzer.inspect(instance), instance).map( _ match {
//				case bsdoc:BsonDocument => Document(bsdoc)
//				case _ => throw new MongoParseException("Unexpected result where Document was expected.")
//			}).getOrElse(throw new MongoParseException("Unable to render JSON to MongoDB"))

	//-------------------------------------------------------------- Read ------
/*
		private def _makeClass[U]( ccTypeFn : ()=>CCType, t:AType, src:BsonValue )(implicit ty:TypeTag[U], vc:VisitorContext) = src match {
			case doc:BsonDocument =>
				val sjT = ccTypeFn()
				val build = scala.collection.mutable.Map.empty[String,Any]
				doc.map{ 
					case(k,v) if(k == "_id") => 
						v match {
							case vObj:BsonDocument =>   // Compound key
								vObj.foreach{ case(fname,koV) => 
									sjT.members.get(fname).map( ft => 
										build.put(fname,parse(ft._1,koV))
										)
								}
							case _ =>
								sjT.members.find( { case(fname,ftype) => ftype._1 match {
									case p:AType if(p.isDbKey) => true 
									case x => false
									}}).map( f => build.put(f._1, parse(f._2._1,v)) )
						}
					case(k,v) if(sjT.members.contains(k)) => 
						build.put(k,parse(sjT.members(k)._1,v))
					case _ => null.asInstanceOf[U]
				}
				// Handle any missing fields
				sjT.members.keySet.diff(doc.keySet).map{ missing =>
					sjT.members(missing) match {
						case (t:CollType,dv) if(t.isOptional) => build.put(missing,None)
						case (t,dv) if(dv.isDefined) => build.put(missing,dv.get)
						case (t,dv) => 
							if(!t.isDbKey || (t.isDbKey && !build.contains(missing))) 
								throw new MongoParseException(s"Missing required field $missing in BsonDocument")   // really missing, not optional and no devault value given
					}
				}
				sjT.materialize(build.toMap).asInstanceOf[U]
			case _ => throw new MongoParseException("Attempt to make a class from something other than a BsonDocument: "+src)
		}

		private def parse[T](t:AType, src:BsonValue, topLevel:Boolean = false)(implicit tt:TypeTag[T], vc:VisitorContext=VisitorContext()) : T = t match {
			case sj:CCType =>
				_makeClass( ()=>{sj}, t, src )
			case sj:TraitType =>
				_makeClass( ()=>{
					// Look-ahead and find type hint--figure out what kind of object his is and inspect it.
					val hintClass = Option(src.asInstanceOf[BsonDocument].get( vc.hintMap.getOrElse(sj.name,vc.hintMap("default")) )).map(_.asString.getValue())
						// See if we need to look up actual objClass (e.g. abbreviation) or if its ready-to-eat
						.map( candidate => vc.hintValueRead.get(sj.name).map(_(candidate)).getOrElse(candidate) )
					if( hintClass.isEmpty ) throw new MongoParseException(s"No type hint given for trait ${sj.name}")
					val sjObjType = Analyzer.inspectByName(hintClass.get.toString,Some(sj))
					if( !sjObjType.isInstanceOf[CCType] ) throw new MongoParseException(s"Type hint $hintClass does not specify a case class")
					sjObjType.asInstanceOf[CCType]
				}, t, src)
			case sj:PrimType if( sj.name == "scala.Any" ) =>
				inferType( src ).asInstanceOf[T]
			case sj:PrimType =>
				PrimitiveTypes.primTypes(sj.primCode)( Unicode.unescape_perl_string(stringValue(src)) ).asInstanceOf[T]
			case sj:EnumType =>
				try {
					sj.enum.withName( src.asString.getValue() ).asInstanceOf[T]
				} catch {
					case w:Throwable => throw new MongoParseException(s"Value ${src} is not a valid for enum ${sj.enum.toString}")
				}
			case sj:CollType =>
				if( sj.isOptional ) {
					Some(parse(sj.colTypes(0), src)).asInstanceOf[T]
				} else if(sj.name.endsWith("Map")) {
					src match {
						case doc:BsonDocument => 
							PrimitiveTypes.collTypes(sj.collCode)( doc.map{ case(k,v) => (k,parse(sj.colTypes(1),v)) }.toSeq ).asInstanceOf[T]
						case _ => 
							throw new MongoParseException(s"Expected BsonDocument and found ${src.getClass.getName}")
					}
				} else if( sj.name.startsWith("scala.Tuple") ) {
					val arity = """\d+""".r.findFirstIn(sj.name).get.toInt
					val resolved = (src match {
						case a:BsonArray => a
						case _ => throw new MongoParseException(s"Expected BsonDocument and found ${src.getClass.getName}")
					}).iterator
					val res = MList.empty[Any]
					var c = 0
					while( resolved.hasNext ) {
						res += parse(sj.colTypes(c), resolved.next)
						c += 1
					}
					arity match {
						case 2  => PrimitiveTypes.collTypes(sj.collCode)( (res(0),res(1)) ).asInstanceOf[T]
						case 3  => PrimitiveTypes.collTypes(sj.collCode)( (res(0),res(1),res(2)) ).asInstanceOf[T]
						case 4  => PrimitiveTypes.collTypes(sj.collCode)( (res(0),res(1),res(2),res(3)) ).asInstanceOf[T]
						case 5  => PrimitiveTypes.collTypes(sj.collCode)( (res(0),res(1),res(2),res(3),res(4)) ).asInstanceOf[T]
						case 6  => PrimitiveTypes.collTypes(sj.collCode)( (res(0),res(1),res(2),res(3),res(4),res(5)) ).asInstanceOf[T]
						case 7  => PrimitiveTypes.collTypes(sj.collCode)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6)) ).asInstanceOf[T]
						case 8  => PrimitiveTypes.collTypes(sj.collCode)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7)) ).asInstanceOf[T]
						case 9  => PrimitiveTypes.collTypes(sj.collCode)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8)) ).asInstanceOf[T]
						case 10 => PrimitiveTypes.collTypes(sj.collCode)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9)) ).asInstanceOf[T]
						case 11 => PrimitiveTypes.collTypes(sj.collCode)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10)) ).asInstanceOf[T]
						case 12 => PrimitiveTypes.collTypes(sj.collCode)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10),res(11)) ).asInstanceOf[T]
						case 13 => PrimitiveTypes.collTypes(sj.collCode)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10),res(11),res(12)) ).asInstanceOf[T]
						case 14 => PrimitiveTypes.collTypes(sj.collCode)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10),res(11),res(12),res(13)) ).asInstanceOf[T]
						case 15 => PrimitiveTypes.collTypes(sj.collCode)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10),res(11),res(12),res(13),res(14)) ).asInstanceOf[T]
						case 16 => PrimitiveTypes.collTypes(sj.collCode)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10),res(11),res(12),res(13),res(14),res(15)) ).asInstanceOf[T]
						case 17 => PrimitiveTypes.collTypes(sj.collCode)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10),res(11),res(12),res(13),res(14),res(15),res(16)) ).asInstanceOf[T]
						case 18 => PrimitiveTypes.collTypes(sj.collCode)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10),res(11),res(12),res(13),res(14),res(15),res(16),res(17)) ).asInstanceOf[T]
						case 19 => PrimitiveTypes.collTypes(sj.collCode)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10),res(11),res(12),res(13),res(14),res(15),res(16),res(17),res(18)) ).asInstanceOf[T]
						case 20 => PrimitiveTypes.collTypes(sj.collCode)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10),res(11),res(12),res(13),res(14),res(15),res(16),res(17),res(18),res(19)) ).asInstanceOf[T]
						case 21 => PrimitiveTypes.collTypes(sj.collCode)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10),res(11),res(12),res(13),res(14),res(15),res(16),res(17),res(18),res(19),res(20)) ).asInstanceOf[T]
						case 22 => PrimitiveTypes.collTypes(sj.collCode)( (res(0),res(1),res(2),res(3),res(4),res(5),res(6),res(7),res(8),res(9),res(10),res(11),res(12),res(13),res(14),res(15),res(16),res(17),res(18),res(19),res(20),res(21)) ).asInstanceOf[T]
					}
				} else {
					// Dumb down to BasicDBList to avoid double-creation/wrapping of value from database
					val resolved = (src match {
						case a:BsonArray => a
						case _ => throw new MongoParseException(s"Expected BsonDocument and found ${src.getClass.getName}")
					}).iterator
					val resList = MList.empty[Any]
					while( resolved.hasNext ) {
						resList += parse(sj.colTypes(0), resolved.next)
					}
					PrimitiveTypes.collTypes(sj.collCode)(resList.toList).asInstanceOf[T]
				}
			case sj:ValueClassType =>
				{if( sj.isTypeParam || topLevel ) 
					makeValueClass(sj, parseValueClassPrimitive(src, sj, vc, topLevel))
				else 
					parseValueClassPrimitive(src, sj, vc, topLevel)
				}.asInstanceOf[T]
		}

		private def primValue( src:BsonValue ) = src match {
			case bs:BsonString  => bs.getValue()
			case bs:BsonBoolean => bs.getValue()
			case bs:BsonInt32   => bs.getValue()
			case bs:BsonInt64   => bs.getValue()
			case bs:BsonDouble  => bs.getValue()
			case bs:BsonNull    => null
		}
		private def parseValueClassPrimitive( src:BsonValue, vc:ValueClassType, visitor:VisitorContext, topLevel:Boolean ) = 
			vc.custom.map{ _.read.applyOrElse(
					(MongoKind(), src),
					(k:(KindMarker, _)) => {
						throw new JsonParseException(s"No custom read function defined for kind ${k._1}",0)
						null.asInstanceOf[Any]
					})
				}.orElse( Some(primValue(src).asInstanceOf[AnyRef]) ).get.asInstanceOf[AnyRef]

		private def makeValueClass( vc:ValueClassType, primitive:AnyRef ) = 
			Class.forName(vc.name).getConstructors()(0).newInstance(primitive)

		def stringValue( bv:BsonValue ) = bv match {
			case bs:BsonString   => bs.getValue()
			case bs:BsonBoolean  => bs.getValue().toString
			case bs:BsonInt32    => bs.getValue().toString
			case bs:BsonInt64    => bs.getValue().toString
			case bs:BsonDouble   => bs.getValue().toString
			case bs:BsonDateTime => bs.getValue().toString
			case bs:BsonNull     => "null"
		}

		private def inferType( src:BsonValue ) : Any = src match {
			case l:BsonArray => 
				val lbuf = new scala.collection.mutable.ListBuffer[Any]()
				val iter = l.iterator
				while( iter.hasNext )
					lbuf += inferType(iter.next)
				lbuf.toList
			case dbo:BsonDocument => 
				val mbuf = new scala.collection.mutable.HashMap[String,Any]()
				val iter = dbo.keySet.iterator
				while( iter.hasNext ) {
					val key = iter.next
					mbuf += (key -> inferType(dbo.get(key)))
				}
				mbuf.toMap
			case bs:BsonBoolean => bs.getValue()
			case bs:BsonString  => bs.getValue()
			case bs:BsonInt32   => bs.getValue()
			case bs:BsonInt64   => bs.getValue()
			case bs:BsonDouble  => bs.getValue()
			case bs:BsonNull    => null
			case bs => throw new MongoParseException(s"Can't infer type for value $bs")
		}

	//-------------------------------------------------------------- Render ------

		private def _render[T](
			graph    : AType, 
			instance : T, 
			typeArgs : List[Type]=List.empty[Type]
		)(implicit tt:TypeTag[T], vc:VisitorContext):Option[BsonValue] = {
			graph match {
				case g:CCType  => 
					val mdbo = new BsonDocument()
					g.superTrait.map( superTrait => {
						val hintValue = vc.hintValueRender.get(superTrait.name).map(_(g.name)).getOrElse(g.name)
						mdbo.put(vc.hintMap.getOrElse(superTrait.name,vc.hintMap("default")), new BsonString(hintValue))
						})
					val (dbIds, regFields) = g.members.partition{ // separate out the db key fields (may be 0, 1, or >1)
						case(fname,ftype) => ftype._1.isDbKey
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
							_render(ftype._1, getField(fname), tt.tpe.typeArgs).map( fval => mdbo.put("_id",fval) )
						case _ => 
							val keyObj = new BsonDocument()
							dbIds.map{ case(fname,ftype) => _render(ftype._1, getField(fname), tt.tpe.typeArgs).map( fval => keyObj.put(fname,fval) ) }
							mdbo.put("_id",keyObj)
					}
					regFields.map{ case(fname,ftype) => 
						_render(ftype._1, getField(fname), tt.tpe.typeArgs).map( fval => mdbo.put(fname,fval) )
					}
					Some(mdbo)
				case g:PrimType  => 
					g.name match {
						case "String" | "java.lang.String" | "scala.Char" | "scala.Enumeration.Value" | "java.util.UUID" if(instance != null) => 
							Some( BsonString( clean( instance.toString ) ) )
						case "org.joda.time.DateTime" =>
							Some( BsonDateTime(instance.asInstanceOf[DateTime].toDate) )
						case _ => // Including Any type
							Some( explodeAny(instance) )
					}
				case g:CollType => 
					g.name match {
						case "scala.Option" => 
							val optVal = instance.asInstanceOf[Option[_]]
							optVal.flatMap( ov => _render(g.colTypes.head, ov, tt.tpe.typeArgs) )
						case n if(n.endsWith("Map")) => 
							val mdbo = new BsonDocument()
							val mapVal = instance.asInstanceOf[Map[_,_]]
							mapVal.map({ case (k,v) => 
								_render(g.colTypes(1), v, tt.tpe.typeArgs).map( item => mdbo.put(k.asInstanceOf[String],item))
							})
							Some(mdbo)
						case t if( t startsWith("scala.Tuple") ) => 
							val arity = """\d+""".r.findFirstIn(t).get.toInt
							val builder = new BsonArray()
							arity match {
								case 2  =>
									val cv = instance.asInstanceOf[Tuple2[_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder.add(item) )
								case 3  =>
									val cv = instance.asInstanceOf[Tuple3[_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder.add(item) )
								case 4  =>
									val cv = instance.asInstanceOf[Tuple4[_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder.add(item) )
								case 5  =>
									val cv = instance.asInstanceOf[Tuple5[_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder.add(item) )
								case 6  =>
									val cv = instance.asInstanceOf[Tuple6[_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder.add(item) )
								case 7  =>
									val cv = instance.asInstanceOf[Tuple7[_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder.add(item) )
								case 8  =>
									val cv = instance.asInstanceOf[Tuple8[_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder.add(item) )
								case 9  =>
									val cv = instance.asInstanceOf[Tuple9[_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder.add(item) )
								case 10  =>
									val cv = instance.asInstanceOf[Tuple10[_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder.add(item) )
								case 11  =>
									val cv = instance.asInstanceOf[Tuple11[_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder.add(item) )
								case 12  =>
									val cv = instance.asInstanceOf[Tuple12[_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(11), cv._12, tt.tpe.typeArgs).map( item => builder.add(item) )
								case 13  =>
									val cv = instance.asInstanceOf[Tuple13[_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(11), cv._12, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(12), cv._13, tt.tpe.typeArgs).map( item => builder.add(item) )
								case 14  =>
									val cv = instance.asInstanceOf[Tuple14[_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(11), cv._12, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(12), cv._13, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(13), cv._14, tt.tpe.typeArgs).map( item => builder.add(item) )
								case 15  =>
									val cv = instance.asInstanceOf[Tuple15[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(11), cv._12, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(12), cv._13, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(13), cv._14, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(14), cv._15, tt.tpe.typeArgs).map( item => builder.add(item) )
								case 16  =>
									val cv = instance.asInstanceOf[Tuple16[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(11), cv._12, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(12), cv._13, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(13), cv._14, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(14), cv._15, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(15), cv._16, tt.tpe.typeArgs).map( item => builder.add(item) )
								case 17  =>
									val cv = instance.asInstanceOf[Tuple17[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(11), cv._12, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(12), cv._13, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(13), cv._14, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(14), cv._15, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(15), cv._16, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(16), cv._17, tt.tpe.typeArgs).map( item => builder.add(item) )
								case 18  =>
									val cv = instance.asInstanceOf[Tuple18[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(11), cv._12, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(12), cv._13, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(13), cv._14, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(14), cv._15, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(15), cv._16, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(16), cv._17, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(17), cv._18, tt.tpe.typeArgs).map( item => builder.add(item) )
								case 19  =>
									val cv = instance.asInstanceOf[Tuple19[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(11), cv._12, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(12), cv._13, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(13), cv._14, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(14), cv._15, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(15), cv._16, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(16), cv._17, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(17), cv._18, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(18), cv._19, tt.tpe.typeArgs).map( item => builder.add(item) )
								case 20  =>
									val cv = instance.asInstanceOf[Tuple20[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(11), cv._12, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(12), cv._13, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(13), cv._14, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(14), cv._15, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(15), cv._16, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(16), cv._17, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(17), cv._18, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(18), cv._19, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(19), cv._20, tt.tpe.typeArgs).map( item => builder.add(item) )
								case 21  =>
									val cv = instance.asInstanceOf[Tuple21[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(11), cv._12, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(12), cv._13, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(13), cv._14, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(14), cv._15, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(15), cv._16, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(16), cv._17, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(17), cv._18, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(18), cv._19, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(19), cv._20, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(20), cv._21, tt.tpe.typeArgs).map( item => builder.add(item) )
								case 22  =>
									val cv = instance.asInstanceOf[Tuple22[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), cv._1, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(1), cv._2, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(2), cv._3, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(3), cv._4, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(4), cv._5, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(5), cv._6, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(6), cv._7, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(7), cv._8, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(8), cv._9, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(9), cv._10, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(10), cv._11, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(11), cv._12, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(12), cv._13, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(13), cv._14, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(14), cv._15, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(15), cv._16, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(16), cv._17, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(17), cv._18, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(18), cv._19, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(19), cv._20, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(20), cv._21, tt.tpe.typeArgs).map( item => builder.add(item) )
									_render(g.colTypes(21), cv._22, tt.tpe.typeArgs).map( item => builder.add(item) )
							}
							Some(builder)
						case _ => 
							val collVal = instance.asInstanceOf[Iterable[_]]
							val builder = new BsonArray()
							collVal.map( item => 
								_render(g.colTypes.head, item, tt.tpe.typeArgs).map( item => builder.add(item) )
							)
							Some(builder)
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
					g.custom.map{ _.render.applyOrElse(
							(MongoKind(), renderVal ),
							(k:(KindMarker, _)) => {
								throw new JsonParseException(s"No custom render function defined for kind ${k._1}",0)
								""
							})
					}.orElse{ _render(g.vcType,renderVal,tt.tpe.typeArgs) }.asInstanceOf[Option[BsonValue]]
					// } match {
					// 	case Some(x) => 
					// 		buf.append(x)
					// 		true
					// 	case None => _render(g.vcType,renderVal,buf,tt.tpe.typeArgs)
					// }
					// vc.valClassHandlers.get("mongo").flatMap(_.get(g.name).map( vcHand =>
					// 		vcHand.render(renderVal).asInstanceOf[BsonValue]
					// 	)).orElse(
					// 		_render(g.vcType,renderVal,tt.tpe.typeArgs)
					// 	)
				case g:EnumType =>
					Some( BsonString(s"${instance.toString}") )
				// case g:CustomType =>
				// 	Some( (g.renderers("mongo")(instance)).asInstanceOf[BsonValue] )
			}
		}
			
		private def explodeAny( inst:Any ) : BsonValue = inst match {
			case n if(n==null) => new BsonNull()
			case s:String   => new BsonString(s)
			case b:Boolean  => new BsonBoolean(b)
			case n:Int      => new BsonInt32(n)
			case n:Long     => new BsonInt64(n)
			case n:Double   => new BsonDouble(n)
			case l:List[_]  => 
				val builder = new BsonArray()
				l.map( item => builder.add(explodeAny(item)) )
				builder
			case m:Map[_,_] => 
				val mdbo = new BsonDocument()
				m.map({ case (k,v) => 
					mdbo.put(k.asInstanceOf[String],explodeAny(v))
				})
				mdbo
			case bs => throw new MongoParseException(s"Can't explode Any value for $bs")
		}
	}*/
}