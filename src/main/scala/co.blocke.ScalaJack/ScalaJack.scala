package co.blocke.scalajack

/**
 * Assumptions:
 *   -- Case classes only
 *   -- Options of value None are removed from generated JSON
 *   -- Default parameters are not supported at this time
 *   -- Simple types supported:
 *         Int, Boolean, Long, String, Enumeration.Value
 *   -- Collections/"containers" supported:
 *         List (mutable (ListBuffer) and immutable)
 *         Map  (mutable and immutable)
 *         Option
 *   -- MongoKey-decorated fields must be type String and map to "_id" (Mongo's default)
 *   
 * Useful references:
 * 		http://stackoverflow.com/questions/17006271/deep-access-of-fields-in-scala-using-runtime-reflection
 */

import com.mongodb.casbah.Imports._
import fields._
import scala.reflect.runtime.universe._
import compat._

object ScalaJack {
	type JSON = String

	private val hint = "_hint"
	
	/**
	 * Read a JSON-encoded case class
	 */
	def read[T]( js:JSON, ext:Boolean = false, hint:String = hint )(implicit m:Manifest[T]) : T = {
		val jp = JsonEmitter(js)
		jp.nextToken
		_readRender(
			_.readValue(jp,ext,hint,ClassContext("scala.collection.immutable.List","")),
			_.readValue(jp,ext,hint,ClassContext("scala.collection.immutable.Map","")),
			_.readClass(jp, ext, hint)
			).asInstanceOf[T]
	}

	/**
	 * Read a case class from a DBObject (MongoDB)
	 */
	def readDB[T]( src:DBObject, hint:String = hint )(implicit m:Manifest[T]) : T = {
		_readRender(
			_.readValueDB(src,hint,ClassContext("scala.collection.immutable.List","")),
			_.readValueDB(src,hint,ClassContext("scala.collection.immutable.Map","")),
			_.readClassDB(src, hint)
			).asInstanceOf[T]
	}

	/**
	 * Render a JSON-encoded case class
	 */
	def render[T]( target:T, ext:Boolean = false, hint:String = hint )(implicit m:Manifest[T]) : JSON = {
		val sb = new StringBuilder
		_readRender(
			_.render(sb, target, None, ext, hint),
			_.render(sb, target, None, ext, hint),
			_.asInstanceOf[Field].render(sb, target, None, ext, hint)
			)
		sb.toString
	}

	/**
	 * Render a case class to DBObject
	 */
	def renderDB[T]( target:T, hint:String = hint )(implicit m:Manifest[T]) : DBObject = {
		_readRender(
			_.renderDB(target, None, hint),
			_.renderDB(target, None, hint),
			_.renderClassDB(target, hint)
			) match {
				case li:MongoDBList => li.asInstanceOf[MongoDBList]
				case x              => x.asInstanceOf[DBObject]
			}
	}

	private def _readRender[T]( listFn:(Field)=>Any, mapFn:(Field)=>Any, classFn:(ClassOrTrait)=>Any )(implicit m:Manifest[T]) : Any = {
		if( m.runtimeClass.getSimpleName == "List" ) {
			val Analyzer.xtractTypes(subtype) : String = (typeOf[T]).toString
			val analyzer = Analyzer()
			listFn( ListField( "", analyzer.typeMap(Analyzer.convertType(subtype))("") ) )
		} else if( m.runtimeClass.getSimpleName == "Map" ) {
			val analyzer = Analyzer()
			mapFn( MapField( "", analyzer.typeMap( Analyzer.convertType(Analyzer.typeSplit((typeOf[T]).toString)(1)))("") ) )
		} else
			classFn( Analyzer(m.runtimeClass.getName).asInstanceOf[ClassOrTrait] )
	}

	def view[T]( master:Any )(implicit m:Manifest[T]) : T = {
		val viewClassField = Analyzer(m.runtimeClass.getName).asInstanceOf[CaseClassField]
		val masterData = master.getClass.getDeclaredFields
		val args = viewClassField.fields.collect{ case f => masterData.find(_.getName == f.name).map( tf => {
			tf.setAccessible(true)
			tf.get(master)
			}) }.flatten.toArray.asInstanceOf[Array[AnyRef]]
		viewClassField.applyMethod.invoke( viewClassField.caseObj, args:_* ).asInstanceOf[T]
	}

	def spliceInto[T,U]( view:T, master:U )(implicit m:Manifest[U]) : U = {
		val masterClassField = Analyzer(m.runtimeClass.getName).asInstanceOf[CaseClassField]
		val viewData = view.getClass.getDeclaredFields
		val masterData = master.getClass.getDeclaredFields
		val args = masterClassField.fields.collect{ case f => viewData.find(_.getName == f.name).map( tf => {
				tf.setAccessible(true)
				tf.get(view)
			}).orElse(masterData.find(_.getName == f.name).map( tf => {
				tf.setAccessible(true)
				tf.get(master)
			}))}.flatten.toArray.asInstanceOf[Array[AnyRef]]
		masterClassField.applyMethod.invoke( masterClassField.caseObj, args:_* ).asInstanceOf[U]
	}

	/**
	 * Magically create an instance of a case class given a map of name->value parameters.
	 * (Reflects on the apply method of the case class' companion object.)
	 * It's a quick way to materialize a cse class represented by a Map, which is how ScalaJack uses it.
	 * ScalaJack parses the JSON, building a value Map as it goes.  When the JSON object has been parsed
	 * ScalaJack calls poof to build the case class from the Map.
	 */
	private[scalajack] def poof( classField:CaseClassField, data:Map[String,Any] ) : Any = {
		val args = classField.fields.collect{ case f => data.get(f.name).getOrElse(None) }.toArray.asInstanceOf[Array[AnyRef]]
		classField.applyMethod.invoke( classField.caseObj, args:_* )
	}
}
