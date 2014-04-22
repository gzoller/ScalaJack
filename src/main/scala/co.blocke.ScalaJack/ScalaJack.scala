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

import com.fasterxml.jackson.core.JsonFactory
import com.mongodb.casbah.Imports._
import fields._
import scala.reflect.runtime.universe._

object ScalaJack {
	type JSON = String
	
	private val jsFactory = new JsonFactory();
	private val hint = "_hint"
	
	/**
	 * Render a JSON-encoded case class
	 */
	def render[T]( target:T, ext:Boolean = false, hint:String = hint )(implicit m:Manifest[T]) : JSON = {
		val sb = new StringBuilder
		// Note: Was using Analyzer(target.getClass.getName) here but this failed to pick up 
		// top-level trait class name.  m.toString did the job.
		if( m.runtimeClass.getSimpleName == "List" ) {
		    val Analyzer.xtractTypes(subtype) = (typeOf[T]).toString
			ListField( "", Analyzer( Analyzer.convertType(subtype)) ).render(sb, target, None, ext, hint)
		} else if( m.runtimeClass.getSimpleName == "Map" ) 
			MapField( "", Analyzer( Analyzer.convertType(Analyzer.typeSplit((typeOf[T]).toString)(1))) ).render(sb, target, None, ext, hint)
	    else 
			Analyzer(m.runtimeClass.getName).render(sb, target, None, ext, hint)
		sb.toString
	}

	/**
	 * Read a JSON-encoded case class
	 */
	def read[T]( js:JSON, ext:Boolean = false, hint:String = hint )(implicit m:Manifest[T]) : T = {
		val jp = jsFactory.createParser(js)
		jp.nextToken
		if( m.runtimeClass.getSimpleName == "List" ) {
		    val Analyzer.xtractTypes(subtype) = (typeOf[T]).toString
			ListField( "", Analyzer(Analyzer.convertType(subtype) ) ).readValue(jp,ext,hint,ClassContext("scala.collection.immutable.List","")).asInstanceOf[T]
		} else if( m.runtimeClass.getSimpleName == "Map" ) 
			MapField( "", Analyzer( Analyzer.convertType(Analyzer.typeSplit((typeOf[T]).toString)(1))) ).readValue(jp,ext,hint,ClassContext("scala.collection.immutable.Map","")).asInstanceOf[T]
	    else
			Analyzer(m.runtimeClass.getName).asInstanceOf[ClassOrTrait].readClass(jp, ext, hint).asInstanceOf[T]
	}

	/**
	 * Render a case class to DBObject
	 */
	def renderDB[T]( target:T, hint:String = hint )(implicit m:Manifest[T]) : DBObject = {
		if( m.runtimeClass.getSimpleName == "List" ) {
		    val Analyzer.xtractTypes(subtype) = (typeOf[T]).toString
			ListField( "", Analyzer(Analyzer.convertType(subtype) ) ).renderDB(target, None, hint).asInstanceOf[MongoDBList]
		} else if( m.runtimeClass.getSimpleName == "Map" ) 
			MapField( "", Analyzer( Analyzer.convertType(Analyzer.typeSplit((typeOf[T]).toString)(1))) ).renderDB(target, None, hint).asInstanceOf[DBObject]
	    else
			Analyzer(m.runtimeClass.getName).asInstanceOf[ClassOrTrait].renderClassDB(target, hint).asInstanceOf[DBObject]
	}

	/**
	 * Read a case class from a DBObject (MongoDB)
	 */
	def readDB[T]( src:DBObject, hint:String = hint )(implicit m:Manifest[T]) : T = {
		if( m.runtimeClass.getSimpleName == "List" ) {
		    val Analyzer.xtractTypes(subtype) = (typeOf[T]).toString
			ListField( "", Analyzer(Analyzer.convertType(subtype) ) ).readValueDB(src,hint,ClassContext("scala.collection.immutable.List","")).asInstanceOf[T]
		} else if( m.runtimeClass.getSimpleName == "Map" ) 
			MapField( "", Analyzer( Analyzer.convertType(Analyzer.typeSplit((typeOf[T]).toString)(1))) ).readValueDB(src,hint,ClassContext("scala.collection.immutable.Map","")).asInstanceOf[T]
	    else
			Analyzer(m.runtimeClass.getName).asInstanceOf[ClassOrTrait].readClassDB(src, hint).asInstanceOf[T]
	}

	/** 
	 * Render a "naked" list to DBObject
	 */
	// def renderListDB[T]( target:List[T], hint:String = hint )(implicit m:Manifest[T]) : MongoDBList = {
	// 	if( target.size == 0 ) MongoDBList.empty
	// 	else ListField( "", Analyzer(Analyzer.convertType(m.runtimeClass.getName)) ).renderDB(target, None, hint).asInstanceOf[MongoDBList]
	// }

	/**
	 * Read a "naked" list from DBObject into List()
	 */
	// def readListDB[T]( src:MongoDBList, hint:String = hint )(implicit m:Manifest[T]) : List[T] = {
	// 	ListField( "", Analyzer(Analyzer.convertType(m.runtimeClass.getName)) ).readValueDB(src,hint,ClassContext("scala.collection.List","")).asInstanceOf[List[T]]
	// }

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
