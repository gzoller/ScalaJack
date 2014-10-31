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

import fields._
import scala.reflect.runtime.universe._
import compat._

object ScalaJack {
	type JSON = String

	val HINT = "_hint"

	// This hook function, which does nothing in core, is fed implicitly into the Analyzer.
	// It takes (fieldType,fieldName) as inputs.  It's an extension mechanism for bolt-on modules
	// to map class names/types to Field values.  Look at Mongo's ObjectId for an example of use (Mongo.scala)
	implicit val hookFn : (String,String)=>Option[Field] = (x,y)=>None
	
	/** Read a JSON-encoded case class
	  * @param js JSON string to read
	  * @param hint type hint label
	  * @param ext set true if you want to support extended JSON (you must supply)
	  * @return An object of type T
	  */
	def read[T]( js:JSON, hint:String = HINT, ext:Boolean = false )(implicit m:Manifest[T]) : T = {
		val jp = JsonEmitter(js)
		jp.nextToken
		_readRender(
			_.readValue(jp,ext,hint,ClassContext("scala.collection.immutable.List","")),
			_.readValue(jp,ext,hint,ClassContext("scala.collection.immutable.Map","")),
			_.readClass(jp, ext, hint)
			).asInstanceOf[T]
	}
	def read[T]( js:JSON, ext:Boolean )(implicit m:Manifest[T]) : T = read[T]( js, HINT, ext )

	/** Render a JSON-encoded case class
	  * @param target an object to render to JSON
	  * @param hint type hint label
	  * @param ext set true if you want to support extended JSON (you must supply)
	  * @return JSON string
	  */
	def render[T]( target:T, hint:String = HINT, ext:Boolean = false )(implicit m:Manifest[T]) : JSON = {
		val sb = new StringBuilder
		_readRender(
			_.render(sb, target, None, ext, hint),
			_.render(sb, target, None, ext, hint),
			_.asInstanceOf[Field].render(sb, target, None, ext, hint)
			)
		sb.toString
	}
	def render[T]( target:T, ext:Boolean )(implicit m:Manifest[T]) : JSON = render[T]( target, HINT, ext )

	private[scalajack] def _readRender[T]( listFn:(Field)=>Any, mapFn:(Field)=>Any, classFn:(ClassOrTrait)=>Any )(implicit m:Manifest[T], hookFn:(String,String)=>Option[Field]) : Any = {
		if( m.runtimeClass.getSimpleName == "List" ) {
			val Analyzer.xtractTypes(subtype) : String = (typeOf[T]).toString
			val analyzer = Analyzer()
			listFn( ListField( "", analyzer.typeMap(Analyzer.convertType(subtype))("") ) )
		} else if( m.runtimeClass.getSimpleName == "Map" ) {
			val analyzer = Analyzer()
			mapFn( MapField( "", analyzer.typeMap( Analyzer.convertType(Analyzer.typeSplit((typeOf[T]).toString)(1)))("") ) )
		} else
			classFn( Analyzer.inspect(m.runtimeClass.getName).asInstanceOf[ClassOrTrait] )
	}

	/** Project fields from given master object to a view object of type T.  Field names/types must match master
	  * precisely.
	  * @param master the master object from which the smaller object is projected
	  * @return an object of type T which is a "subset" of the master
	  */
	def view[T]( master:Any )(implicit m:Manifest[T]) : T = {
		val viewClassField = Analyzer.inspect(m.runtimeClass.getName).asInstanceOf[CaseClassField]
		val masterData = master.getClass.getDeclaredFields
		val args = viewClassField.fields.collect{ case f => masterData.find(_.getName == f.name).map( tf => {
			tf.setAccessible(true)
			tf.get(master)
			}) }.flatten.toArray.asInstanceOf[Array[AnyRef]]
		viewClassField.applyMethod.invoke( viewClassField.caseObj, args:_* ).asInstanceOf[T]
	}

	/** Splice a view (subset) object's fields into a master object's fields.
	  * @param view the subset object
	  * @param master master object
	  * @return the master object with the view object's corresponding fields merged/overlayed
	  */
	def spliceInto[T,U]( view:T, master:U )(implicit m:Manifest[U]) : U = {
		val masterClassField = Analyzer.inspect(m.runtimeClass.getName).asInstanceOf[CaseClassField]
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
