package co.blocke.scalajack

/**
 * Assumptions:
 *   -- Case classes only
 *   -- Options of value None are removed from generated output
 *   -- Default parameters are not supported at this time
 *   -- MongoKey-decorated fields must be type String and map to "_id" (Mongo's default)
 *   
 * Useful references:
 * 		http://stackoverflow.com/questions/17006271/deep-access-of-fields-in-scala-using-runtime-reflection
 */

import scala.reflect.runtime.universe._
import json._

case class ValClassHandler(
	read:(String) => Any,
	render:(Any)  => String
	)

case class VisitorContext(
	typeHint       : String  = "_hint",
	isCanonical    : Boolean = true,    // allow non-string keys in Maps--not part of JSON spec
	isValidating   : Boolean = false,
	estFieldsInObj : Int     = 128,
	valClassMap    : Map[String,ValClassHandler] = Map.empty[String,ValClassHandler]
	// hintMap : Map[String,String] = Map.empty[String,String]
	)


case class ScalaJack_JSON() extends ScalaJack[String] with JSONReadRenderFrame 

trait SupportedType[U] {
	def makeScalaJack():ScalaJack[U]
}
case class JsonType() extends SupportedType[String] {
	def makeScalaJack():ScalaJack[String] = ScalaJack_JSON()
}
  
object ScalaJack {
	def apply[R]( kind:SupportedType[R] = JsonType() ) : ScalaJack[R] = kind.makeScalaJack

	// Legacy support (JSON implied)
	private val jsonJS = apply()
	def read[T](js:String, hint:String="_hint")(implicit tt:TypeTag[T]) = jsonJS.read(js,VisitorContext(hint,true,true))
	def render[T](instance:T, hint:String="_hint")(implicit tt:TypeTag[T]) = jsonJS.render(instance,VisitorContext(hint,true,true)).toString

	/** Project fields from given master object to a view object of type T.  Field names/types must match master
	  * precisely.
	  * @param master the master object from which the smaller object is projected
	  * @return an object of type T which is a "subset" of the master
	  */
	def view[T]( master:Any )(implicit tt:TypeTag[T]) : T = 
		Analyzer.inspectByName(tt.tpe.typeSymbol.fullName) match {
			case viewClass:CCType =>
				val masterData = master.getClass.getDeclaredFields
				val args = viewClass.members.collect{ case (fname,ftype) => masterData.find(_.getName == fname).map( tf => {
					tf.setAccessible(true)
					(fname, tf.get(master))
					}) }.flatten.toMap 
				Util.poof( viewClass, args ).asInstanceOf[T]
			case _ => throw new ViewException("Type parameter must be a case class, but was instead "+tt.tpe.typeSymbol.fullName)
		}

	/** Splice a view (subset) object's fields into a master object's fields.
	  * @param view the subset object
	  * @param master master object
	  * @return the master object with the view object's corresponding fields merged/overlayed
	  */
	def spliceInto[T,U]( view:T, master:U )(implicit tu:TypeTag[U]) : U = 
		Analyzer.inspectByName(tu.tpe.typeSymbol.fullName) match {
			case masterClass:CCType =>
				val viewData = view.getClass.getDeclaredFields
				val masterData = master.getClass.getDeclaredFields
				val args = masterClass.members.collect{ case (fname,ftype) => viewData.find(_.getName == fname).map( tf => {
						tf.setAccessible(true)
						(fname,tf.get(view))
					}).orElse(masterData.find(_.getName == fname).map( tf => {
						tf.setAccessible(true)
						(fname,tf.get(master))
					}))}.flatten.toMap
				Util.poof( masterClass, args ).asInstanceOf[U]
			case _ => throw new ViewException("Type parameter must be a case class, but was instead "+tu.tpe.typeSymbol.fullName)
		}
}

trait ScalaJack[R] {
	this: ReadRenderFrame[R] =>
	def read[T](in:R, vctx:VisitorContext=VisitorContext())(implicit tt:TypeTag[T]) = {
		implicit val vc = vctx
		renderer.read(in)
	}
	def render[T](instance:T, vctx:VisitorContext=VisitorContext())(implicit tt:TypeTag[T]) = {
		implicit val vc = vctx
		renderer.render(instance)
	}
}

case class ViewException(msg:String) extends Exception(msg)