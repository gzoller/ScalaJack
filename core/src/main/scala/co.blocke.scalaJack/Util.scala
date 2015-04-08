package co.blocke
package scalajack

import scala.reflect.runtime.universe._

object Util {
	/**
	 * Magically create an instance of a case class given a map of name->value parameters.
	 * (Reflects on the apply method of the case class' companion object.)
	 * It's a quick way to materialize a cse class represented by a Map, which is how ScalaJack uses it.
	 * ScalaJack parses the JSON, building a value Map as it goes.  When the JSON object has been parsed
	 * ScalaJack calls poof to build the case class from the Map.
	 */
	private[scalajack] def poof[T]( cc:CCType, data:Map[String,Any] )(implicit tt:TypeTag[T]) : Any = {
		// Get constructor arguments in right order, we should.
		val args = cc.members.collect{ case (fname,ftype) => data.get(fname).getOrElse(None) }.toArray.asInstanceOf[Array[AnyRef]]
		Class.forName(cc.name).getConstructors()(0).newInstance(args:_*)

		// Can go to this one after Scala fixes their bug: https://issues.scala-lang.org/browse/SI-9102
		// At that point we'll compute ctor differently (see Analyzer.scala).  We also won't need all the special handling
		// of SjValueClass objects in _parse in this file.
		// cc.ctor.apply( args: _* )   //-- didn't seem to work for value classes, but newInstance did... hmm...
	}
}