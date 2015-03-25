package co.blocke.scalajack

import com.mongodb.casbah.Imports._
import fields._
import scala.reflect.runtime.universe._
import compat._

import mongo._

object MongoScalaJack {
	
	/** Read a case class from a DBObject (MongoDB)
	  */
	def readDB[T]( src:DBObject, hint:String = ScalaJack.HINT )(implicit m:Manifest[T]) : T = {
		ScalaJack._readRender(
			_.readValueDB(src,hint,ClassContext("scala.collection.immutable.List","")),
			_.readValueDB(src,hint,ClassContext("scala.collection.immutable.Map","")),
			_.readClassDB(src, hint)
			).asInstanceOf[T]
	}

	/** Render a case class to DBObject
	  */
	def renderDB[T]( target:T, hint:String = ScalaJack.HINT )(implicit m:Manifest[T]) : DBObject = {
		ScalaJack._readRender(
			_.renderDB(target, None, hint),
			_.renderDB(target, None, hint),
			_.renderClassDB(target, hint)
			) match {
				case li:MongoDBList => li.asInstanceOf[MongoDBList]
				case x              => x.asInstanceOf[DBObject]
			}
	}
}
