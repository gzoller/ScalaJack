package co.blocke.scalajack

import fields._
import scala.reflect.runtime.universe._
import java.sql.{Connection, DriverManager, ResultSet}
import scala.util.Try

object MySQLScalaJack {

	val noop = classOf[com.mysql.jdbc.Driver] // load the driver

	def database[T]( url:String, collection:String )(implicit m:Manifest[T]) = new MySQLFunctions[T](){
		val ccfld     = Analyzer(m.runtimeClass.getName).asInstanceOf[CaseClassField]
		val connMaker = () => DriverManager.getConnection(url)
		val table     = collection
	}

}
