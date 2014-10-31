package co.blocke.scalajack

import fields._
import scala.reflect.runtime.universe._
import java.sql.{Connection, DriverManager, ResultSet}
import scala.util.Try

trait MySQLFunctions[T] {
	val ccfld     : CaseClassField
	val connMaker : () => Connection
	val table     : String

	def select[T]( where:String )(implicit m:Manifest[T]) : DataIterator[T] = {
		Try( connMaker() ).toOption.map( c => {
			val stmt = c.createStatement()
			new DataIterator[T](){
				val conn:Connection    = c
				val ccf:CaseClassField = ccfld
				val fields             = ccf.fields.map(_.name).mkString(",")
				val rs:ResultSet       = stmt.executeQuery( s"""SELECT $fields FROM $table WHERE $where""" )
			}
		}).orElse( throw new Exception("Attempt to connect to database failed.") )
		.get
	}	
}
