package co.blocke.scalajack

import fields._
import scala.language.implicitConversions
import java.sql.{Connection, DriverManager, ResultSet}

package object mysql {

	implicit def mysqlSJ( sj:ScalaJack.type ) = MySQLScalaJack

}