package co.blocke.scalajack

import java.sql.{ Connection, DriverManager }

trait db {
	def _withConnection[A](url: String)(block: Connection => A): A = {
		val connection = getConnectionByUrl(url)
		try {
			block(connection)
		} finally {
			connection.close()
		}
	}

	def _withTransaction[A](url: String)(block: Connection => A): A = {
		_withConnection(url) { connection =>
			try {
				connection.setAutoCommit(false)
				val r = block(connection)
				connection.commit()
				r
			} catch {
				case e: Throwable => connection.rollback(); throw e
			}
		}
	}

	private def getConnectionByUrl(url: String) = DriverManager.getConnection(url)
}

object db extends db {
	Class.forName("com.mysql.jdbc.Driver").newInstance // load the driver

	private val dbs = scala.collection.mutable.Map.empty[String,String] // label -> url

	def addDbs( newDbs:Map[String,String] ) = dbs ++= newDbs

	/** block-style access to a connection
	  *  @param database name as given in application.conf
	  */
	def withConnection[A](dbName: String)(block: Connection => A): A = this._withConnection(dbs(dbName))(block)

	/** block-style access to a transaction
	  *  @param database name as given in application.conf
	  */
	def withTransaction[A](dbName: String)(block: Connection => A): A = this._withTransaction(dbs(dbName))(block)

}