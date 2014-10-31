package co.blocke.scalajack

import fields._
import scala.reflect.runtime.universe._
import java.sql.{Connection, PreparedStatement, ResultSet}
import scala.util.Try
import java.sql.Types._

import mysql._

object MySQLScalaJack {

	def select[T]( where:String, extras:String="" )(implicit m:Manifest[T], c:Connection) : DataIterator[T] = {
		val stmt    = c.createStatement()
		val ccf     = Analyzer.inspect(m.runtimeClass.getName).asInstanceOf[CaseClassField]
		val fields  = ccf.fields.map(_.name).mkString(",")
		val table   = ccf.collAnno.getOrElse(throw new Exception("Must supply @Collection annotation for class "+m.runtimeClass.getName))
		val where2  = { if(where.length == 0) "true" else where }
		DataIterator( stmt.executeQuery( s"""SELECT $fields FROM $table WHERE $where2 $extras""" ), ccf )
	}

	// Basically an upsert
	def insertInto[T]( data:List[T] )(implicit m:Manifest[T], c:Connection) = {
		val ccf     = Analyzer.inspect(m.runtimeClass.getName).asInstanceOf[CaseClassField]

		val table   = ccf.collAnno.getOrElse(throw new Exception("Must supply @Collection annotation for class "+m.runtimeClass.getName))
		val fields  = ccf.fields.map(_.name).mkString("(",",",")")
		val qVals   = ("?"*ccf.fields.size).toList.mkString("(",",",")")
		val qUpVals = ccf.fields.filterNot( _.hasDBKeyAnno ).map( _.name+"=?" ).toList.mkString(",")
		val stmt    = c.prepareStatement(s"""INSERT INTO $table $fields VALUES $qVals ON DUPLICATE KEY UPDATE $qUpVals""")

		c.setAutoCommit(false)
		data.foreach( row =>{
			val cz = row.getClass
			setRow( stmt, row, cz, ccf )
			stmt.addBatch()
			})
		val count = stmt.executeBatch()
		c.commit()
		count
	}

	private def setRow[T]( stmt:PreparedStatement, row:Any, cz:Class[T], ccf:CaseClassField ) = {
		var fi = 1
		var ui = 1+ccf.fields.size
		ccf.fields.map( f => {
			val targetField = cz.getDeclaredField(f.name)
			targetField.setAccessible(true)
			val ftype = targetField.getType.getName
			val fval = targetField.get(row)

			setField(stmt,fi,f,fval)
			fi = fi+1
			if( !f.hasDBKeyAnno ) {
				setField(stmt,ui,f,fval)
				ui = ui+1
			}
		})
	}

	private def setRowUpdate[T]( stmt:PreparedStatement, row:Any, cz:Class[T], ccf:CaseClassField ) = {
		var fi = 1
		var ui = 1
		ccf.fields.map( f => {
			val targetField = cz.getDeclaredField(f.name)
			targetField.setAccessible(true)
			val ftype = targetField.getType.getName
			val fval = targetField.get(row)

			setField(stmt,fi,f,fval)
			fi = fi+1
			if( !f.hasDBKeyAnno ) {
				setField(stmt,ui,f,fval)
				ui = ui+1
			}
		})
	}

	private def setField(stmt:PreparedStatement, idx:Int, fld:Field, v:Any) {
		fld match {
			case f:StringField  => stmt.setString(idx, v.asInstanceOf[String])
			case f:CharField    => stmt.setInt(idx, v.asInstanceOf[Char].toInt)
			case f:IntField     => stmt.setInt(idx, v.asInstanceOf[Int])
			case f:LongField    => stmt.setLong(idx, v.asInstanceOf[Long])
			case f:FloatField   => stmt.setFloat(idx, v.asInstanceOf[Float])
			case f:DoubleField  => stmt.setDouble(idx, v.asInstanceOf[Double])
			case f:BoolField    => stmt.setBoolean(idx, v.asInstanceOf[Boolean])
			case f:OptField     => v.asInstanceOf[Option[_]].fold( stmt.setNull(idx,sqlType(f.subField)) )( v2 => setField(stmt,idx, f.subField, v) )
			case x              => throw new Exception("Unsupported type for field "+x+" for MySQL serialization")
		}
	}

	private def sqlType(fld:Field) : Int = fld match {
		case f:StringField  => VARCHAR
		case f:CharField    => INTEGER
		case f:IntField     => INTEGER
		case f:LongField    => INTEGER
		case f:FloatField   => FLOAT
		case f:DoubleField  => DOUBLE
		case f:BoolField    => BOOLEAN
	}
}