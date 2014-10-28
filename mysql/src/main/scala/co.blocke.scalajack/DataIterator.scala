package co.blocke.scalajack

//import scala.reflect.runtime.universe._
import fields._
import java.sql.{Connection, DriverManager, ResultSet}
import scala.util.Try

trait DataIterator[T] extends Iterator[T] {
	val conn : Connection
	val rs   : ResultSet
	val ccf  : CaseClassField

	def hasNext() = {
		val noMore = rs.isLast
		if( noMore ) 
			close
		noMore
		}

	def next() : T = 
		if( !rs.next ) {
			close()
			throw new NoSuchElementException()
		} else
			marshal()

	def close() = Try( conn.close ).toOption

	private def marshal() = {
		val raw = ccf.fields.map( _ match {
			case f:StringField  => (f.name, rs.getString( f.name ))
			case f:CharField    => (f.name, rs.getInt( f.name ).toChar)
			case f:IntField     => (f.name, rs.getInt( f.name ))
			case f:LongField    => (f.name, rs.getLong( f.name ))
			case f:FloatField   => (f.name, rs.getFloat( f.name ))
			case f:DoubleField  => (f.name, rs.getDouble( f.name ))
			case f:BoolField    => (f.name, rs.getBoolean( f.name ))
			case x              => throw new Exception("Unsupported type for field "+x+" for MySQL serialization")
			}).toMap
		ScalaJack.poof( ccf, raw ).asInstanceOf[T]
	}
}