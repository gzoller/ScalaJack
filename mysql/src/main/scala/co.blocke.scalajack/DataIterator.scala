package co.blocke.scalajack

//import scala.reflect.runtime.universe._
import fields._
import java.sql.{ Connection, DriverManager, ResultSet }
import scala.util.Try

case class DataIterator[T]( rs:ResultSet, ccf:CaseClassField ) {

	def hasNext() = !rs.isAfterLast()

	def next()(implicit m:Manifest[T]) : T = {
		if( rs.isBeforeFirst() )
			rs.next()
		if( rs.isAfterLast )
			throw new Exception("Illegal iterator access beyond last element.")
		val t = marshal()
		rs.next()
		t
	}

	def toList()(implicit m:Manifest[T]) = {
		val ret = scala.collection.mutable.ListBuffer.empty[T]
		while( hasNext ) ret += next()
		ret.toList
	}

	private def marshal[T]()(implicit m:Manifest[T]) = {
		val raw = ccf.fields.map( decodeField( _ ) ).toMap
		ScalaJack.poof( ccf, raw ).asInstanceOf[T]
	}
	private def decodeField(fld:Field) : (String, Any) = 
		fld match {
			case f:StringField  => (f.name, rs.getString ( f.name ))
			case f:CharField    => (f.name, rs.getInt    ( f.name ).toChar)
			case f:IntField     => (f.name, rs.getInt    ( f.name ))
			case f:LongField    => (f.name, rs.getLong   ( f.name ))
			case f:FloatField   => (f.name, rs.getFloat  ( f.name ))
			case f:DoubleField  => (f.name, rs.getDouble ( f.name ))
			case f:BoolField    => (f.name, rs.getBoolean( f.name ))
			case f:OptField     => (f.name, Try(decodeField(f.subField)).toOption.flatMap( r => if( r._2 == null ) None else Some(r._2) ))
			case x              => throw new Exception("Unsupported type for field "+x+" for MySQL serialization")
			}

}