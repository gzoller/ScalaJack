package co.blocke.scalajack

// import fields._
import scala.language.implicitConversions
import com.mongodb.casbah.Imports._

package object mongo {
	implicit def mongoOID( s:String ) = new ObjectId( s)
}

case class ScalaJack_Mongo() extends ScalaJack[MongoDBObject] with mongo.MongoReadRenderFrame 

case class MongoType() extends SupportedType[MongoDBObject] {
	def makeScalaJack():ScalaJack[MongoDBObject] = ScalaJack_Mongo()
}

/*


	import MongoCCF._
	import MongoPrimative._
	import MongoEnum._
	import MongoList._
	import MongoMap._
	import MongoOpt._
	import MongoTrait._
	import MongoValueClass._

	implicit val hookFn:(String,String)=>Option[Field] = 
		( fieldType, fieldName ) => 
			if( fieldType == "org.bson.types.ObjectId" ) 
				Some(ObjectIdField( fieldName ))
			else 
				None
	
	implicit def mongoSJ( sj:ScalaJack.type ) = MongoScalaJack

	implicit def mongoCorT( cot:ClassOrTrait ) = cot match {
		case x:CaseClassField  => MongoCaseClassField(x)
		case x:TraitField      => MongoTraitField(x)
	}

	implicit def mongoField( f:Field ) = f match {
		case x:StringField     => MongoStringField(x)
		case x:JodaField       => MongoJodaField(x)
		case x:UUIDField       => MongoUUIDField(x)
		case x:IntField        => MongoIntField(x)
		case x:CharField       => MongoCharField(x)
		case x:LongField       => MongoLongField(x)
		case x:FloatField      => MongoFloatField(x)
		case x:DoubleField     => MongoDoubleField(x)
		case x:BoolField       => MongoBoolField(x)
		case x:CaseClassField  => MongoCaseClassField(x)
		case x:EnumField       => MongoEnumField(x)
		case x:ListField       => MongoListField(x)
		case x:MapField        => MongoMapField(x)
		case x:ObjectIdField   => x  // already a MongoField
		case x:OptField        => MongoOptField(x)
		case x:TraitField      => MongoTraitField(x)
		case x:ValueClassField => MongoValueClassField(x)
		case x:ValueClassFieldUnboxed => MongoValueClassFieldUnboxed(x)
	}
	*/
