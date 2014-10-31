package co.blocke.scalajack

@Collection(name="people")
case class Person(
	@DBKey 
	name:String, 
	age:Int, 
	crazy:Double )

@Collection(name="stuff")
case class Stuff( 
	@DBKey 
	str:String, 
	i:Int, 
	b:Boolean, 
	maybe:Option[String] )