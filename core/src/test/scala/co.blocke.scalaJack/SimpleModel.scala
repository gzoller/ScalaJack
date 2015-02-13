package co.blocke.scalajack
package test

trait Blah
case class Foo(
	name:String, 
	age:Int) extends Blah
case class Stuff(
	item:String,
	other:Blah
	)
case class WithType[T](me:T)

case class All(
	a:	Int,
	b:	java.lang.Integer,
	c:	Boolean,
	d:	java.lang.String,
	e:	String,
	f:	Float,
	g:	Double,
	h:	Long,
	i:	Char,
	j:	Null,
	k:	Byte,
	l:	Short,
	m:  Any,
	n:  Any  // different than m
	)
case class AllColl(
	a: List[Int],
	b: List[Foo],
	c: Option[Int],
	d: Option[String],
	e: List[Option[Int]],
	f: Map[String,Int],
	g: Map[Foo,Option[WithType[Int]]] // test sloppy
 	)

object Colors extends Enumeration {
  val Red, Amber, Green = Value
}
import Formats._  // try alternate Ennumeration form
case class EnumExer( a:Colors.Value, b:Format )