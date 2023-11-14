package co.blocke.scalajack
package parser

import scala.collection.Factory

enum Expects{
    case ExpectBoolean, ExpectString, ExpectLong, ExpectBigLong, ExpectBigDouble, ExpectDouble, ExpectList, ExpectClass, ExpectObject, ExpectTuple
}

// Don't need Expects for:
//   expectList: ListInstruction is the only acceptable instruction with arity-1 (1 paramter)
//   expectObject: ObjectInstruction is the only taker here: 2 params [K,V]
//   expectClass: ClassInstrunction is the only applicable value


sealed trait Instruction:
    val expect: Expects
    type Z

// Expects function requires no parameters
abstract class SimpleInstruction[I,O]() extends Instruction:
    type Z = O
    val expect: Expects
    def transform(in: Either[ParseError, I]): Either[ParseError, O]

// Expects function requires a single parameter
abstract class ListInstruction[E,T](val elementInstruction: Instruction) extends Instruction:
    type Z = T
    def transform(in: Either[ParseError, List[elementInstruction.Z]]): Either[ParseError, T]

abstract class ClassInstruction[T](val fieldInstructions: Map[String,Instruction]) extends Instruction:
    type Z = T
    def transform(in: Either[ParseError, Map[String,Any]]): Either[ParseError, T]
    
//-------------------------------------------------------------

case class BooleanInstruction() extends SimpleInstruction[Boolean,Boolean]:
    val expect: Expects = Expects.ExpectBoolean
    def transform(in: Either[ParseError, Boolean]): Either[ParseError, Boolean] = in

case class CharInstruction() extends SimpleInstruction[String,Char]:
    val expect: Expects = Expects.ExpectString
    def transform(in: Either[ParseError, String]): Either[ParseError, Char] = in.map(_.head)

case class IntInstruction() extends SimpleInstruction[Long,Int]:
    val expect: Expects = Expects.ExpectLong
    def transform(in: Either[ParseError, Long]): Either[ParseError, Int] = in.map(_.intValue)

case class StringInstruction() extends SimpleInstruction[String,String]:
    val expect: Expects = Expects.ExpectString
    def transform(in: Either[ParseError, String]): Either[ParseError, String] = in

case class SeqInstruction[E,T](override val elementInstruction: Instruction)(using Factory[E,T]) extends ListInstruction[E,T](elementInstruction):
    val expect: Expects = Expects.ExpectList
    inline def collectionConvert[X,Y](in:List[X])(using Factory[X,Y]) = in.to( summon[Factory[X,Y]] )
    def transform(in: Either[ParseError, List[elementInstruction.Z]]): Either[ParseError, T] = in.map(e => collectionConvert[E,T](e.asInstanceOf[List[E]]))

case class ScalaClassInstruction[T](override val fieldInstructions: Map[String,Instruction], instantiator: Map[String,?]=>T) extends ClassInstruction[T](fieldInstructions):
    val expect: Expects = Expects.ExpectClass
    def transform(in: Either[ParseError, Map[String,Any]]): Either[ParseError,T] = in.map(instantiator(_))
