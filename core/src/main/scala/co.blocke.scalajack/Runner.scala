package co.blocke.scalajack

import model._
import typeadapter._

//case class Fact(fall: TypeAdapter[_]) extends TypeAdapterFactory.FromClassSymbol {
//
//  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
//    if (classSymbol.name.toString == "String") {
//      (fall).asInstanceOf[TypeAdapter[T]]
//    } else
//      next.typeAdapterOf[T]
//  }
//}

object Runner extends App {

  val sj = ScalaJack()

  //  val falling = new FallbackTypeAdapter(Some(sj.context.typeAdapterOf[String]).asInstanceOf[Option[co.blocke.scalajack.model.TypeAdapter[Any]]], sj.context.typeAdapterOf[Int])
  //  val sj2 = ScalaJack().withAdapters(Fact(falling))
  //  sj2.render[String](25)
}
