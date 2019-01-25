package co.blocke.scalajack
package util

object BijectiveFunctionHelpers {

  import BijectiveFunction.Implicits._

  val typeToSymbol: BijectiveFunction[Type, Symbol] = {
    val apply = (tpe: Type) => tpe.typeSymbol
    val unapply = (typeSymbol: Symbol) => typeSymbol.asType.toType
    apply ⇄ unapply
  }
  val symbolToType: BijectiveFunction[Symbol, Type] = typeToSymbol.inverse

  val symbolToClassSymbol: BijectiveFunction[Symbol, ClassSymbol] = {
    val apply = (symbol: Symbol) => symbol.asClass
    val unapply = (classSymbol: ClassSymbol) => classSymbol
    apply ⇄ unapply
  }

  val typeToClassSymbol: BijectiveFunction[Type, ClassSymbol] = typeToSymbol andThen symbolToClassSymbol
  val classSymbolToType: BijectiveFunction[ClassSymbol, Type] = typeToClassSymbol.inverse

  val fullNameToClassSymbol: BijectiveFunction[String, ClassSymbol] = {
    val apply = (fullName: String) => staticClass(fullName)
    val unapply = (symbol: ClassSymbol) => symbol.fullName
    apply ⇄ unapply
  }
  val classSymbolToFullName: BijectiveFunction[ClassSymbol, String] = fullNameToClassSymbol.inverse

  val fullNameToType: BijectiveFunction[String, Type] = fullNameToClassSymbol andThen classSymbolToType
  val typeToFullName: BijectiveFunction[Type, String] = fullNameToType.inverse
}