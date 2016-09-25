package co.blocke.scalajack.bytecode

case class Invocation(ownerType: Type, name: String, returnType: Type, parameterTypes: List[Type], isInterface: Boolean) {

  def descriptor: String = {
    s"(${parameterTypes.map(_.descriptor).mkString})${returnType.descriptor}"
  }

}
