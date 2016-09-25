package co.blocke.scalajack.bytecode

case class Method(ownerType: Type, name: String, returnType: Type, parameterTypes: List[Type]) {

  def descriptor = s"(${parameterTypes.map(_.descriptor).mkString("")})${returnType.descriptor}"

  def signature = s"(${parameterTypes.map(_.signature).mkString("")})${returnType.signature}"

}
