package co.blocke.scalajack

class TermDeserializer[T](next: Deserializer[T])(implicit tt: TypeTag[T]) extends Deserializer[T] {

  override def deserializeFromNothing[AST, S](path: Path)(implicit ops: AstOps[AST, S]): DeserializationResult[T] =
    next.deserializeFromNothing(path)

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[T] =
    next.deserialize(path, ast) match {
      case success @ DeserializationSuccess(_) =>
        success

      case failure @ DeserializationFailure(_) if failure.isUnsupported(path) =>
        ast match {
          case AstString(termName) =>
            if (tt.tpe.typeSymbol.isClass) {
              val classSymbol: ClassSymbol = tt.tpe.typeSymbol.asClass

              /*
              val companionSymbol: Symbol = classSymbol.companion
              if (companionSymbol.isModule) {
                val companionModuleSymbol: ModuleSymbol = companionSymbol.asModule

                val termMember = companionModuleSymbol.typeSignature.member(TermName(termName))
                //                println(termMember)
              }
              */

              val ownerType: Type = classSymbol.owner.typeSignature
              // Perhaps the term refers to a sibling type?
              val siblingSymbol: Symbol = ownerType.member(TermName(termName))
              if (siblingSymbol.isModule) {
                val st = SingleType(ownerType, siblingSymbol)
                //                val st = siblingSymbol.asModule.thisPrefix
                //                val st = SingleType(ownerType, siblingSymbol)
                val siblingModule = reflectModule(siblingSymbol.asModule).instance
                return DeserializationSuccess(TypeTagged[T](siblingModule.asInstanceOf[T], st))
              }
            }
            failure

          case _ =>
            failure
        }

      case failure @ DeserializationFailure(_) =>
        failure
    }

}
