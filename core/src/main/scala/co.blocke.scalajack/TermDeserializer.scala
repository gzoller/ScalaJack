package co.blocke.scalajack

class TermDeserializer[T](next: Deserializer[T])(implicit tt: TypeTag[T]) extends Deserializer[T] {

  override def deserializeFromNothing[J](path: Path)(implicit ops: JsonOps[J]): DeserializationResult[T] =
    next.deserializeFromNothing(path)

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[T] =
    next.deserialize(path, json) match {
      case success @ DeserializationSuccess(_) =>
        success

      case failure @ DeserializationFailure(_) if failure.isUnsupported(path) =>
        json match {
          case JsonString(termName) =>
            if (tt.tpe.typeSymbol.isClass) {
              val classSymbol: ClassSymbol = tt.tpe.typeSymbol.asClass

              val companionSymbol: Symbol = classSymbol.companion
              if (companionSymbol.isModule) {
                val companionModuleSymbol: ModuleSymbol = companionSymbol.asModule

                val termMember = companionModuleSymbol.typeSignature.member(TermName(termName))
                println(termMember)
              }

              val ownerType: Type = classSymbol.owner.typeSignature
              // Perhaps the term refers to a sibling type?
              val siblingSymbol: Symbol = ownerType.member(TermName(termName))
              if (siblingSymbol.isModule) {
                val st = SingleType(ownerType, siblingSymbol)
                //                val st = siblingSymbol.asModule.thisPrefix
                //                val st = SingleType(ownerType, siblingSymbol)
                val siblingModule = reflectModule(siblingSymbol.asModule).instance
                println(st)
                println(siblingSymbol.asModule)
                return DeserializationSuccess(TypeTagged[T](siblingModule.asInstanceOf[T], st))
              }

              println(classSymbol.owner)
            }
            failure

          case _ =>
            failure
        }

      case failure @ DeserializationFailure(_) =>
        failure
    }

}
