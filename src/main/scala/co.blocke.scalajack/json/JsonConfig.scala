package co.blocke.scalajack
package json

case class JsonConfig(
    noneAsNull: Boolean = false,
    tryFailureHandling: TryOption = TryOption.NO_WRITE,
    // --------------------------
    typeHintLabel: String = "_hint",
    typeHintLabelByTrait: Map[String, String] = Map.empty[String, String], // Trait name -> type hint label
    typeHintDefaultTransformer: String => String = (v: String) => v, // in case you want something different than class name (simple name re-mapping)
    typeHintTransformer: Map[String, Any => String] = Map.empty[String, Any => String], // if you want class-specific control (instance value => String)
    // --------------------------
    enumsAsIds: Char | List[String] = Nil // Char is '*' for all enums as ids, or a list of fully-qualified class names
)

enum TryOption:
  case AS_NULL, NO_WRITE, ERR_MSG_STRING
