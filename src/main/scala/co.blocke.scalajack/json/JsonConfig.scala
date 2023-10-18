package co.blocke.scalajack
package json

case class JsonConfig(
    noneAsNull: Boolean = false,
    tryFailureHandling: TryOption = TryOption.NO_WRITE,
    enumsAsIds: Char | List[String] = Nil // Char is '*' for all enums as ids
)

enum TryOption:
  case AS_NULL, NO_WRITE, ERR_MSG_STRING
