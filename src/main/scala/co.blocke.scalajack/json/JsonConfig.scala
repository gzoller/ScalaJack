package co.blocke.scalajack
package json

case class JsonConfig(
    enumsAsIds: Char | List[String] = Nil  // Char is '*' for all enums as ids
)