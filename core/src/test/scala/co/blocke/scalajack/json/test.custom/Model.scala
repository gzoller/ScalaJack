package co.blocke.scalajack
package json.test.custom

object MyTypes {
  type Phone = String
}
import MyTypes._

class PhoneDeserializer()(implicit tt: TypeTag[Phone]) extends Deserializer[Phone] {

  private val nullTypeTagged: TypeTagged[Phone] = TypeTagged[Phone](null.asInstanceOf[Phone], tt.tpe)

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[Phone] =
    ast match {
      case AstNull() => DeserializationSuccess(nullTypeTagged)
      case AstString(s) =>
        val fixed: Phone = s.replaceAll("-", "")
        DeserializationSuccess(TypeTagged(fixed, tt.tpe))
    }
}

class PhoneSerializer()(implicit tt: TypeTag[Phone]) extends Serializer[Phone] {
  def serialize[AST, S](tagged: TypeTagged[Phone])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(value) =>
        val fixed = "%s-%s-%s".format(value.substring(0, 3), value.substring(3, 6), value.substring(6))
        SerializationSuccess(AstString(fixed))
    }
}

// Override just Phone
object PhoneAdapter extends TypeAdapter.===[Phone] {
  override val deserializer: Deserializer[Phone] = new PhoneDeserializer()
  override val serializer: Serializer[Phone] = new PhoneSerializer()
}

// Override Phone...and its parents (String)!
object OopsPhoneAdapter extends TypeAdapter.=:=[Phone] {
  override val deserializer: Deserializer[Phone] = new PhoneDeserializer()
  override val serializer: Serializer[Phone] = new PhoneSerializer()
}

case class Person(name: String, phone: Phone)

trait Address { val postalCode: String }
case class USAddress(street: String, city: String, state: String, postalCode: String) extends Address
case class CanadaAddress(street: String, city: String, province: String, postalCode: String) extends Address
case class DefaultAddress(postalCode: String) extends Address
trait Demographic { val address: Address }
case class USDemographic(age: Int, address: Address) extends Demographic
