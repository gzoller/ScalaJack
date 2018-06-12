package co.blocke.scalajack
package typeadapter

/**
 * This class is virtually identical to SimpleTypeAdapter, except... it uses == rather than =:= type compare
 * types in the factory to see if this is the TypeAdapter being sought by Context.
 *
 * This difference is because =:= matches children.  Consider:
 *    type Phone = String
 *    case class( name:String, phone:Phone )
 *
 * With =:= both name and phone (String and Phone) will match a PhoneTypeAdapter based on SimpleTypeAdapter.
 * This is actually what you want if you haven't overridden Phone with its own TypeAdapter... it should default
 * to the TypeAdapter of its base type.
 *
 * But... if you did provide an override PhoneTypeAdapter you want the matching to be strict, hence ==.  With
 * strict matching String != Phone.
 *
 * This means all TypeAdapter overrides for primitives should extend BasicTypeAdapter, not SimpleTypeAdapter.
 */
abstract class BasicTypeAdapter[V](implicit valueTypeTag: TypeTag[V]) extends TypeAdapterFactory with TypeAdapter[V] {

  private val valueType = valueTypeTag.tpe

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe == valueType) {
      // stricter type test than SimpleTypeAdapter:  == vs =:=
      this.asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
    }

}
