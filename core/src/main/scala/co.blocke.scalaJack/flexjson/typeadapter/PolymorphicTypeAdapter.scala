package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.FlexJsonFlavor.MemberName
import co.blocke.scalajack.flexjson.{ Context, ForwardingWriter, Reader, TypeAdapter, TypeAdapterFactory, Writer }

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ ClassSymbol, Type }

case class PolymorphicTypeAdapterFactory(hintFieldName: String) extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context, superParamTypes: List[Type]): Option[TypeAdapter[_]] =
    if (classSymbol.isTrait) {
      Some(PolymorphicTypeAdapter(hintFieldName, context.typeAdapterOf[Type], context.typeAdapterOf[MemberName], context, tpe.typeArgs))
    } else {
      None
    }

}

class PolymorphicWriter(
    override val delegate: Writer,
    typeFieldName:         String,
    tpe:                   Type,
    typeTypeAdapter:       TypeAdapter[Type],
    memberNameTypeAdapter: TypeAdapter[MemberName]
) extends ForwardingWriter {

  var depth = 0

  override def beginObject(): Unit = {
    depth += 1
    super.beginObject()

    if (depth == 1) {
      memberNameTypeAdapter.write(typeFieldName, this)
      typeTypeAdapter.write(tpe, this)
    }
  }

  override def endObject(): Unit = {
    depth -= 1
    super.endObject()
  }

}

case class PolymorphicTypeAdapter[T](
    typeMemberName:        MemberName,
    typeTypeAdapter:       TypeAdapter[Type],
    memberNameTypeAdapter: TypeAdapter[MemberName],
    context:               Context,
    paramTypes:            List[Type]
) extends TypeAdapter[T] {

  override def read(reader: Reader): T = {
    val originalPosition = reader.position

    reader.beginObject()

    var optionalConcreteType: Option[Type] = None

    while (optionalConcreteType.isEmpty && reader.hasMoreMembers) {
      val memberName = memberNameTypeAdapter.read(reader)

      if (memberName == typeMemberName) {
        val concreteType = typeTypeAdapter.read(reader)
        optionalConcreteType = Some(concreteType)
      } else {
        reader.skipValue()
      }
    }

    val concreteType = optionalConcreteType.getOrElse(throw new Exception(s"""Could not find type field named "$typeMemberName" """))

    val concreteTypeAdapter = context.typeAdapter(concreteType)

    reader.position = originalPosition

    concreteTypeAdapter.read(reader).asInstanceOf[T]
  }

  override def write(value: T, writer: Writer): Unit = {
    // TODO figure out a better way to infer the type (perhaps infer the type arguments?)
    val valueType = currentMirror.classSymbol(value.getClass).info
    // println("VALUE : " + valueType)
    // val g = valueType.baseClasses.map { b =>
    //   (b.toString, b.typeSignature.typeParams)
    // }
    // println(g)
    // println("ARGS: " + paramTypes)

    /*
PREREQ:  Need an ordered list of resolved parameter types passed in when this TypeAdapter was created.
         This is reasonable because our (current) assumption is that top-level call always gives us
         the specific type, so we just need to navigate that information thru the stack.
         Note this should be an empty list if the trait takes no parameters.

         paramTypes: List[Type]   (List(String,Int) for our example)

STEPS:  (with the valueType)  -- navigate paramTypes thru context to CaseClassTypeAdapter
  1. Find the "with" of this trait.  It should contain an ordered list of its parameters, e.g.. Thing2[String,P]
  2. Map the given/known parameters passed in against this discovered list from valueType
  3. Create a Symbol -> Type map (resolvedTypes) of any non-concrete types, so Map(P -> Int) here
  4. Go back and substitute symbols in resolvedTypes in the class (replace P with Int)
  5. Now discover the class with context

Note that if valueType has unresolved Symbols then this will (rightfully) explode.  We have no way of knowing.
Perhaps a refinement would be to support an Any type for _ that would resolve primitive types and maybe
simple collections of primitive types.
*/

    val valueTypeAdapter = context.typeAdapter(valueType, paramTypes).asInstanceOf[TypeAdapter[T]]

    val polymorphicWriter = new PolymorphicWriter(writer, typeMemberName, valueType, typeTypeAdapter, memberNameTypeAdapter)
    valueTypeAdapter.write(value, polymorphicWriter)
  }

}
