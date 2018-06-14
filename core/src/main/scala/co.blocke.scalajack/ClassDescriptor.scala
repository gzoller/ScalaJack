package co.blocke.scalajack

object ClassDescriptor {

  trait Member[Owner] {

    def nameInScala: String

    def nameInJson: String

  }

  trait TypeMember[Owner] extends Member[Owner]

  trait FieldMember[Owner] extends Member[Owner] {

    type Value

    implicit def valueTypeTag: TypeTag[Value]

    def valueIn(tagged: TypeTagged[Owner]): TypeTagged[Value]

    def deserializer: Deserializer[Value]

    def serializer: Serializer[Value]

  }

}
