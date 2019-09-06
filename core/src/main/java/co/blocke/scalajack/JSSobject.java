package co.blocke.scalajack;

import java.lang.annotation.*;

@Inherited
@Target({ElementType.CONSTRUCTOR})
@Retention(RetentionPolicy.RUNTIME)
public @interface JSSobject {
    String description();

    /*
    configurables:
    description: Option[String] = None
    dependencies: Option[Map[String, Array[String]]], // "credit_card": ["billing_address"] (if credit_card field is present, billing_address is required

    auto-generated:
    required: Option[Array[String]],
    properties: Option[Map[String, Schema]], // Map[fieldName, Schema]
    additionalProperties: Option[Either[Boolean, Schema]],
    propertyNames: Option[Schema], // StringSchema

    irrelevant (for classes):
    maxProperties: Option[Int],
    minProperties: Option[Int],
    patternProperties: Option[Map[String, Schema]], // Map[regex, Schema]
     */
}
