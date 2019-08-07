package co.blocke.scalajack
package schema

import org.scalactic._
import Accumulation._
import model._
import typeadapter.classes._
import scala.reflect.runtime.universe._
import scala.reflect.api

case class ObjectSchema[T](
    maxProperties: Option[Int],
    minProperties: Option[Int],
    required: Option[Array[String]],
    properties: Option[Map[String, Schema[_]]], // Map[fieldName, Schema]
    patternProperties: Option[Map[String, Schema[_]]], // Map[regex, Schema]
    additionalProperties: Option[Either[Boolean, Schema[_]]],
    dependencies: Option[Map[String, Array[String]]], // "credit_card": ["billing_address"] (if credit_card field is present, billing_address is required
    propertyNames: Option[StringSchema]
)(implicit context: Context)
    extends Schema[T] {

  val mirror = runtimeMirror(getClass.getClassLoader) // whatever mirror you use to obtain the `Type`

  def validate(value: T)(implicit tt: TypeTag[T]): Boolean Or Every[SJError] = {
    val simpleSingle = withGood(
      check(
        maxProperties,
        (m: Int) => {
          context.typeAdapterOf[T] match {
            case ccta: CaseClassTypeAdapter[T] =>
              if (ccta.fieldMembersByName.size <= m) Good(true)
              else Bad(One(new SchemaValidationError(s"Given object has more than the maximum allowed properties ($m)")))
            case pcta: PlainClassTypeAdapter[T] =>
              if (pcta.fieldMembersByName.size <= m) Good(true)
              else Bad(One(new SchemaValidationError(s"Given object has more than the maximum allowed properties ($m)")))
            case x =>
              Bad(One(new SchemaValidationError("Given object is an unsupported kind (e.g. Trait)")))
          }
        }
      ),
      check(
        minProperties,
        (m: Int) => {
          context.typeAdapterOf[T] match {
            case ccta: CaseClassTypeAdapter[T] =>
              if (ccta.fieldMembersByName.size >= m) Good(true)
              else Bad(One(new SchemaValidationError(s"Given object has less than the minimum allowed properties ($m)")))
            case pcta: PlainClassTypeAdapter[T] =>
              if (pcta.fieldMembersByName.size >= m) Good(true)
              else Bad(One(new SchemaValidationError(s"Given object has less than the minimum allowed properties ($m)")))
            case x =>
              Bad(One(new SchemaValidationError("Given object is an unsupported kind (e.g. Trait)")))
          }
        }
      ),
      check(
        required,
        (m: Array[String]) => {
          context.typeAdapterOf[T] match {
            case ccta: CaseClassTypeAdapter[T] =>
              val set        = getFieldsPresent(value, ccta)
              val diffFields = m.toSet.diff(set).toList
              if (diffFields.size == 0) Good(true)
              else Bad(One(new SchemaValidationError("Given object is missing required fields: " + diffFields.mkString(","))))
            case pcta: PlainClassTypeAdapter[T] =>
              val set        = getFieldsPresent(value, pcta)
              val diffFields = m.toSet.diff(set).toList
              if (diffFields.size == 0) Good(true)
              else Bad(One(new SchemaValidationError("Given object is missing required fields: " + diffFields.mkString(","))))
            case x =>
              Bad(One(new SchemaValidationError("Given object is an unsupported kind (e.g. Trait)")))
          }
        }
      )
    ) { _ & _ & _ }

    withGood(propertyChecks(value), patternChecks(value), simpleSingle) { _ & _ & _ }
  }

  private def propertyChecks(value: T)(implicit tt: TypeTag[T]): Boolean Or Every[SJError] =
    properties
      .map { m =>
        context.typeAdapterOf[T] match {
          case ccta: ClassHelper.ClassLikeTypeAdapter[T] =>
            val results: Or[List[Boolean], Every[SJError]] = m
              .map {
                case (f: String, s: Schema[_]) =>
                  if (ccta.fieldMembersByName.contains(f)) {
                    val fieldMember    = ccta.fieldMembersByName(f)
                    val realValue      = fieldMember.valueIn(value)
                    val castValue      = realValue.asInstanceOf[fieldMember.Value]
                    val realType: Type = runtimeMirror(realValue.getClass.getClassLoader()).classSymbol(realValue.getClass).toType
                    if (realType == typeOf[Integer]) {
                      val lg: Long = castValue.asInstanceOf[Integer].toLong
                      s.asInstanceOf[Schema[Long]].validate(lg)
                    } else if (realType == typeOf[Int]) {
                      val lg: Long = castValue.asInstanceOf[Int].toLong
                      s.asInstanceOf[Schema[Long]].validate(lg)
                    } else
                      s.asInstanceOf[Schema[Any]].validate(castValue)(backward(realType))
                  } else
                    Bad(One(new SchemaValidationError(s"Missing expected field: $f")))
              }
              .toList
              .combined
            results match {
              case Good(_) => Good(true)
              case Bad(b)  => Bad(b)
            }
        }
      }
      .getOrElse(Good(true))

  private def patternChecks(value: T)(implicit tt: TypeTag[T]): Boolean Or Every[SJError] =
    patternProperties
      .map { m =>
        context.typeAdapterOf[T] match {
          case ccta: ClassHelper.ClassLikeTypeAdapter[T] =>
            val results: Or[List[Boolean], Every[SJError]] = m
              .map {
                case (regex: String, s: Schema[_]) =>
                  val r           = regex.r
                  val matchedKeys = ccta.fieldMembersByName.keySet.filter(k => r.findFirstIn(k).isDefined)
                  matchedKeys.map { key =>
                    val fieldMember    = ccta.fieldMembersByName(key)
                    val realValue      = fieldMember.valueIn(value)
                    val castValue      = realValue.asInstanceOf[fieldMember.Value]
                    val realType: Type = runtimeMirror(realValue.getClass.getClassLoader()).classSymbol(realValue.getClass).toType
                    if (realType == typeOf[Integer]) {
                      val lg: Long = castValue.asInstanceOf[Integer].toLong
                      s.asInstanceOf[Schema[Long]].validate(lg)
                    } else if (realType == typeOf[Int]) {
                      val lg: Long = castValue.asInstanceOf[Int].toLong
                      s.asInstanceOf[Schema[Long]].validate(lg)
                    } else
                      s.asInstanceOf[Schema[Any]].validate(castValue)(backward(realType))
                  }
              }
              .toList
              .flatten
              .combined
            results match {
              case Good(_) => Good(true)
              case Bad(b)  => Bad(b)
            }
        }
      }
      .getOrElse(Good(true))

  // Collect any non-optional fields, or optional fields that have Some() value, in T instance.
  private def getFieldsPresent(inst: T, ccta: CaseClassTypeAdapter[T]): Set[String] =
    ccta.fieldMembersByName.collect {
      case (fname: String, member: ClassHelper.ClassFieldMember[T, Any])
          if (!member.isOptional || member
            .valueIn(inst)
            .asInstanceOf[Option[_]]
            .isDefined) =>
        fname
    }.toSet

  private def getFieldsPresent(inst: T, pcta: PlainClassTypeAdapter[T]): Set[String] =
    (pcta.fieldMembersByName ++ pcta.nonConstructorFields).collect {
      case (fname: String, member: ClassHelper.ClassFieldMember[T, Any])
          if (!member.isOptional || member
            .valueIn(inst)
            .asInstanceOf[Option[_]]
            .isDefined) =>
        fname
    }.toSet

  private def backward[V](tpe: Type): TypeTag[V] =
    TypeTag(
      mirror,
      new api.TypeCreator {
        def apply[U <: api.Universe with Singleton](m: api.Mirror[U]) =
          if (m eq mirror) tpe.asInstanceOf[U#Type]
          else throw new IllegalArgumentException(s"Type tag defined in $mirror cannot be migrated to other mirrors.")
      }
    )
}
