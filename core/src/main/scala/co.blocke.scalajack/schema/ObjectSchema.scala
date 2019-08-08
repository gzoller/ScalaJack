package co.blocke.scalajack
package schema

import org.scalactic._
import Accumulation._
import model._
import typeadapter.classes._

import scala.reflect.runtime.universe._
import scala.reflect.api
import scala.reflect.runtime.universe

case class ObjectSchema[T](
    maxProperties:        Option[Int],
    minProperties:        Option[Int],
    required:             Option[Array[String]],
    properties:           Option[Map[String, Schema[_]]], // Map[fieldName, Schema]
    patternProperties:    Option[Map[String, Schema[_]]], // Map[regex, Schema]
    additionalProperties: Option[Either[Boolean, Schema[_]]],
    dependencies:         Option[Map[String, Array[String]]], // "credit_card": ["billing_address"] (if credit_card field is present, billing_address is required
    propertyNames:        Option[StringSchema]
)(implicit context: Context)
  extends Schema[T] {

  val mirror: universe.Mirror = runtimeMirror(getClass.getClassLoader) // whatever mirror you use to obtain the `Type`

  def validate(value: T, fieldName: Option[String] = None)(implicit tt: TypeTag[T]): Boolean Or Every[SJError] = {
    val (patternNames, patternCheck) = patternChecks(value)
    val (propNames, propertyCheck) = propertyChecks(value)
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
              val set = getFieldsPresent(value, ccta)
              val diffFields = m.toSet.diff(set).toList
              if (diffFields.isEmpty) Good(true)
              else Bad(One(new SchemaValidationError("Given object is missing required fields: " + diffFields.mkString(","))))
            case pcta: PlainClassTypeAdapter[T] =>
              val set = getFieldsPresent(value, pcta)
              val diffFields = m.toSet.diff(set).toList
              if (diffFields.isEmpty) Good(true)
              else Bad(One(new SchemaValidationError("Given object is missing required fields: " + diffFields.mkString(","))))
            case x =>
              Bad(One(new SchemaValidationError("Given object is an unsupported kind (e.g. Trait)")))
          }
        }
      )
    ) { _ & _ & _ }

    withGood(propertyCheck, patternCheck, additionalChecks(value, patternNames ++ propNames), simpleSingle) { _ & _ & _ & _ }
  }

  private def additionalChecks(value: T, names: List[String])(implicit tt: TypeTag[T]): Boolean Or Every[SJError] = {
    val set = (context.typeAdapterOf[T] match {
      case ccta: CaseClassTypeAdapter[T]  => getFieldsPresent(value, ccta)
      case pcta: PlainClassTypeAdapter[T] => getFieldsPresent(value, pcta)
      case _                              => names
    }).toList
    val diffNames = set.diff(names)
    additionalProperties
      .map {
        case Left(true) => Good(true) // do nothing, don't care
        case Left(false) =>
          if (diffNames.nonEmpty)
            Bad(One(new SchemaValidationError("Given object contains unspecified fields: " + diffNames.mkString(","))))
          else
            Good(true)
        case Right(s: Schema[_]) =>
          context.typeAdapterOf[T] match {
            case ta: ClassHelper.ClassLikeTypeAdapter[T] =>
              val res = diffNames.map { name =>
                val fieldMember = ta.fieldMembersByName(name)
                val realValue = fieldMember.valueIn(value)
                val castValue = realValue.asInstanceOf[fieldMember.Value]
                val realType: Type = runtimeMirror(realValue.getClass.getClassLoader).classSymbol(realValue.getClass).toType
                if (realType == typeOf[Integer]) {
                  val lg: Long = castValue.asInstanceOf[Integer].toLong
                  s.asInstanceOf[Schema[Long]].validate(lg, Some(name))
                } else if (realType == typeOf[Int]) {
                  val lg: Long = castValue.asInstanceOf[Int].toLong
                  s.asInstanceOf[Schema[Long]].validate(lg, Some(name))
                } else
                  s.asInstanceOf[Schema[Any]].validate(castValue, Some(name))(backward(realType))
              }.combined
              res match {
                case Good(_) => Good(true)
                case Bad(b)  => Bad(b)
              }
            case _ => Bad(One(new SchemaValidationError("Given object is an unsupported kind (e.g. Trait)")))
          }
      }
      .getOrElse(Good(true))
  }

  private def propertyChecks(value: T)(implicit tt: TypeTag[T]): (List[String], Boolean Or Every[SJError]) = {
    val names = collection.mutable.ListBuffer.empty[String]
    properties
      .map { m =>
        context.typeAdapterOf[T] match {
          case ccta: ClassHelper.ClassLikeTypeAdapter[T] =>
            val results: Or[List[Boolean], Every[SJError]] = m
              .map {
                case (f: String, s: Schema[_]) =>
                  if (ccta.fieldMembersByName.contains(f)) {
                    names += f
                    val fieldMember = ccta.fieldMembersByName(f)
                    val realValue = fieldMember.valueIn(value)
                    val castValue = realValue.asInstanceOf[fieldMember.Value]
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
              case Good(_) => (names.toList, Good(true))
              case Bad(b)  => (names.toList, Bad(b))
            }
        }
      }
      .getOrElse((List.empty[String], Good(true)))
  }

  private def patternChecks(value: T)(implicit tt: TypeTag[T]): (List[String], Boolean Or Every[SJError]) = {
    val names = collection.mutable.ListBuffer.empty[String]
    patternProperties
      .map { m =>
        context.typeAdapterOf[T] match {
          case ccta: ClassHelper.ClassLikeTypeAdapter[T] =>
            val results: Or[List[Boolean], Every[SJError]] = m
              .map {
                case (regex: String, s: Schema[_]) =>
                  val r = regex.r
                  val matchedKeys = ccta.fieldMembersByName.keySet.filter(k => r.findFirstIn(k).isDefined)
                  matchedKeys.map { key =>
                    names += key
                    val fieldMember = ccta.fieldMembersByName(key)
                    val realValue = fieldMember.valueIn(value)
                    val castValue = realValue.asInstanceOf[fieldMember.Value]
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
              case Good(_) => (names.toList, Good(true))
              case Bad(b)  => (names.toList, Bad(b))
            }
        }
      }
      .getOrElse((List.empty[String], Good(true)))
  }

  // Collect any non-optional fields, or optional fields that have Some() value, in T instance.
  private def getFieldsPresent(inst: T, ccta: CaseClassTypeAdapter[T]): Set[String] =
    ccta.fieldMembersByName.collect {
      case (fname: String, member: ClassHelper.ClassFieldMember[T, Any]) if (!member.isOptional || member
        .valueIn(inst)
        .asInstanceOf[Option[_]]
        .isDefined) =>
        fname
    }.toSet

  private def getFieldsPresent(inst: T, pcta: PlainClassTypeAdapter[T]): Set[String] =
    (pcta.fieldMembersByName ++ pcta.nonConstructorFields).collect {
      case (fname: String, member: ClassHelper.ClassFieldMember[T, Any]) if (!member.isOptional || member
        .valueIn(inst)
        .asInstanceOf[Option[_]]
        .isDefined) =>
        fname
    }.toSet

  private def backward[V](tpe: Type): TypeTag[V] =
    TypeTag(
      mirror,
      new api.TypeCreator {
        def apply[U <: api.Universe with Singleton](m: api.Mirror[U]): U#Type =
          if (m eq mirror) tpe.asInstanceOf[U#Type]
          else throw new IllegalArgumentException(s"Type tag defined in $mirror cannot be migrated to other mirrors.")
      }
    )
}
