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

  val typeLabel = "object"

  val mirror: universe.Mirror = runtimeMirror(getClass.getClassLoader) // whatever mirror you use to obtain the `Type`

  def validate(value: T, fieldName: Option[String] = None)(implicit tt: TypeTag[T]): Boolean Or Every[SJError] = {
    val errField = fieldName.map(fn => s"(field $fn)--").getOrElse("")
    val (patternNames, patternCheck) = patternChecks(value)
    val (propNames, propertyCheck) = propertyChecks(value)
    val simpleSingle = withGood(
      check(
        maxProperties,
        (m: Int) => {
          context.typeAdapterOf[T] match {
            case ccta: CaseClassTypeAdapter[T] =>
              if (ccta.fieldMembersByName.size <= m) Good(true)
              else Bad(One(new SchemaValidationError(s"${errField}Given object has more than the maximum allowed properties ($m)")))
            case pcta: PlainClassTypeAdapter[T] =>
              if (pcta.fieldMembersByName.size <= m) Good(true)
              else Bad(One(new SchemaValidationError(s"${errField}Given object has more than the maximum allowed properties ($m)")))
            case x =>
              Bad(One(new SchemaValidationError(s"${errField}Given object is an unsupported kind (e.g. Trait)")))
          }
        }
      ),
      check(
        minProperties,
        (m: Int) => {
          context.typeAdapterOf[T] match {
            case ccta: CaseClassTypeAdapter[T] =>
              if (ccta.fieldMembersByName.size >= m) Good(true)
              else Bad(One(new SchemaValidationError(s"${errField}Given object has less than the minimum allowed properties ($m)")))
            case pcta: PlainClassTypeAdapter[T] =>
              if (pcta.fieldMembersByName.size >= m) Good(true)
              else Bad(One(new SchemaValidationError(s"${errField}Given object has less than the minimum allowed properties ($m)")))
            case x =>
              Bad(One(new SchemaValidationError(s"${errField}Given object is an unsupported kind (e.g. Trait)")))
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
              else Bad(One(new SchemaValidationError(s"${errField}Given object is missing required fields: " + diffFields.mkString(","))))
            case pcta: PlainClassTypeAdapter[T] =>
              val set = getFieldsPresent(value, pcta)
              val diffFields = m.toSet.diff(set).toList
              if (diffFields.isEmpty) Good(true)
              else Bad(One(new SchemaValidationError(s"${errField}Given object is missing required fields: " + diffFields.mkString(","))))
            case x =>
              Bad(One(new SchemaValidationError(s"${errField}Given object is an unsupported kind (e.g. Trait)")))
          }
        }
      )
    ) { _ & _ & _ }

    withGood(
      propertyCheck,
      patternCheck,
      additionalChecks(value, patternNames ++ propNames),
      propNameChecks(value),
      dependencyChecks(value),
      simpleSingle) {
        _ & _ & _ & _ & _ & _
      }
  }

  private def valueGood(ta: ClassHelper.ClassLikeTypeAdapter[T], fieldName: String, objValue: T, s: Schema[_])(
      implicit
      tt: TypeTag[T]): Boolean Or Every[SJError] = {
    val fieldMember = ta.fieldMembersByName(fieldName)
    val realValue = fieldMember.valueIn(objValue)
    val castValue = realValue.asInstanceOf[fieldMember.Value]
    val realType: Type = runtimeMirror(realValue.getClass.getClassLoader).classSymbol(realValue.getClass).toType
    try {
      if (realType == typeOf[Integer]) {
        val lg: Long = castValue.asInstanceOf[Integer].toLong
        s.asInstanceOf[Schema[Long]].validate(lg, Some(fieldName))
      } else if (realType == typeOf[Int]) {
        val lg: Long = castValue.asInstanceOf[Int].toLong
        s.asInstanceOf[Schema[Long]].validate(lg, Some(fieldName))
      } else
        s.asInstanceOf[Schema[Any]].validate(castValue, Some(fieldName))(backward(realType))
    } catch {
      case _: ClassCastException => Bad(One(new SchemaValidationError(s"(field $fieldName)--Wrong type.  Expected ${s.typeLabel}")))
      case _                     => Bad(One(new SchemaValidationError(s"(field $fieldName)--Some other error occurred")))
    }
  }

  private def dependencyChecks(value: T)(implicit tt: TypeTag[T]): Boolean Or Every[SJError] =
    dependencies
      .map(dep =>
        context.typeAdapterOf[T] match {
          case ta: ClassHelper.ClassLikeTypeAdapter[T] =>
            dep
              .map {
                case (key, needs) =>
                  val keyRes = fieldIsPresent(ta, key, value)
                  if (keyRes.isGood) {
                    needs.map(need => fieldIsPresent(ta, need, value)).toList.combined match {
                      case Good(_) => Good(true)
                      case Bad(b)  => Bad(b)
                    }
                  } else Good(true) // Don't care about dependencies on a field that's not there!
              }
              .toList
              .combined match {
                case Good(_) => Good(true)
                case Bad(b)  => Bad(b)
              }
          case _ =>
            Bad(One(new SchemaValidationError("Given object is an unsupported kind (e.g. Trait)")))
        })
      .getOrElse(Good(true))

  private def fieldIsPresent(ta: ClassHelper.ClassLikeTypeAdapter[T], fieldName: String, value: T)(implicit tt: TypeTag[T]): Boolean Or One[SJError] =
    ta.fieldMembersByName
      .get(fieldName)
      .map {
        _.valueIn(value) match {
          case Some(_) => Good(true)
          case None    => Bad(One(new SchemaValidationError(s"Dependency field $fieldName is not set in given object.")))
          case _       => Good(true)
        }
      }
      .getOrElse(Bad(One(new SchemaValidationError(s"Dependency field $fieldName is not defined in given object."))))

  private def propNameChecks(value: T)(implicit tt: TypeTag[T]): Boolean Or Every[SJError] =
    propertyNames
      .map(pname =>
        context.typeAdapterOf[T] match {
          case ta: ClassHelper.ClassLikeTypeAdapter[T] =>
            ta.fieldMembersByName.keySet.map(key => pname.validate(key, Some(key))).toList.combined match {
              case Good(_) => Good(true)
              case Bad(b)  => Bad(b)
            }
          case _ =>
            Bad(One(new SchemaValidationError("Given object is an unsupported kind (e.g. Trait)")))
        })
      .getOrElse(Good(true))

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
              diffNames.map(name => valueGood(ta, name, value, s)).combined match {
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
                    valueGood(ccta, f, value, s)
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
                    valueGood(ccta, key, value, s)
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
