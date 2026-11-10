package example.model

import java.util.UUID

opaque type EntityId[Tag] = UUID

object EntityId:
  def wrap[Tag](value: UUID): EntityId[Tag] = value

  extension [Tag](id: EntityId[Tag]) def value: UUID = id

sealed trait UserIdTag
type UserId = EntityId[UserIdTag]

object UserId:
  def apply(value: UUID): UserId = EntityId.wrap[UserIdTag](value)

enum AuditActor:
  case System(code: String)
  case User(userId: UserId)

case class OpaqueIdHolder(id: UserId, optional: Option[UserId], history: List[UserId], byName: Map[String, UserId])

case class OpaqueIdDefault(id: UserId = UserId(UUID.fromString("019a6f80-0000-7000-8000-000000000001")))

object PrimitiveOpaques:
  opaque type ByteValue = Byte
  opaque type ShortValue = Short
  opaque type IntValue = Int
  opaque type LongValue = Long
  opaque type FloatValue = Float
  opaque type DoubleValue = Double
  opaque type BooleanValue = Boolean
  opaque type CharValue = Char
  opaque type StringValue = String

  def byte(value: Byte): ByteValue = value
  def short(value: Short): ShortValue = value
  def int(value: Int): IntValue = value
  def long(value: Long): LongValue = value
  def float(value: Float): FloatValue = value
  def double(value: Double): DoubleValue = value
  def boolean(value: Boolean): BooleanValue = value
  def char(value: Char): CharValue = value
  def string(value: String): StringValue = value

opaque type TaggedValue[Tag, Value] = Value

object TaggedValue:
  def wrap[Tag, Value](value: Value): TaggedValue[Tag, Value] = value

opaque type CounterId[Tag] = Int

object CounterId:
  def wrap[Tag](value: Int): CounterId[Tag] = value

case class PrimitiveOpaqueFields(
    count: PrimitiveOpaques.IntValue,
    total: TaggedValue[UserIdTag, Long],
    enabled: PrimitiveOpaques.BooleanValue = PrimitiveOpaques.boolean(true)
)

enum PrimitiveOpaqueActor:
  case Count(count: PrimitiveOpaques.IntValue)
  case TaggedCount(taggedCount: CounterId[UserIdTag])
  case User(userId: UserId)
