package co.blocke.scalajack

object NormalGuidance extends SerializationGuidance {
  override val isMapKey: Boolean = false
  override val secondLookParsing: Boolean = false
  override val inSeq: Boolean = false
}

object SecondLookGuidance extends SerializationGuidance {
  override val isMapKey: Boolean = false
  override val secondLookParsing: Boolean = true
  override val inSeq: Boolean = false
}

object MapKeyGuidance extends SerializationGuidance {
  override val isMapKey: Boolean = true
  override val secondLookParsing: Boolean = false
  override val inSeq: Boolean = false
}

object SeqGuidance extends SerializationGuidance {
  override val isMapKey: Boolean = false
  override val secondLookParsing: Boolean = false
  override val inSeq: Boolean = true
}

trait SerializationGuidance {
  // If we're deserializing a Map key we want to re-parse contents of a string to see if its
  // a complex key, e.g. a serialized object or non-string primitive value.  We *don't* want that
  // re-parse behavior on the value side--we want these to be caught as errors.
  val isMapKey: Boolean

  // This is a looser interpretation of JSON where "true" (string) will be parsed as true, "1" as 1, etc.
  val secondLookParsing: Boolean

  // Some things, like Option, are handled differently if they're part of a sequence.  This flag is set
  // when dealing with Seq variants.
  val inSeq: Boolean

  def or(g: SerializationGuidance): SerializationGuidance = {
    val self = this

    new SerializationGuidance {
      val isMapKey: Boolean = self.isMapKey || g.isMapKey
      val secondLookParsing: Boolean = self.secondLookParsing || g.secondLookParsing
      val inSeq: Boolean = self.inSeq || g.inSeq
    }
  }

  override def toString(): String = ":: isMapKey: " + this.isMapKey +
    "  secondLookParsing: " + this.secondLookParsing +
    "  inSeq: " + this.inSeq
}

