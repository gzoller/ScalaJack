package co.blocke.scalajack

object NormalGuidance extends DeserializationGuidance {
  override val isMapKey: Boolean = false
  override val secondLookParsing: Boolean = false
}

object SecondLookGuidance extends DeserializationGuidance {
  override val isMapKey: Boolean = false
  override val secondLookParsing: Boolean = true
}

object MapKeyGuidance extends DeserializationGuidance {
  override val isMapKey: Boolean = true
  override val secondLookParsing: Boolean = false
}

trait DeserializationGuidance {
  // If we're deserializing a Map key we want to re-parse contents of a string to see if its
  // a complex key, e.g. a serialized object or non-string primitive value.  We *don't* want that
  // re-parse behavior on the value side--we want these to be caught as errors.
  val isMapKey: Boolean

  // This is a looser interpretation of JSON where "true" (string) will be parsed as true, "1" as 1, etc.
  val secondLookParsing: Boolean

  def or(g: DeserializationGuidance): DeserializationGuidance = {
    val self = this

    new DeserializationGuidance {
      val isMapKey: Boolean = self.isMapKey || g.isMapKey
      val secondLookParsing: Boolean = self.secondLookParsing || g.secondLookParsing
    }
  }

  override def toString(): String = ":: isMapKey: " + this.isMapKey + "  secondLookParsing: " + this.secondLookParsing
}

