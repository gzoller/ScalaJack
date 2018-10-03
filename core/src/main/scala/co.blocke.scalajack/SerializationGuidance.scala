package co.blocke.scalajack

case class SerializationGuidance(
    // If we're deserializing a Map key we want to re-parse contents of a string to see if its
    // a complex key, e.g. a serialized object or non-string primitive value.  We *don't* want that
    // re-parse behavior on the value side--we want these to be caught as errors.
    isMapKey: Boolean = false,

    // WARNING: Mutually exclusive with MapKeyGuidance, with a bias to MapKeyGuidance
    isMapValue: Boolean = false,

    // This is a looser interpretation of JSON where "true" (string) will be parsed as true, "1" as 1, etc.
    secondLookParsing: Boolean = false,

    // Some things, like Option, are handled differently if they're part of a sequence.  This flag is set
    // when dealing with Seq variants.
    inSeq: Boolean = false,

    // From ScalaJack class
    isCanonical: Boolean = false) {

  def withMapKey() = this.copy(isMapKey   = true, isMapValue = false, inSeq = false)
  def withMapValue() = this.copy(isMapKey   = false, isMapValue = true, inSeq = false)
  def withSeq() = this.copy(isMapKey   = false, isMapValue = false, inSeq = true)

  override def toString(): String = ":: isMapKey: " + this.isMapKey + "  isMapValue: " + this.isMapValue +
    "  secondLookParsing: " + this.secondLookParsing +
    "  inSeq: " + this.inSeq
}

