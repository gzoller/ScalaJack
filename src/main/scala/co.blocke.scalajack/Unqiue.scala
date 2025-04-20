package co.blocke.scalajack

import co.blocke.scala_reflection.RTypeRef
import co.blocke.scala_reflection.reflect.rtypeRefs.*


case class Unique( fingerprints: Map[String, List[ScalaClassRef[?]]], optionalFields: Set[String] ):
  // If your hash (key) is not in the map--it can't be uniquely identified (ie needs a type hint)
  lazy val simpleUniqueHash: Map[String, String] =
    fingerprints.collect {
      case (hash, List(singleClass: ScalaClassRef[?])) => hash -> singleClass.name
    }.toMap
  def fingerprintOf(in: List[String]): String = Unique.hashOf( (in.toSet -- optionalFields).toList )
  def needsTypeHint(in: List[String]): Boolean = !simpleUniqueHash.contains(fingerprintOf(in)) && !simpleUniqueHash.contains("")

  def fingerprintByClass: Map[String, Option[String]] = {
    val classToHashes: Map[String, List[String]] =
      fingerprints.toList.flatMap { case (hash, classRefs) =>
        classRefs.map(c => c.name -> hash)
      }.groupMap(_._1)(_._2)

    classToHashes.view.mapValues {
      case List(singleHash) => Some(singleHash)
      case _                => None
    }.toMap
  }
  override def toString: String =
    "Optional: "+optionalFields.mkString(",")+"\n" +
    "Fingerprints: "+fingerprints.map{case(k,v) => s"$k -> ${v.map(_.name)}"}.mkString(",")


object Unique:
  // Given a TraitRef, analyze all child classes (and any descendant traits!) to extract field "fingerprints"
  // that uniquely identify classes (and those that cannot be so uniquely identified!).
  def findUniqueWithExcluded(inTrait: Sealable): Unique = {
    // Helper to recursively extract all ClassSpec instances from any ClassLike structure
    def collectSpecs(nodes: List[RTypeRef[?]]): List[ScalaClassRef[?]] = {
      nodes.flatMap {
        case cs: ScalaClassRef[?] => List(cs)
        case tr: TraitRef[?] => collectSpecs( tr.sealedChildren )
        case _ => throw new Exception("Can never happen--Trait has a non-trait, non-class implementation!")
      }
    }

    val allImplementingClasses: List[ScalaClassRef[?]] = collectSpecs(inTrait.sealedChildren)

    // Step 1: Collect all optional field names
    val optionalFields: Set[String] =
      allImplementingClasses.flatMap(_.fields.filter(f => isOptional(f)).map(_.name)).toSet

    // Step 2: For each class, filter out optional fields and hash the remaining
    val hashed: List[(String, ScalaClassRef[?])] = allImplementingClasses.map { cls =>
      val keptFieldNames = cls.fields.filterNot(f => optionalFields.contains(f.name)).map(_.name)
      val hash = hashOf(keptFieldNames)
      hash -> cls
    }

    // Step 3: Group by hash
    Unique(hashed.groupMap(_._1)(_._2), optionalFields)
  }

  inline def hashOf(names: List[String]): String = names.sorted.mkString("|")

  inline private def isOptional( fr: FieldInfoRef ): Boolean =
    fr.fieldRef match {
      case _: OptionRef[?] => true
      case _ => false
    }