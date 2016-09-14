package co.blocke.scalajack

import json.TypeAdapterFactory

case class CustomReadRender(
  read:   PartialFunction[(KindMarker, Any), Any],
  render: PartialFunction[(KindMarker, Any), Any]
)

case class VisitorContext(
    isCanonical:     Boolean                           = true, // allow non-string keys in Maps--not part of JSON spec
    isValidating:    Boolean                           = false,
    estFieldsInObj:  Int                               = 1024,
    customHandlers:  Map[String, CustomReadRender]     = Map.empty[String, CustomReadRender],
    customAdapters:  List[TypeAdapterFactory]          = List.empty[TypeAdapterFactory],
    parseOrElse:     Map[String, String]               = Map.empty[String, String],
    hintMap:         Map[String, String]               = Map("default" -> "_hint"), // per-class type hints (for nested classes)
    hintValueRead:   Map[String, (String) => String]   = Map.empty[String, (String) => String], // per-class type hint value -> class name
    hintValueRender: Map[String, (String) => String]   = Map.empty[String, (String) => String] // per-class type class name -> hint value
) {
  def withDefaultHint(h: String) = this.copy(hintMap = this.hintMap.+(("default", h)))
  def withAdapter(ta: TypeAdapterFactory) = this.copy(customAdapters = customAdapters :+ ta)
}