package co.blocke.scalajack
package fields

/**
 * This field is a placeholder for the runtime type of a field.  We don't know what that type is during the Analyze phase,
 * so just stick this TypeField here and figure things out at runtime.
 */

case class TypeField( name:String, symbol:String ) extends Field 
