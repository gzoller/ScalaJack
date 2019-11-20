package co.blocke.series60
package model

/**
 * Result returned from Reader.readObjectFields.  This is a status object that lets the
 * caller know whether all expected fields were found, which were missing, and and captured
 * content ("extra" fields found in input but not asked for by parsing/read).
 *
 * @param allThere
 * @param objectArgs
 * @param fieldSet
 * @param captured "Extra" fields found in input but not asked for by parsing/read).  Will always be None if object is not mixed with SJCapture.
 */
case class ObjectFieldsRead(
    allThere:   Boolean, // True if all fields found (Ok to create object)
    objectArgs: Array[Any], // Ordered arguments matching object constructor
    fieldSet:   Array[Boolean], // Bit map of fields set in case any missing
    captured:   Map[String, Any] // Captured fields if SJCapture
)
