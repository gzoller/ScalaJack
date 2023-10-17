package co.blocke.scalajack
package typeadapter

import model._
import collection._

import co.blocke.scala_reflection._
import co.blocke.scala_reflection.impl.CollectionRType
import co.blocke.scala_reflection.info._


object CollectionTypeAdapterFactory extends TypeAdapterFactory:
  def matches(concrete: RType): Boolean = 
    concrete match {
      case _: CollectionRType => true
      case _ => false
    }

  inline def isOptionalTA(ta: TypeAdapter[_]) = ta.isInstanceOf[OptionTypeAdapter[_]] || ta.isInstanceOf[JavaOptionalTypeAdapter[_]] 

  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[_] =
    concrete match {
      case c: SeqLikeInfo => 
        val elementInfo = c.elementType
        val companionClass = Class.forName(c.infoClass.getName+"$")
        val companionInstance = companionClass.getField("MODULE$").get(companionClass)
        val builderMethod = companionClass.getMethod("newBuilder")
        SeqLikeTypeAdapter(concrete, elementInfo.isInstanceOf[OptionInfo], taCache.typeAdapterOf(elementInfo), companionInstance, builderMethod)
        
      case c: MapLikeInfo =>
        val companionClass = Class.forName(c.infoClass.getName+"$")
        val companionInstance = companionClass.getField("MODULE$").get(companionClass)
        val builderMethod = companionClass.getMethod("newBuilder")

        val jackFlavor = taCache.jackFlavor
        val keyTypeAdapter = taCache.typeAdapterOf(c.elementType)
        // Wrap Map keys in a StringWrapTypeAdapter?
        val finalKeyTypeAdapter = keyTypeAdapter match {
          case k if k.isStringish => k  // ready-to-eat
          case k if k.maybeStringish => jackFlavor.maybeStringWrapTypeAdapterFactory(k)
          case k => jackFlavor.stringWrapTypeAdapterFactory(k) // wrap map keys in quotes
        }
        val valueTypeAdapter = taCache.typeAdapterOf(c.elementType2) match {
          case ta: OptionTypeAdapter[_] => ta.convertNullToNone()
          case ta: JavaOptionalTypeAdapter[_] => ta.convertNullToNone()
          case ta => ta
        }

        // Note: We include Any here because Any *could* be an Option, so we must include it as a possibility
        val keyIsOptionalOrAny =
          isOptionalTA(keyTypeAdapter) ||
            (keyTypeAdapter.isInstanceOf[StringWrapTypeAdapter[_]] && isOptionalTA(keyTypeAdapter
              .asInstanceOf[StringWrapTypeAdapter[_]]
              .wrappedTypeAdapter)) ||
              keyTypeAdapter.isInstanceOf[AnyTypeAdapter]
              // keyTypeAdapter == taCache.jackFlavor.anyMapKeyTypeAdapter

        val valueIsOptionalOrAny = isOptionalTA(valueTypeAdapter) || valueTypeAdapter.isInstanceOf[AnyTypeAdapter]

        MapLikeTypeAdapter(
          concrete, 
          keyIsOptionalOrAny,
          valueIsOptionalOrAny,
          finalKeyTypeAdapter, 
          valueTypeAdapter,
          companionInstance, 
          builderMethod)

       
      case c: JavaListInfo =>
        val elementInfo = c.elementType
        // For List-like Java collections, use a ListBuilder then convert later to the Java collection
        val companionClass = Class.forName("scala.collection.immutable.List$")
        val companionInstance = companionClass.getField("MODULE$").get(companionClass)
        val builderMethod = companionClass.getMethod("newBuilder")
        val javaCollectionConstructor = c.infoClass.getConstructor(Class.forName("java.util.Collection"))
        val toArrayMethod = c.infoClass.getMethod("toArray")
        val elementTA = taCache.typeAdapterOf(elementInfo)

        JavaSeqLikeTypeAdapter(
          concrete,
          isOptionalTA(elementTA), 
          elementTA,
          companionInstance,
          builderMethod,
          javaCollectionConstructor,
          toArrayMethod
        )

      case c: JavaSetInfo =>
        val elementInfo = c.elementType
        // For List-like Java collections, use a ListBuilder then convert later to the Java collection
        val companionClass = Class.forName("scala.collection.immutable.List$")
        val companionInstance = companionClass.getField("MODULE$").get(companionClass)
        val builderMethod = companionClass.getMethod("newBuilder")
        val javaCollectionConstructor = c.infoClass.getConstructor(Class.forName("java.util.Collection"))
        val toArrayMethod = c.infoClass.getMethod("toArray")
        val elementTA = taCache.typeAdapterOf(elementInfo)
        
        JavaSeqLikeTypeAdapter(
          concrete,
          isOptionalTA(elementTA),
          elementTA,
          companionInstance,
          builderMethod,
          javaCollectionConstructor,
          toArrayMethod
        )

      case c: JavaQueueInfo =>
        val elementInfo = c.elementType
        // For List-like Java collections, use a ListBuilder then convert later to the Java collection
        val companionClass = Class.forName("scala.collection.immutable.List$")
        val companionInstance = companionClass.getField("MODULE$").get(companionClass)
        val builderMethod = companionClass.getMethod("newBuilder")
        val javaCollectionConstructor = c.infoClass.getConstructor(Class.forName("java.util.Collection"))
        val toArrayMethod = c.infoClass.getMethod("toArray")
        val elementTA = taCache.typeAdapterOf(elementInfo)
        
        JavaSeqLikeTypeAdapter(
          concrete,
          isOptionalTA(elementTA),
          elementTA,
          companionInstance,
          builderMethod,
          javaCollectionConstructor,
          toArrayMethod
        )

      case c: JavaStackInfo =>
        val elementInfo = c.elementType
        // For List-like Java collections, use a ListBuilder then convert later to the Java collection
        val companionClass = Class.forName("scala.collection.immutable.List$")
        val companionInstance = companionClass.getField("MODULE$").get(companionClass)
        val builderMethod = companionClass.getMethod("newBuilder")
        val javaCollectionConstructor = c.infoClass.getConstructors.head
        val toArrayMethod = c.infoClass.getMethod("toArray")
        val elementTA = taCache.typeAdapterOf(elementInfo)
        
        JavaStackTypeAdapter(
          concrete,
          isOptionalTA(elementTA), 
          taCache.typeAdapterOf(elementInfo),
          companionInstance,
          builderMethod,
          javaCollectionConstructor,
          toArrayMethod
        )
  
      case c: JavaMapInfo =>
        val companionClass = Class.forName("scala.collection.immutable.Map$")
        val companionInstance = companionClass.getField("MODULE$").get(companionClass)
        val builderMethod = companionClass.getMethod("newBuilder")
        val javaMapConstructor = c.infoClass.getConstructor(Class.forName("java.util.Map"))

        val jackFlavor = taCache.jackFlavor
        val keyTypeAdapter = taCache.typeAdapterOf(c.elementType)
        // Wrap Map keys in a StringWrapTypeAdapter?
        val finalKeyTypeAdapter = keyTypeAdapter match {
          case k if k.isStringish => k  // ready-to-eat
          case k if k.maybeStringish => jackFlavor.maybeStringWrapTypeAdapterFactory(k)
          case k => jackFlavor.stringWrapTypeAdapterFactory(k) // wrap map keys in quotes
        }
        val valueTypeAdapter = taCache.typeAdapterOf(c.elementType2) match {
          case ta: OptionTypeAdapter[_] => ta.convertNullToNone()
          case ta: JavaOptionalTypeAdapter[_] => ta.convertNullToNone()
          case ta => ta
        }

        // Note: We include Any here because Any *could* be an Option, so we must include it as a possibility
        val keyIsOptionalOrAny =
          isOptionalTA(keyTypeAdapter) ||
            (keyTypeAdapter.isInstanceOf[StringWrapTypeAdapter[_]] && isOptionalTA(keyTypeAdapter
              .asInstanceOf[StringWrapTypeAdapter[_]]
              .wrappedTypeAdapter)) ||
              keyTypeAdapter.isInstanceOf[AnyTypeAdapter]
              // keyTypeAdapter == taCache.jackFlavor.anyMapKeyTypeAdapter

        val valueIsOptionalOrAny = isOptionalTA(valueTypeAdapter) || valueTypeAdapter.isInstanceOf[AnyTypeAdapter]

        JavaMapLikeTypeAdapter(
          concrete, 
          keyIsOptionalOrAny,
          valueIsOptionalOrAny,
          finalKeyTypeAdapter, 
          valueTypeAdapter,
          companionInstance, 
          builderMethod,
          javaMapConstructor)

    }
