package com.opb.circe

import io.circe._
import scala.collection.immutable.{Set, Map => ImmutableMap}

/**
  * A Empty Aware Encoder is a encoder that represents a Json as Json.Null if
  * the Json value is considered empty. See [[com.opb.circe.EmptyAwareEncoder]].isEmpty
  * It is intended to be used with [[io.circe.Printer]] using dropNullValues = true
  */
object EmptyAwareEncoder {

  /**
    * Get a encoder, and returns it empty aware version.
    * @param encoder A encoder for type A
    * @tparam A The type of the encoder
    * @return The encoder for the same type, but empty Aware
    */
  def overrideEncoder[A](implicit encoder: Encoder[A]): Encoder[A] = {
    encoder.asEmptyAware()
  }

  /**
    * Import this to represent empty collections as Json.Null
    */
  object CollectionsOverride {
    implicit final def encodeList[A](implicit encodeA: Encoder[A]): Encoder[List[A]] = overrideEncoder(Encoder.encodeList)
    implicit final def encodeSeq[A](implicit encodeA: Encoder[A]): Encoder[Seq[A]] = overrideEncoder(Encoder.encodeSeq)
    implicit final def encodeVector[A](implicit encodeA: Encoder[A]): Encoder[Vector[A]] = overrideEncoder(Encoder.encodeVector)
    implicit final def encodeSet[A](implicit encodeA: Encoder[A]): Encoder[Set[A]] = overrideEncoder(Encoder.encodeSet)
    implicit final def encodeMap[K, V](implicit encodeK: KeyEncoder[K], encodeV: Encoder[V]): Encoder[ImmutableMap[K, V]] = overrideEncoder(Encoder.encodeMap)
    //TODO Type error somehow, have to investigate
    //implicit final def encodeMapLike[K, V, M[K, V] <: Map[K, V]](implicit encodeK: KeyEncoder[K], encodeV: Encoder[V], ev: M[K, V] => Iterable[(K, V)]): ObjectEncoder[M[K, V]] = overrideEncoder(Encoder.encodeMapLike)
  }

  /**
    * Returns true if the Json value is empty. A Json is considered empty if it
    * is a empty array, null, a empty object or if all the keys on the object are
    * associated with empty values
    * @param jsonResult The result produced from the encoder
    * @param maxNestedLevel If your object have keys is associated with other objects, it can go up n
    *                       levels to check if all your keys are empty (until the max level).
    *                       The default value is 0, because can lead to potential nˆ2 computation.
    *                       Use only if you cannot define the encoder of nested objects
    * @return true if the Json is empty, false otherwise. It also returns false on the maxNestedLevel is
    *         reached and still have object values to check
    */
  def isEmpty(jsonResult: Json, maxNestedLevel: Int = 0): Boolean = {
    def isValueEmpty(jsonValue: Json, level: Int): Boolean = {
      def isObjectEmpty(jsObj: JsonObject): Boolean = {
        jsObj.isEmpty || jsObj.values.forall(value ⇒ isValueEmpty(value, level + 1))
      }
      if (jsonValue.isObject && level <= maxNestedLevel) {
        isObjectEmpty(jsonValue.asObject.get)
      } else if (jsonValue.isArray) {
        jsonValue.asArray.get.isEmpty
      } else if (jsonValue.isNull) {
        true
      } else false
    }

    isValueEmpty(jsonResult, 0)
  }

  private def emptyValueAsNull(value: Json, maxNestedLevel: Int = 0): Json = {
    if (isEmpty(value, maxNestedLevel)) Json.Null else value
  }

  /**
    * Implicit class that adds the method asEmptyWare to any encoder. Making ease to transform it
    * in empty aware. Usage Example:
    * implicit val yourClassEncoder = deriveEncoder[YourClass].asEmptyAware()
    * @param emptyUnawareEncoder The encoder that is not empty aware yet
    * @tparam A The type of the encoder
    */
  implicit class ToEmptyAwareEncoder[A](emptyUnawareEncoder: Encoder[A]) {

    /**
      * Return a empty aware version with the same type of the original encoder
      * @param maxNestedLevel Check recursively for empty values, default is 0. See [[com.opb.circe.EmptyAwareEncoder]].isEmpty
      * @return Your new encoder, empty aware
      */
    def asEmptyAware(maxNestedLevel: Int = 0): Encoder[A] = {
      (a: A) ⇒ emptyValueAsNull(emptyUnawareEncoder(a), maxNestedLevel)
    }
  }

}
