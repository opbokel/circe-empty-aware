package com.opb.circe

import io.circe._
import scala.collection.immutable.{Set, Map => ImmutableMap}

object EmptyAwareEncoder {

  def overrideEncoder[A](implicit encoder: Encoder[A]): Encoder[A] = {
    encoder.asEmptyAware()
  }

  object CollectionsOverride {
    implicit final def encodeList[A](implicit encodeA: Encoder[A]): Encoder[List[A]] = overrideEncoder(Encoder.encodeList)
    implicit final def encodeSeq[A](implicit encodeA: Encoder[A]): Encoder[Seq[A]] = overrideEncoder(Encoder.encodeSeq)
    implicit final def encodeVector[A](implicit encodeA: Encoder[A]): Encoder[Vector[A]] = overrideEncoder(Encoder.encodeVector)
    implicit final def encodeSet[A](implicit encodeA: Encoder[A]): Encoder[Set[A]] = overrideEncoder(Encoder.encodeSet)
    implicit final def encodeMap[K, V](implicit encodeK: KeyEncoder[K], encodeV: Encoder[V]): Encoder[ImmutableMap[K, V]] = overrideEncoder(Encoder.encodeMap)
    //Type error somehow, have to investigate
    //implicit final def encodeMapLike[K, V, M[K, V] <: Map[K, V]](implicit encodeK: KeyEncoder[K], encodeV: Encoder[V], ev: M[K, V] => Iterable[(K, V)]): ObjectEncoder[M[K, V]] = overrideEncoder(Encoder.encodeMapLike)
  }

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

  def emptyValueAsNull(value: Json, maxNestedLevel: Int = 0): Json = {
    if (isEmpty(value, maxNestedLevel)) Json.Null else value
  }

  implicit class ToEmptyAwareEncoder[A](emptyUnawareEncoder: Encoder[A]) {

    def asEmptyAware(maxNestedLevel: Int = 0): Encoder[A] = {
      (a: A) ⇒ emptyValueAsNull(emptyUnawareEncoder(a), maxNestedLevel)
    }
  }

}
