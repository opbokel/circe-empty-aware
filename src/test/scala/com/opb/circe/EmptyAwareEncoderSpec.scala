package com.opb.circe

import io.circe.Json
import io.circe.generic.semiauto._
import org.scalatest.{Matchers, WordSpecLike}

class EmptyAwareEncoderSpec extends WordSpecLike with Matchers {

  "The EmptyAwareEncoder" should {
    "override collection encoders and return null when empty" in {
      case class ColWrapper(map: Map[Int, Int] = Map.empty, seq: Seq[Int] = Nil,
        vector: Vector[Int] = Vector.empty, set: Set[Int] = Set.empty,
        list: List[Int] = Nil)

      implicit val encoder = deriveEncoder[ColWrapper]

      val emptyColWrapper = ColWrapper()

      val filledColWrapper = ColWrapper(Map(1 → 2, 3 → 4), Seq(1, 2), Vector(3, 4), Set(7, 8), List(5))

      val json1 = encoder(emptyColWrapper)

      json1.hcursor.downField("map").focus.get.asObject.get.toMap shouldBe Map.empty[String, Json]
      json1.hcursor.downField("seq").focus.get.asArray.get shouldBe Vector.empty
      json1.hcursor.downField("vector").focus.get.asArray.get shouldBe Vector.empty
      json1.hcursor.downField("set").focus.get.asArray.get shouldBe Vector.empty
      json1.hcursor.downField("list").focus.get.asArray.get shouldBe Vector.empty

      val json2 = encoder(filledColWrapper)

      json2.hcursor.downField("map").focus.get.asObject.get.toMap shouldBe Map("1" → Json.fromInt(2), "3" → Json.fromInt(4))
      json2.hcursor.downField("seq").focus.get.asArray.get shouldBe Vector(Json.fromInt(1), Json.fromInt(2))
      json2.hcursor.downField("vector").focus.get.asArray.get shouldBe Vector(Json.fromInt(3), Json.fromInt(4))
      json2.hcursor.downField("set").focus.get.asArray.get shouldBe Vector(Json.fromInt(7), Json.fromInt(8))
      json2.hcursor.downField("list").focus.get.asArray.get shouldBe Vector(Json.fromInt(5))

      val (json3, json4) = {
        import EmptyAwareEncoder.CollectionsOverride._
        val encoder = deriveEncoder[ColWrapper]
        (encoder(emptyColWrapper), encoder(filledColWrapper))
      }

      json3.hcursor.downField("map").focus.get shouldBe Json.Null
      json3.hcursor.downField("seq").focus.get shouldBe Json.Null
      json3.hcursor.downField("vector").focus.get shouldBe Json.Null
      json3.hcursor.downField("set").focus.get shouldBe Json.Null
      json3.hcursor.downField("list").focus.get shouldBe Json.Null

      json4.hcursor.downField("map").focus.get.asObject.get.toMap shouldBe Map("1" → Json.fromInt(2), "3" → Json.fromInt(4))
      json4.hcursor.downField("seq").focus.get.asArray.get shouldBe Vector(Json.fromInt(1), Json.fromInt(2))
      json4.hcursor.downField("vector").focus.get.asArray.get shouldBe Vector(Json.fromInt(3), Json.fromInt(4))
      json4.hcursor.downField("set").focus.get.asArray.get shouldBe Vector(Json.fromInt(7), Json.fromInt(8))
      json4.hcursor.downField("list").focus.get.asArray.get shouldBe Vector(Json.fromInt(5))

    }

    "transform objects with only empty fields is null" in {
      import EmptyAwareEncoder._

      case class OptClass(opt: Option[Int] = None, list: List[Int] = Nil)

      val encoder = deriveEncoder[OptClass].asEmptyAware()

      val json1 = encoder(OptClass(Some(3), List(1, 2)))

      json1.hcursor.downField("opt").focus.get.asNumber.get.toInt.get shouldBe 3
      json1.hcursor.downField("list").focus.get.asArray.get shouldBe Vector(Json.fromInt(1), Json.fromInt(2))

      val json2 = encoder(OptClass())
      json2.isNull shouldBe true

    }

    "check for empty objects recursively" in {
      import EmptyAwareEncoder._

      case class OptDeepClass0(opt: Option[OptDeepClass1] = None, value: Option[Int] = None)
      case class OptDeepClass1(opt: Option[OptDeepClass2] = None, value: Option[Int] = None)
      case class OptDeepClass2(opt: Option[OptDeepClass3] = None, value: Option[Int] = None)
      case class OptDeepClass3(value: Option[Int] = None)

      val deep3 = OptDeepClass0(Some(OptDeepClass1(Some(OptDeepClass2(None, Some(3))))))

      implicit val encoder3 = deriveEncoder[OptDeepClass3]
      implicit val encoder2 = deriveEncoder[OptDeepClass2]
      implicit val encoder1 = deriveEncoder[OptDeepClass1]
      implicit val encoder0 = deriveEncoder[OptDeepClass0].asEmptyAware(5)

      val deep3Js = encoder0(deep3)

      deep3Js.hcursor.downField("opt").downField("opt").downField("value").focus.get.asNumber.get.toInt.get shouldBe 3

      val deep4Empty = OptDeepClass0(Some(OptDeepClass1(Some(OptDeepClass2(Some(OptDeepClass3()))))))

      val deep4EmptyJs = encoder0(deep4Empty)

      deep4EmptyJs.isNull shouldBe true

      val deep4ValueInMiddle = OptDeepClass0(Some(OptDeepClass1(Some(OptDeepClass2(Some(OptDeepClass3()))), Some(5))))

      val deep4ValueInMiddleJs = encoder0(deep4ValueInMiddle)

      deep4ValueInMiddleJs.hcursor.downField("opt").downField("value").focus.get.asNumber.get.toInt.get shouldBe 5
    }

  }
}
