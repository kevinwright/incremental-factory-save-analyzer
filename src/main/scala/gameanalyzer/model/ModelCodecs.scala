package gameanalyzer.model

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*

import scala.collection.immutable.ListMap

object ModelCodecs {

  type SimpleSkillValuesType = ListMap[String, Double]
  type CompoundSkillValuesType = ListMap[String, Map[String, Int]]
  type SkillValuesType = SimpleSkillValuesType | CompoundSkillValuesType

  given simpleSkillValuesCodec: JsonValueCodec[SimpleSkillValuesType] =
    JsonCodecMaker.make

  given compoundSkillValuesCodec: JsonValueCodec[CompoundSkillValuesType] =
    JsonCodecMaker.make

  given skillValuesCodec: JsonValueCodec[SkillValuesType] =
    new JsonValueCodec[SkillValuesType] {
      def decodeValue(
          in: JsonReader,
          default: SkillValuesType
      ): SkillValuesType = {
        in.setMark()

        try {
          simpleSkillValuesCodec.decodeValue(in, ListMap.empty)
        } catch {
          case _: JsonReaderException =>
            in.rollbackToMark()
            compoundSkillValuesCodec.decodeValue(in, ListMap.empty)
        }
      }

      def encodeValue(x: SkillValuesType, out: JsonWriter): Unit = {
        x.values.head match {
          case _: Double =>
            simpleSkillValuesCodec.encodeValue(
              x.asInstanceOf[SimpleSkillValuesType],
              out
            )
          case _: SkillTreeNodeValues.Compound =>
            compoundSkillValuesCodec.encodeValue(
              x.asInstanceOf[CompoundSkillValuesType],
              out
            )
          case _ =>
            simpleSkillValuesCodec.encodeValue(
              ListMap.empty,
              out
            )
        }
      }

      def nullValue: SkillValuesType = ListMap.empty
    }

//  given skillValuesCodec(using
//      simpleCodec: JsonValueCodec[Map[String, Double]],
//      mapCodec: JsonValueCodec[Map[String, Map[String, Int]]]
//  ): JsonValueCodec[SkillTreeNodeValues] =
//    new JsonValueCodec[SkillTreeNodeValues] {
//      def decodeValue(
//          in: JsonReader,
//          default: SkillTreeNodeValues
//      ): SkillTreeNodeValues = {
//        val b = in.nextToken()
//        println(s"decoding, next token = ${b}")
//
//        if (b == '{') {
//          in.rollbackToken()
//          SkillTreeNodeValues.Compound(mapCodec.decodeValue(in, Map.empty))
//        } else {
//          in.rollbackToken()
//          SkillTreeNodeValues.Simple(simpleCodec.decodeValue(in, Map.empty))
//        }
//      }
//
//      def encodeValue(x: SkillTreeNodeValues, out: JsonWriter): Unit =
//        x match {
//          case s: SkillTreeNodeValues.Simple =>
//            simpleCodec.encodeValue(s.map, out)
//          case c: SkillTreeNodeValues.Compound =>
//            mapCodec.encodeValue(c.map, out)
//          case e: SkillTreeNodeValues.Empty.type =>
//            mapCodec.encodeValue(Map.empty, out)
//        }
//
//      def nullValue: SkillTreeNodeValues = SkillTreeNodeValues.Empty
//    }

//  given skillValuesCodec: JsonValueCodec[SkillTreeNodeValues] =
//    JsonCodecMaker.makeWithoutDiscriminator
  given gameStateRootCodec: JsonValueCodec[GameStateRoot] = JsonCodecMaker.make

}
