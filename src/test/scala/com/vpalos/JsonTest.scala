package com.vpalos

import java.io.InputStreamReader
import java.net.URL

import org.scalatest._

/**
 * Unit tests for the io.qws.helpers.Json class.
 */
class JsonTest extends FlatSpec with Matchers {

  import com.vpalos.Json._

  val url = getClass.getResource("/com/vpalos/JsonTest.json")

  def sample = Json.parse(url)

  behavior of "Json.parse"

  /**
   * Parsing from various sources.
   */
  it should "parse local source (i.e. URL)" in {
    noException should be thrownBy {
      sample
    }
  }

  it should "parse String sources" in {
    noException should be thrownBy Json.parse("""{"a":1}""")
  }

  it should "parse String via attached `.toJson` method" in {
    noException should be thrownBy """{"a":1}""".toJson
  }

  it should "parse Reader and InputStream sources" in {
    noException should be thrownBy Json.parse(url.openStream())
    noException should be thrownBy Json.parse(new InputStreamReader(url.openStream()))
  }

  /**
   * Parsing validation tests.
   */
  it should "correctly fail to parse bad JSON" in {
    a [JsonParseException] should be thrownBy Json.parse("""{ "a": no }""")
    a [JsonParseException] should be thrownBy Json.parse("""{"a":"\u0001"}""")
    a [JsonParseException] should be thrownBy Json.parse("""{"a":"'""}""")
    a [JsonParseException] should be thrownBy Json.parse("")
    a [JsonParseException] should be thrownBy Json.parse("\n\n\r\n")
    a [JsonParseException] should be thrownBy Json.parse(" ")
    a [JsonParseException] should be thrownBy Json.parse("   ")
    a [JsonParseException] should be thrownBy Json.parse("[+1]")
    a [JsonParseException] should be thrownBy Json.parse("[1e]")
    a [JsonParseException] should be thrownBy Json.parse("[1e-]")
    a [JsonParseException] should be thrownBy Json.parse("[--1]")
    a [JsonParseException] should be thrownBy Json.parse("[1-+]")
    a [JsonParseException] should be thrownBy Json.parse("[0xaf]")
    a [JsonParseException] should be thrownBy Json.parse("[- 5]")
    a [JsonParseException] should be thrownBy Json.parse("['hello']")
    a [JsonParseException] should be thrownBy Json.parse("[1, 2, 3,]")
    a [JsonParseException] should be thrownBy Json.parse("{key: 1}")
    a [JsonParseException] should be thrownBy Json.parse("{false: 1}")
    a [JsonParseException] should be thrownBy Json.parse("{true: 1}")
    a [JsonParseException] should be thrownBy Json.parse("{null: 1}")
    a [JsonParseException] should be thrownBy Json.parse("{'key': 1}")
    a [JsonParseException] should be thrownBy Json.parse("{1: 2, 3: 4}")
    a [JsonParseException] should be thrownBy Json.parse("[\"hello \r\n world\"]")
    a [JsonParseException] should be thrownBy Json.parse("""{"hello": "world", "foo": "bar",}""")
  }

  it should "correctly fail to parse control characters" in {
    val controls = List(
      "\u0000", "\u0001", "\u0002", "\u0003", "\u0004",
      "\u0005", "\u0006", "\u0007", "\u000e", "\u000f",
      "\u0010", "\u0011", "\u0012", "\u0013", "\u0014",
      "\u0015", "\u0016", "\u0017", "\u0018", "\u0019",
      "\u001a", "\u001b", "\u001c", "\u001d", "\u001e",
      "\u001f"
    )
    controls.foreach { control =>
      a [JsonParseException] should be thrownBy Json.parse(s"""{$control}""")
    }
  }

  it should "correctly parse JSON edge-cases" in {
    noException should be thrownBy {
      Json.parse("{\r\n}")
      Json.parse("{\n\n\r\n}")
      Json.parse("{\t}")
      Json.parse("{ }")
      Json.parse("""[{}]""")
    }
  }

  it should "correctly parse JSON string values" in {
    noException should be thrownBy {
      assert("""{"a":"'\""}""".toJson.a.asString == "'\"")
      assert("""{"a":"value"}""".toJson.a.asString == "value")
      assert("""{"a":""}""".toJson.a.asString == "")
      assert("""{"a":"\u2028"}""".toJson.a.asString == "\u2028")
      assert("""{"a":"\u2029"}""".toJson.a.asString == "\u2029")
      assert("""{"a":"\ud834\udf06"}""".toJson.a.asString == "\ud834\udf06")
      assert("""{"a":"\b"}""".toJson.a.asString == "\b")
      assert("""{"a":"\f"}""".toJson.a.asString == "\f")
      assert("""{"a":"\n"}""".toJson.a.asString == "\n")
      assert("""{"a":"\r"}""".toJson.a.asString == "\r")
      assert("""{"a":"\t"}""".toJson.a.asString == "\t")
      assert("""{"a":"hello\/world"}""".toJson.a.asString == "hello/world")
      assert("""{"a":"hello\\world"}""".toJson.a.asString == "hello\\world")
      assert("""{"a":"hello\"world"}""".toJson.a.asString == "hello\"world")
    }
  }

  it should "correctly parse JSON numeric values" in {
    noException should be thrownBy {
      assert("""{"a":100}""".toJson.a.asInt == 100)
      assert("""{"a":-100}""".toJson.a.asInt == -100)
      assert("""{"a":10.5}""".toJson.a.asDouble == 10.5)
      assert("""{"a":-3.141}""".toJson.a.asDouble == -3.141)
      assert("""{"a":0.625}""".toJson.a.asDouble == 0.625)
      assert("""{"a":-0.03125}""".toJson.a.asDouble == -0.03125)
      assert("""{"a":1e3}""".toJson.a.asInt == 1000)
      assert("""{"a":1e+2}""".toJson.a.asInt == 100)
      assert("""{"a":-1e-2}""".toJson.a.asDouble == -0.01)
      assert("""{"a":0.03125e+5}""".toJson.a.asInt == 3125)
      assert("""{"a":1E2}""".toJson.a.asInt == 100)
    }
  }

  it should "correctly parse JSON objects and arrays" in {
    noException should be thrownBy {
      assert( """[1, 2, [3, [4, 5.991]], 6, [true, false], [null], [[]]]""".toJson(2)(1)(1).asDouble == 5.991)
      assert( """[100, true, false, null, {"a": ["hello"], "b": ["world"]}, [0.01]]""".toJson(4).b(0).asString == "world")
      assert( """{"hello": "world"}""".toJson.hello.asString == "world")
      assert( """{"hello": "world", "foo": ["bar", true], "fox": {"quick": true, "purple": false}}""".toJson.foo(1).asBoolean)
    }
  }

  /**
   * Typing tests.
   */
  it should "correctly parse to proper types" in {
    noException should be thrownBy {
      assert( """{"a":100}""".toJson.a.asType == "number")
      assert( """{"a":"100"}""".toJson.a.asType == "string")
      assert( """{"a":true}""".toJson.a.asType == "boolean")
      assert( """{"a":false}""".toJson.a.asType == "boolean")
      assert( """{"a":"true"}""".toJson.a.asType == "string")
      assert( """{"a":"true"}""".toJson.a.isNotNull)
      assert( """{"a":null}""".toJson.a.isNull)
    }
  }

  it should "correctly extract values as requested types" in {
    noException should be thrownBy {
      val json = sample
      assert(json.format.duration.asDouble == 46.665)
      assert(json.format.duration.asInt == 46)
      assert(json.format.duration.asBoolean)
      assert(json.format.duration.asLong == 46)
    }
  }

  it should "not use default values when extracting existing values" in {
    noException should be thrownBy {
      val json = sample
      assert(json.format.duration.asDouble(77.7) == 46.665)
      assert(json.format.duration.asInt(77) == 46)
      assert(json.format.duration.asBoolean(default = false))
      assert(json.format.duration.asLong(77) == 46)
    }
  }

  it should "use correct implicit default values when extracting non-existing values" in {
    noException should be thrownBy {
      val json = sample
      assert(json.missing.field.asDouble == 0)
      assert(json.missing.field.asInt == 0)
      assert(!json.missing.field.asBoolean)
      assert(json.missing.field.asLong == 0)
    }
  }

  it should "use explicit default values if given when extracting non-existing values" in {
    noException should be thrownBy {
      val json = sample
      assert(json.missing.field.asDouble(77.7) == 77.7)
      assert(json.missing.field.asInt(77) == 77)
      assert(json.missing.field.asBoolean(default = true))
      assert(json.missing.field.asLong(77) == 77)
    }
  }

  /**
   * Navigation/access tests.
   */

  it should "allow dynamic access using dot notation" in {
    noException should be thrownBy {
      val json = sample
      assert(json.format.tags.TITLE.asString == "Big Buck Bunny - test 8")
    }
  }

  it should "allow dynamic access to non-existing data" in {
    noException should be thrownBy {
      assert(sample.does.not.exist.isUndefined)
      assert(sample("does").not("exist").isUndefined)
      assert(sample("does")("not")("exist").isUndefined)
      assert(sample(0)(1).stuff(2)("foo").bar.isUndefined)
    }
  }

  it should "allow dynamic access using call notation" in {
    noException should be thrownBy {
      val json = sample
      assert(json("format").tags.TITLE.asString == "Big Buck Bunny - test 8")
      assert(json("format")("tags").TITLE.asString == "Big Buck Bunny - test 8")
      assert(json("format").tags("TITLE").asString == "Big Buck Bunny - test 8")
      assert(json("format")("tags")("TITLE").asString == "Big Buck Bunny - test 8")
      assert(json("streams")(0).codec_name.asString == "h264")
      assert(json.streams(1)("codec_name").asString == "aac")
      assert(json.streams(1).codec_name.asString == "aac")
    }
  }

  it should "allow dynamic access using negative array indexes" in {
    noException should be thrownBy {
      val json = sample
      assert(json.streams(-3)("codec_name").asString == "aac")
      assert(json.streams(-2).tags.language.asString == "jpn")
    }
  }

  /**
   * Querying tests.
   */

  it should "correctly perform queries" in {
    noException should be thrownBy {
      val json = sample
      assert(json.find("does not exist").isUndefined)
      assert(json.find("COMMENT").asString == "Matroska Validation File 8")
      assert(json.findAll("codec_name").length == 11)
      assert(json.findAll("language").length == 9)
      assert(json.findAll(_.asDouble > 0.0).length == 21)
    }
  }

  /**
   * Update tests.
   */

  it should "correctly update existing values and change types of values" in {
    noException should be thrownBy {
      val json = sample
      json.format.duration = true
      assert(json.format.duration.asBoolean)
      json.format.duration = 54
      assert(json.format.duration.asInt == 54)
      json.format.duration = null
      assert(json.format.duration.isNull)
      json.format.duration = "abc"
      assert(json.format.duration.asString == "abc")

      json.format.newStuff = 123
      assert(json.format.newStuff.asInt == 123)
      json.streams(-1) = 9972
      assert(json.streams(-1).asInt == 9972)
    }
  }

  it should "correctly create non-existing object containers and/or values on updates" in {
    noException should be thrownBy {
      val json = sample
      json.foo = Map("hint" -> 34)
      json.foo.bar = 1337
      assert(json.foo.bar.asInt == 1337)

      json.d = true
      assert(json.d.asBoolean)
      json.c.d = true
      assert(json.c.d.asBoolean)
      json.b.c.d = true
      assert(json.b.c.d.asBoolean)

      json("dd") = true
      assert(json("dd").asBoolean)
      json("cc")("dd") = true
      assert(json("cc")("dd").asBoolean)
      json("bb")("cc")("dd") = true
      assert(json("bb")("cc")("dd").asBoolean)
    }
  }

  it should "correctly create non-existing array containers and/or values on updates" in {
    noException should be thrownBy {
      val json = sample

      json.boo = List()
      json.boo(-1) = 1337
      assert(json.boo(0).asInt == 1337)
      json.boo(10) = 7331
      assert(json.boo(10).asInt == 7331)

      json.list = List()
      json.list(25) = 45.89
      assert(json.list(25).asDouble == 45.89)

      json.list(30).foo.bar(34)("stuff") = List(1, 2, 3)
      assert(json("list")(30)("foo")("bar")(34).stuff(2).asInt == 3)
      json.list(30)("foo").bar(34).stuff(4) = 441
      assert(json.list(30).foo.bar(34).stuff.asValues.size == 5)
      assert(json.list(30).foo.bar(34).stuff(3).isNull)
    }
  }

  it should "disallow non-explicit change of types for dictionary/array values" in {
    a [RuntimeException] should be thrownBy { sample.format(7) = 54 }
    a [RuntimeException] should be thrownBy { sample.streams.field = 54 }
  }

  /**
   * Rendering tests.
   */
  it should "properly serialize beautified and non-beautified JSONs" in {
    val json = Json.parse("""{"a":123,"b":{"c":4.56, "d":[1,2,{"e":"f"},4]},"g":7}""")
    assert(json.toString == """{"a":123,"b":{"c":4.56,"d":[1,2,{"e":"f"},4]},"g":7}""")
    assert(json.toString("") ==
      """{
        |  "a": 123,
        |  "b": {
        |    "c": 4.56,
        |    "d": [
        |      1,
        |      2,
        |      {
        |        "e": "f"
        |      },
        |      4
        |    ]
        |  },
        |  "g": 7
        |}""".stripMargin.replace("\r",""))
    assert(json.b.d.toString == """[1,2,{"e":"f"},4]""" )
    assert(json.b.d.toString("") ==
      """[
        |  1,
        |  2,
        |  {
        |    "e": "f"
        |  },
        |  4
        |]""".stripMargin.replace("\r",""))
    }


  /**
   * Object creation tests.
   */
  it should "correctly create new Json object from raw values" in {
    noException should be thrownBy {
      assert(Json(true).asString == "true")
      assert(Json(0.89).asString == "0.89")
      assert(Json("Abc").asString == "Abc")
      assert(Json(null).asString == "null")

      assert(Json(Map("first" -> 1.0, "stuff" -> 22.0)).stuff.asLong == 22)
      assert(Json(Map("first" -> 1.0, "second" -> Map("stuff" -> 81))).second.stuff.asLong == 81)
      assert(Json(Seq(1.0, 22.3304))(1).asDouble == 22.3304)
      assert(Json(Seq(1.0, 2.0, 33.44506))(2).asLong== 33)
    }
  }

  /**
   * Reald-world tests (these need and active Internet connection).
   */
  it should "parse a predefined set of online (real-world) sources (i.e. URLs)" in {
    noException should be thrownBy {
      Json.parse(new URL("https://api.github.com/users/vpalos/repos"))
      Json.parse(new URL("http://mysafeinfo.com/api/data?list=englishmonarchs&format=json"))
    }
  }

}