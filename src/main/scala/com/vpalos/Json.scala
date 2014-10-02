package com.vpalos

import java.io._
import java.net.URL

import org.apache.commons.lang3.StringEscapeUtils

import scala.collection.mutable
import scala.collection.JavaConversions._
import scala.language.{dynamics, implicitConversions}
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

/**
 * A dynamic representation of a Json document enabling very intuitive parsing, manipulation
 * and navigation within Json data from code. This implementation is not optimized for speed
 * or low memory usage, it is meant to be useful in scripting scenarios where navigating the
 * JSON structure can be rather verbose.
 */
abstract class Json extends Dynamic {

  /**
   * Dynamic look-ups.
   */
  def apply(name: String): Json
  def apply(index: Int): Json
  def selectDynamic(name: String) = apply(name)
  def applyDynamic(name: String)  = apply(name)

  /**
   * Dynamic update functions.
   */
  def update(name: String, value: JValue[_]): Unit
  def update(index: Int, value: JValue[_]): Unit
  def updateDynamic(name: String)(value: JValue[_]): Unit = update(name, value)

  /**
   * Type checkers.
   */
  def asType: String
  def isDefined = true
  def isNull    = false
  def isObject  = false
  def isArray   = false

  /**
   * Syntactic sugars.
   */
  final def isUndefined = !isDefined
  final def isNotNull   = !isNull

  /**
   * Scalar extraction functions.
   */
  def asBoolean: Boolean
  def asBoolean(default: Boolean): Boolean
  def asString: String
  def asString(default: String): String
  def asInt: Int
  def asInt(default: Int): Int
  def asLong: Long
  def asLong(default: Long): Long
  def asDouble: Double
  def asDouble(default: Double): Double

  /**
   * Collection extraction functions.
   */
  def asKeys: TraversableOnce[String] = Seq.empty[String]
  def asValues: TraversableOnce[Json] = Seq.empty[Json]

  /**
   * Querying functions.
   */
  final def find(name: String): Json = {
    val query = findAll(name)
    if (query.hasNext) {
      query.next()
    } else {
      JUndefined.Instance
    }
  }
  final def find(op: Json => Boolean): Json = {
    val query = findAll(op)
    if (query.hasNext) {
      query.next()
    } else {
      JUndefined.Instance
    }
  }

  final def exists(name: String): Boolean = find(name).isDefined
  final def exists(op: Json => Boolean): Boolean = find(op).isDefined

  def findAll(name: String): Iterator[Json]
  def findAll(op: Json => Boolean): Iterator[Json]
}

abstract class JValue[T](val data: T) extends Json {

  /**
   * Internals.
   */
  protected def buildObject: JValue[_]  = this
  protected def buildArray: JValue[_]   = this

  protected def attempt[S](op: => S, default: S) = {
    try {
      op
    } catch {
      case _: Exception => default
    }
  }

  protected def nullify(value: JValue[_]): JValue[_] = {
    if (value != null) {
      value
    } else {
      JNull.Instance
    }
  }

  /**
   * Dynamic look-ups.
   */
  def apply(name: String): Json = new JUndefinedInObject(parent = this, name = name)
  def apply(index: Int): Json   = new JUndefinedInArray(parent = this, index = index)

  /**
   * Dynamic update functions.
   */
  def update(name: String, value: JValue[_]): Unit = {
    throw new RuntimeException(s"Attempting to update a dictionary key ($name) on something that is not a dictionary!")
  }

  def update(index: Int, value: JValue[_]): Unit = {
    throw new RuntimeException(s"Attempting to update an array index ($index) on something that is not an array!")
  }

  /**
   * Default scalar extraction functions.
   */
  def asBoolean                         = asBoolean(default = false)
  def asBoolean(default: Boolean)       = default
  def asString                          = asString("")
  def asString(default: String)         = default
  def asInt                             = asInt(0)
  def asInt(default: Int)               = default
  def asLong                            = asLong(0L)
  def asLong(default: Long)             = default
  def asDouble                          = asDouble(0.0)
  def asDouble(default: Double)         = default

  /**
   * Default string renderer.
   */
  override def toString = data.toString
  def toString(prefix: String): String = toString

  /**
   * Querying functions.
   */
  override def findAll(name: String): Iterator[Json] = Iterator.empty
  override def findAll(op: Json => Boolean): Iterator[Json] = Iterator.empty
}

/**
 * Class representing any undefined Json value.
 *
 * JUndefined objects are created, for example, when trying to access a non-existing field
 * in a JSON structure (e.g. `json.foo.bar`). However,the new instance of JUndefined keeps
 * a record of how it was requested (i.e. parent and key) to be able to actually build the
 * missing object itself (and any other missing links up the chain) in case of updates.
 */
class JUndefined extends JValue(null) {

  def asType = "undefined"
  override def isDefined = false
  override def toString = ""

  override def update(name: String, value: JValue[_]) = buildObject.update(name, value)
  override def update(index: Int, value: JValue[_])   = buildArray.update(index, value)
}
object JUndefined {
  val Instance = new JUndefined()
}

/**
 * Class representing an undefined Json dictionary.
 */
class JUndefinedInObject(val parent: JValue[_], val name: String) extends JUndefined {
  override protected def buildObject: JValue[_] = {
    val spawn = new JObject(Map.empty[String, JValue[_]])
    parent.update(name, spawn)
    spawn
  }
  override protected def buildArray: JValue[_] = {
    val spawn = new JArray()
    parent.update(name, spawn)
    spawn
  }
}

/**
 * Class representing an undefined Json array.
 */
class JUndefinedInArray(val parent: JValue[_], val index: Int) extends JUndefined {
  override protected def buildObject: JValue[_] = {
    val spawn = new JObject(Map.empty[String, JValue[_]])
    parent.update(index, spawn)
    spawn
  }
  override protected def buildArray: JValue[_] = {
    val spawn = new JArray()
    parent.update(index, spawn)
    spawn
  }
}

/**
 * Class representing null Json values.
 */
class JNull extends JValue[Null](null) {
  def asType = "null"
  override def isNull = true
  override def toString = "null"
  override def asString = "null"
}
object JNull {
  val Instance = new JNull()
}

/**
 * Class representing boolean Json values.
 */
class JBoolean(value: Boolean) extends JValue(value) {
  def asType = "boolean"

  override def asBoolean(default: Boolean)        = data
  override def asString(default: String)          = attempt(data.toString, default)
  override def asInt(default: Int)                = attempt(if (data) 1 else 0, default)
  override def asLong(default: Long)              = attempt(if (data) 1 else 0, default)
  override def asDouble(default: Double)          = attempt(if (data) 1 else 0, default)
}
object JBoolean {
  val True = new JBoolean(true)
  val False = new JBoolean(false)
}

/**
 * Class representing numeric Json values.
 */
class JNumber(value: Double) extends JValue(value) {
  def asType = "number"

  override def asBoolean(default: Boolean)        = data != 0.0
  override def asString(default: String)          = data.toString
  override def asInt(default: Int)                = data.toInt
  override def asLong(default: Long)              = data.toLong
  override def asDouble(default: Double)          = data

  override def toString = {
    if (data.isInfinity || data.isNaN) {
      "null"
    } else {
      if (data.isWhole()) {
        f"$data%.0f"
      } else {
        s"$data"
      }
    }
  }
}

/**
 * Class representing string Json values. The `s` string interpolator is used to ensure
 * that null values get rendered correctly to "null" avoiding any NullPointerExceptions.
 *
 * Some conventions were used here to translate pure JSON string values into the proper
 * Scala types to with increased tolerance, specifically:
 * - only the string "false" will convert into Boolean `false`;
 * - floating point numbers can be converted in to integers (e.g. Long, BigInt etc.);
 */
class JString(value: String) extends JValue(StringEscapeUtils.unescapeJson(s"$value")) {
  def asType = "string"

  override def asBoolean(default: Boolean)        = attempt(data != "false", default)
  override def asString(default: String)          = attempt(data.toString, default)
  override def asInt(default: Int)                = attempt(data.toDouble.toInt, default)
  override def asLong(default: Long)              = attempt(data.toDouble.toLong, default)
  override def asDouble(default: Double)          = attempt(data.toDouble, default)

  override def toString = {
    s""""${StringEscapeUtils.escapeJson(data.toString)}""""
  }
}

/**
 * Class representing Json object nodes.
 */
class JObject(value: mutable.Map[String, JValue[_]]) extends JValue(value) {

  def this(pairs: Map[String, JValue[_]] = Map.empty[String, JValue[_]]) = this(mutable.Map(pairs.toSeq: _*))
  def this(pairs: (String, JValue[_])*) = this(mutable.Map(pairs.toSeq: _*))

  def asType = "object"
  override def isObject = true

  override def apply(name: String): Json = {
    data.getOrElse(name, new JUndefinedInObject(parent = this, name = name))
  }

  override def update(name: String, value: JValue[_]): Unit = {
    data.update(name, nullify(value))
  }

  override def asKeys: Iterable[String] = data.keys
  override def asValues: Iterable[Json] = data.values

  override def findAll(name: String): Iterator[Json] = {
    val currentLevel = data.iterator.filter {
      case (key: String, _: JValue[_]) => key == name
    }.map(_._2)
    val deeperLevels = data.valuesIterator.flatMap {
      case o: JObject => o.findAll(name)
      case a: JArray  => a.findAll(name)
      case _          => Iterator.empty
    }
    currentLevel ++ deeperLevels
  }

  override def findAll(op: Json => Boolean): Iterator[Json] = {
    val currentLevel = data.iterator.filter {
      case (key: String, value: JValue[_]) => op(value)
    }.map(_._2)
    val deeperLevels = data.valuesIterator.flatMap {
      case o: JObject => o.findAll(op)
      case a: JArray  => a.findAll(op)
      case _          => Iterator.empty
    }
    currentLevel ++ deeperLevels
  }

  override def toString(prefix: String): String = {
    val indent = s"$prefix  "
    data.toArray.sortBy(_._1).map {
      case (key: String, value: JValue[_]) =>
        s""""${StringEscapeUtils.escapeJson(key)}": ${value.toString(indent)}"""
    }.mkString(s"{\n$indent", s",\n$indent", s"\n$prefix}")
  }

  override def toString = toString("")
}

/**
 * Class representing Json array nodes.
 */
class JArray(values: Seq[JValue[_]] = Seq.empty[JValue[_]]) extends JValue(mutable.ArrayBuffer.concat(values)) {
  def asType = "array"
  override def isArray = true

  private def normalize(index: Int) = {
    val length = data.length
    if (index >= 0) {
      index
    } else {
      if (length == 0) {
        0
      } else {
        length + (index % length)
      }
    }
  }

  override def apply(index: Int): Json = {
    val realIndex = normalize(index)
    if (realIndex < data.length) {
      data(realIndex)
    } else {
      new JUndefinedInArray(parent = this, index = realIndex)
    }
  }

  override def update(index: Int, value: JValue[_]): Unit = {
    val realIndex = normalize(index)
    val fillSpace = realIndex - data.length
    if (fillSpace > 0) {
      data ++= Seq.fill(fillSpace)(JNull.Instance)
    }
    if (fillSpace >= 0) {
      data += nullify(value)
    } else {
      data.update(realIndex, nullify(value))
    }
  }

  override def asValues: Iterable[Json] = data

  override def findAll(name: String): Iterator[Json] = {
    data.iterator.flatMap {
      case o: JObject => o.findAll(name)
      case a: JArray  => a.findAll(name)
      case _          => Iterator.empty
    }
  }

  override def findAll(op: Json => Boolean): Iterator[Json] = {
    val currentLevel = data.iterator.filter(op)

    val deeperLevels = data.iterator.flatMap {
      case o: JObject => o.findAll(op)
      case a: JArray  => a.findAll(op)
      case _          => Iterator.empty
    }

    currentLevel ++ deeperLevels
  }

  override def toString(prefix: String): String = {
    val indent = s"$prefix  "
    data.map(item => s"${item.toString(indent)}")
        .mkString(s"[\n$indent", s",\n$indent", s"\n$prefix]")
  }

  override def toString = toString("")
}

/**
 * String wrapper class augmented with Json parsing.
 */
class JsonString(val value: String) {
  def toJson: Json = Json.parse(value)
}

/**
 * Json parsing exception.
 */
case class JsonParseException(message: String, line: Int, column: Int)
  extends Exception(s"JSON parsing error [line $line, column $column]: $message.")

/**
 * Json type classes and static constructs.
 */
object Json {

  /**
   * Json object creation static functions.
   */
  def apply():                JValue[_] = new JObject()
  def apply(json: JValue[_]): JValue[_] = json
  def apply(value: Null):     JValue[_] = JNull.Instance

  /**
   * Internal Json parser; built following the official JSON definition (http://json.org).
   *
   * Notes:
   *
   * A string literal is a pair of double quotes surrounding a sequence of:
   * - any characters except double quotes, control characters or backslash (`\`);
   * - a backslash (`\`) followed by:
   *    - another backslash;
   *    - a double quote character (`"`);
   *    - one of the letters `b`, `f`, `n`, `r` or `t`;
   *    - Unicode script code: `u` followed by four hexadecimal digits.
   *
   * A numeric literal (i.e. number) is a numeric character sequence following these rules:
   * - an integer (e.g. `123`);
   * - an integer followed by a decimal point (e.g. `123.`);
   * - an integer followed by a decimal point and fractional part (e.g. `3.14`);
   * - a decimal point followed by a fractional part (e.g. `.1`);
   * - any of the above followed by `e` or `E` and an optionally signed integer (e.g. `123.4e-10`).
   */
  private class JsonParser extends RegexParsers with PackratParsers {

    lazy val literalString: PackratParser[String] =
      "\"" ~> """([^"\p{Cntrl}\\]|\\[\\/"bfnrt]|\\u\p{XDigit}{4})*+""".r <~ "\""

    lazy val jNull: PackratParser[JNull] =
      "null" ^^ { _ => JNull.Instance }

    lazy val jBoolean: PackratParser[JBoolean] = {
      "true" ^^ { _ => JBoolean.True} | "false" ^^ { _ => JBoolean.False}
    }

    lazy val jNumber: PackratParser[JNumber] =
      """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?""".r ^^ { number => new JNumber(number.toDouble) }

    lazy val jString: PackratParser[JString] =
      literalString ^^ { s => new JString(s) }

    lazy val jField: PackratParser[(String, JValue[_])] =
      ((literalString <~ ":") ~ jValue) ^^ { l => l._1 -> l._2 }

    lazy val jMap: PackratParser[JObject] =
      "{" ~> repsep(jField, ",") <~ "}" ^^ { l => new JObject(mutable.Map(l: _*)) }

    lazy val jArray: PackratParser[JArray] =
      "[" ~> repsep(jValue, ",") <~ "]" ^^ { l => new JArray(l) }

    lazy val jValue: PackratParser[JValue[_]] =
      jObject | jNull | jBoolean | jNumber | jString

    lazy val jObject: PackratParser[JValue[_]] =
      jMap | jArray
  }

  /**
   * Statically available parser instance.
   */
  private val parser = new JsonParser

  /**
   * Parse Json document from a Reader source.
   */
  @throws
  def parse(input: Reader) = {
    parser.parseAll(parser.jObject, input) match {
      case parser.Success(result: JValue[_], next) =>
        result
      case parser.NoSuccess(message, next) =>
        throw new JsonParseException(message, next.pos.line, next.pos.column)
    }
  }

  /**
   * Parse Json document from a String source. Alternatively, use the `.toJson()` method on it.
   */
  def parse(input: String): Json = parse(new StringReader(input))

  /**
   * Parse Json document from an stream source.
   */
  def parse(input: InputStream): Json = parse(new InputStreamReader(input))

  /**
   * Parse Json document from a URL source.
   */
  def parse(input: URL): Json = parse(input.openStream())

  /**
   * Parse Json document from a File source.
   */
  def parse(input: File): Json = parse(new FileInputStream(input))

  /**
   * Parse Json document from an array fo bytes source.
   */
  def parse(input: Array[Byte]): Json = parse(new ByteArrayInputStream(input))

  /**
   * Augment the String class with JsonString to mixin the provided helper method(s).
   */
  @inline implicit def String_to_JsonString(s: String): JsonString = new JsonString(s)
  @inline implicit def JsonString_to_String(j: JsonString): String = j.value

  /**
   * Implicit value extractors.
   */
  implicit def Json_to_Boolean(json: Json): Boolean = json.asBoolean
  implicit def Json_to_Int(json: Json): Int         = json.asInt
  implicit def Json_to_Long(json: Json): Long       = json.asLong
  implicit def Json_to_Double(json: Json): Double   = json.asDouble
  implicit def Json_to_String(json: Json): String   = json.asString

  /**
   * Implicit converters for value/collection assignments.
   */
  implicit def Any_to_JValue(value: Any): JValue[_] = {
    value match {
      case v: Boolean => new JBoolean(v)
      case v: Int => new JNumber(v)
      case v: Long => new JNumber(v)
      case v: Double => new JNumber(v)
      case v: BigInt => new JNumber(v.toLong)
      case v: BigDecimal => new JNumber(v.toDouble)
      case v: String => new JString(v)
      case v: Map[String @unchecked, _] => new JObject(v.mapValues(Any_to_JValue))
      case v: java.util.Map[String @unchecked, _] => Any_to_JValue(v.toMap[String, Any])
      case v: TraversableOnce[_] => new JArray(v.toSeq.map(Any_to_JValue))
      case v: Array[_] => new JArray(v.toSeq.map(Any_to_JValue))
      case v: JValue[_] => v
      case _ => throw new RuntimeException(s"Can't convert '${value.getClass.getCanonicalName}' into a Json value!")
    }
  }

}
