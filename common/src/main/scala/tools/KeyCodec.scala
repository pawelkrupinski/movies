package tools

import org.bson.json.{JsonMode, JsonWriterSettings}
import org.bson.{BsonDocument, BsonInt64, BsonString, BsonValue}

/** How a store's key is its documents' `_id`, and back: what makes a [[KeyedStore]] generic in its key. The value's
 *  half is [[tools.DocCodec]]. */
trait KeyCodec[K] {
  def id(key: K): BsonValue
  /** The key of a stored `_id`; None for an `_id` this store did not write (a marker document, a stray row). */
  def key(id: BsonValue): Option[K]
}

object KeyCodec {
  /** A string `_id`: most stores' (a serialised key, an occurrence id, a code). */
  val Text: KeyCodec[String] = new KeyCodec[String] {
    def id(key: String): BsonValue           = new BsonString(key)
    def key(id: BsonValue): Option[String]   = Option.when(id.isString)(id.asString.getValue)
  }

  /** A key written as text: `write` it, `read` it back (None for text that is no key). */
  def textual[K](write: K => String, read: String => Option[K]): KeyCodec[K] = new KeyCodec[K] {
    def id(key: K): BsonValue         = new BsonString(write(key))
    def key(id: BsonValue): Option[K] = Text.key(id).flatMap(read)
  }

  /** A 64-bit number `_id`, read back from any BSON number. */
  val Number: KeyCodec[Long] = new KeyCodec[Long] {
    def id(key: Long): BsonValue           = new BsonInt64(key)
    def key(id: BsonValue): Option[Long]   = Option.when(id.isNumber)(id.asNumber.longValue)
  }

  /** `id` as text a keyset scan can carry between pages and give back exactly ([[value]]): its extended JSON. */
  private[tools] def text(id: BsonValue): String = new BsonDocument("v", id).toJson(Exact)

  /** Extended JSON: every BSON type written as itself, so an Int64 reads back an Int64. */
  private val Exact = JsonWriterSettings.builder().outputMode(JsonMode.EXTENDED).build()

  /** The `_id` [[text]] wrote. */
  private[tools] def value(text: String): BsonValue = BsonDocument.parse(text).get("v")
}
