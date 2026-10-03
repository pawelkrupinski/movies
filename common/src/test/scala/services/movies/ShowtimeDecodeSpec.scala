package services.movies

import models.{Showtime, SourceData}
import org.bson.codecs.configuration.CodecRegistries.{fromCodecs, fromProviders, fromRegistries}
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.bson.{BsonDocument, BsonDocumentReader, BsonDocumentWriter}
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.PersistedCodecs

import java.time.{Instant, LocalDateTime}
import scala.util.Try

/**
 * Showtimes and `screenings` rows are decoded by hand, field by field (a US corpus pass reads
 * ~1.7M showtimes, and the macro codec's per-document machinery was most of its CPU) — and must
 * read every stored shape EXACTLY as the macro codec reads it. A row goes through both, and the
 * two must agree: on the value, or on failing. A showtime has no macro codec any more (`Showtime`
 * is not a case class), so its shapes are pinned to what the macro read and wrote, captured from
 * it on 2026-10-03.
 */
class ShowtimeDecodeSpec extends AnyFlatSpec with Matchers {

  private val macroRegistry = fromRegistries(
    fromCodecs(JavaTimeCodecs.localDateTime, ShowtimeCodec),
    fromProviders((PersistedCodecs.omittingNone[MovieCodecs.OmittingNone] ::: PersistedCodecs.writingNone[MovieCodecs.WritingNone])*),
    DEFAULT_CODEC_REGISTRY)

  private def both[A](clazz: Class[A], json: String): (Try[A], Try[A]) = {
    def read(codec: Codec[A]) = Try(codec.decode(new BsonDocumentReader(BsonDocument.parse(json)), DecoderContext.builder().build()))
    (read(macroRegistry.get(clazz)), read(MovieCodecs.registry.get(clazz)))
  }

  private val at = """{ "$date": "2026-12-17T13:00:00Z" }"""

  private val showtimes = Seq(
    s"""{ "dateTime": $at, "bookingUrl": "https://www.cinemark.com/refer.aspx?t=242&sid=738379", "room": "Sala 3", "format": ["2D", "NAP"] }""",
    s"""{ "dateTime": $at }""",
    s"""{ "dateTime": $at, "bookingUrl": null, "room": null }""",
    s"""{ "dateTime": $at, "format": [], "retired": { "nested": [1, 2] }, "bookingUrl": "https://x/1" }""",
    s"""{ "format": ["IMAX"], "dateTime": $at, "room": "1" }""",
    s"""{ "bookingUrl": "https://x/1" }""")

  "the registry" should "decode showtimes and screenings rows with the streaming codecs, not the macros" in {
    MovieCodecs.registry.get(classOf[Showtime]) shouldBe ShowtimeCodec
    MovieCodecs.registry.get(classOf[StoredScreeningsDto]).getClass.getName should include("StreamingScreeningsCodec")
  }

  private def decoded(json: String): Try[Showtime] =
    Try(ShowtimeCodec.decode(new BsonDocumentReader(BsonDocument.parse(json)), DecoderContext.builder().build()))

  private val dateTime = LocalDateTime.of(2026, 12, 17, 13, 0)

  "a showtime" should "read every stored shape exactly as the macro codec read it" in {
    showtimes.map(decoded(_).toOption) shouldBe Seq(
      Some(Showtime(dateTime, Some("https://www.cinemark.com/refer.aspx?t=242&sid=738379"), Some("Sala 3"), List("2D", "NAP"))),
      Some(Showtime(dateTime, None)),
      Some(Showtime(dateTime, None)),
      Some(Showtime(dateTime, Some("https://x/1"))),
      Some(Showtime(dateTime, None, Some("1"), List("IMAX"))),
      None)
  }

  it should "be written exactly as the macro codec wrote it, however its URL is held" in {
    def written(showtime: Showtime) = {
      val out = new BsonDocument()
      ShowtimeCodec.encode(new BsonDocumentWriter(out), showtime, EncoderContext.builder().build())
      out.toJson
    }
    written(Showtime(dateTime, None)) shouldBe """{"dateTime": {"$date": "2026-12-17T13:00:00Z"}, "format": []}"""
    written(Showtime(dateTime, Some("u"), Some("r"), List("2D", "NAP"))) shouldBe
      """{"dateTime": {"$date": "2026-12-17T13:00:00Z"}, "bookingUrl": "u", "room": "r", "format": ["2D", "NAP"]}"""
    written(Showtime(dateTime, None, None, List("X"))) shouldBe """{"dateTime": {"$date": "2026-12-17T13:00:00Z"}, "format": ["X"]}"""
    written(Showtime(dateTime, Some("https://x/1")).withUrlPrefix("https://x/")) shouldBe
      """{"dateTime": {"$date": "2026-12-17T13:00:00Z"}, "bookingUrl": "https://x/1", "format": []}"""
  }

  private val updated = """{ "$date": "2026-09-30T10:00:00Z" }"""
  private val rows = Seq(
    s"""{ "_id": "lalka|2026\u001fHelios", "filmId": "lalka|2026", "slotKey": "Helios", "showtimes": [${showtimes.head}, ${showtimes(1)}], "updatedAt": $updated, "listingKey": "k1" }""",
    s"""{ "_id": "a", "filmId": "a", "slotKey": "Helios", "showtimes": [], "updatedAt": $updated }""",
    s"""{ "_id": "a", "filmId": "a", "slotKey": "Helios", "showtimes": [${showtimes(2)}], "updatedAt": $updated, "listingKey": null, "extra": "x" }""",
    s"""{ "updatedAt": $updated, "listingKey": "k", "showtimes": [], "slotKey": "Helios", "filmId": "a", "_id": "a" }""",
    s"""{ "_id": "a", "filmId": "a", "slotKey": "Helios", "updatedAt": $updated }""",
    s"""{ "_id": "a", "slotKey": "Helios", "showtimes": [], "updatedAt": $updated }""")

  "a screenings row" should "read every stored shape exactly as the macro codec reads it" in {
    rows.foreach { json =>
      val (viaMacro, streamed) = both(classOf[StoredScreeningsDto], json)
      withClue(json) {
        streamed.isSuccess shouldBe viaMacro.isSuccess
        streamed.toOption shouldBe viaMacro.toOption
      }
    }
    both(classOf[StoredScreeningsDto], rows(1))._2.get shouldBe
      StoredScreeningsDto("a", "a", "Helios", Nil, Instant.parse("2026-09-30T10:00:00Z"), None)
  }

  it should "read its booking URLs split at the row's prefix, before or after its showtimes" in {
    SplitBookingUrlShapes.rows.foreach { case (fields, showtimes) =>
      val json = s"""{ "_id": "a", "filmId": "a", "slotKey": "Helios", $fields, "updatedAt": $updated }"""
      withClue(json) {
        val row = MovieCodecs.registry.get(classOf[StoredScreeningsDto])
          .decode(new BsonDocumentReader(BsonDocument.parse(json)), DecoderContext.builder().build())
        row.showtimes shouldBe showtimes
        row.showtimes.map(_.bookingUrl) shouldBe showtimes.map(_.bookingUrl)
        row.showtimes.exists(_.awaitsRowPrefix) shouldBe false
      }
    }
  }

  private def written[A](codec: Codec[A], value: A): String = {
    val out = new BsonDocument()
    codec.encode(new BsonDocumentWriter(out), value, EncoderContext.builder().build())
    out.toJson
  }

  it should "be written as its macro codec wrote it when its booking URLs share no prefix" in {
    val whole = StoredScreeningsDto("a", "a", "Helios",
      Seq(Showtime(dateTime, Some("https://x/1"), Some("1"), List("2D")), Showtime(dateTime, None)), Instant.parse("2026-09-30T10:00:00Z"), None)
    written(MovieCodecs.registry.get(classOf[StoredScreeningsDto]), whole) shouldBe
      written(macroRegistry.get(classOf[StoredScreeningsDto]), whole)
    val keyed = whole.copy(listingKey = Some("k"))
    written(MovieCodecs.registry.get(classOf[StoredScreeningsDto]), keyed) shouldBe
      written(macroRegistry.get(classOf[StoredScreeningsDto]), keyed)
  }

  it should "be written with its booking URLs split at the prefix they share, and read back unchanged" in {
    val codec = MovieCodecs.registry.get(classOf[StoredScreeningsDto])
    val row = StoredScreeningsDto("a", "a", "Helios", Seq(
      Showtime(dateTime, Some("https://kino.example/buy?show=101"), Some("1")),
      Showtime(dateTime, Some("https://kino.example/buy?show=2")),
      Showtime(dateTime, None)), Instant.parse("2026-09-30T10:00:00Z"), Some("k"))
    val json = written(codec, row)
    json shouldBe """{"_id": "a", "filmId": "a", "slotKey": "Helios", "bookingUrlPrefix": "https://kino.example/buy?show=", "showtimes": [""" +
      """{"dateTime": {"$date": "2026-12-17T13:00:00Z"}, "bookingUrlRest": "101", "room": "1", "format": []}, """ +
      """{"dateTime": {"$date": "2026-12-17T13:00:00Z"}, "bookingUrlRest": "2", "format": []}, """ +
      """{"dateTime": {"$date": "2026-12-17T13:00:00Z"}, "format": []}], "updatedAt": {"$date": "2026-09-30T10:00:00Z"}, "listingKey": "k"}"""
    codec.decode(new BsonDocumentReader(BsonDocument.parse(json)), DecoderContext.builder().build()) shouldBe row
  }

  private val slots = Seq(
    s"""{ "_id": "a\u001fHelios", "filmId": "a", "slotKey": "Helios", "slot": { "title": "Lalka", "showtimes": [${showtimes.head}] }, "updatedAt": $updated, "listingKey": "k" }""",
    s"""{ "_id": "a", "filmId": "a", "slotKey": "Helios", "slot": { "title": "Lalka", "cast": "A, B" }, "updatedAt": $updated }""",
    s"""{ "slot": {}, "updatedAt": $updated, "listingKey": null, "slotKey": "Helios", "filmId": "a", "_id": "a", "extra": 1 }""",
    s"""{ "_id": "a", "filmId": "a", "slotKey": "Helios", "updatedAt": $updated }""",
    s"""{ "_id": "a", "filmId": "a", "slot": {}, "updatedAt": $updated }""")

  "a movie_slots row" should "read every stored shape exactly as its macro codec, with the slot's own codec, reads it" in {
    // The macro registry's SourceData codec is the plain macro; the slot inside is compared through
    // the registry's own backward-compatible one on both sides, so this pins the WRAPPER.
    val slotCodec = MovieCodecs.registry.get(classOf[SourceData])
    val viaMacroRegistry = fromRegistries(fromCodecs(slotCodec), macroRegistry)
    MovieCodecs.registry.get(classOf[StoredSlotDto]).getClass.getName should include("StreamingSlotCodec")
    slots.foreach { json =>
      def read(codec: Codec[StoredSlotDto]) = Try(codec.decode(new BsonDocumentReader(BsonDocument.parse(json)), DecoderContext.builder().build()))
      val (viaMacro, streamed) = (read(viaMacroRegistry.get(classOf[StoredSlotDto])), read(MovieCodecs.registry.get(classOf[StoredSlotDto])))
      withClue(json) {
        streamed.isSuccess shouldBe viaMacro.isSuccess
        streamed.toOption shouldBe viaMacro.toOption
      }
    }
  }
}
