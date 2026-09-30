package services.movies

import models.Showtime
import org.bson.codecs.configuration.CodecRegistries.{fromCodecs, fromProviders, fromRegistries}
import org.bson.codecs.{Codec, DecoderContext}
import org.bson.{BsonDocument, BsonDocumentReader}
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.PersistedCodecs

import java.time.{Instant, LocalDateTime}
import scala.util.Try

/**
 * Showtimes and `screenings` rows are decoded by hand, field by field (a US corpus pass reads
 * ~1.7M showtimes, and the macro codec's per-document machinery was most of its CPU) — and must
 * read every stored shape EXACTLY as the macro codec reads it. Each document here goes through
 * both, and the two must agree: on the value, or on failing.
 */
class ShowtimeDecodeSpec extends AnyFlatSpec with Matchers {

  private val macroRegistry = fromRegistries(
    fromCodecs(JavaTimeCodecs.localDateTime),
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
    MovieCodecs.registry.get(classOf[Showtime]).getClass.getName should include("StreamingShowtimeCodec")
    MovieCodecs.registry.get(classOf[StoredScreeningsDto]).getClass.getName should include("StreamingScreeningsCodec")
  }

  "a showtime" should "read every stored shape exactly as the macro codec reads it" in {
    showtimes.foreach { json =>
      val (viaMacro, streamed) = both(classOf[Showtime], json)
      withClue(json) {
        streamed.isSuccess shouldBe viaMacro.isSuccess
        streamed.toOption shouldBe viaMacro.toOption
      }
    }
    both(classOf[Showtime], showtimes.head)._2.get shouldBe
      Showtime(LocalDateTime.of(2026, 12, 17, 13, 0), Some("https://www.cinemark.com/refer.aspx?t=242&sid=738379"), Some("Sala 3"), List("2D", "NAP"))
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
}
