package services.scrapes

import models.Showtime
import org.bson.codecs.configuration.CodecRegistries.{fromCodecs, fromProviders, fromRegistries}
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.bson.{BsonDocument, BsonDocumentReader, BsonDocumentWriter}
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.PersistedCodecs
import services.movies.{JavaTimeCodecs, ShowtimeCodec}

import scala.util.Try

/** An archived listing's showtimes go through the movies' hand-written [[ShowtimeCodec]] — a venue's
 *  listing is read back as each of its scrapes lands — and the rest of it through the macro codecs, so
 *  every stored listing reads and writes exactly as the macro codecs around that showtime codec do.
 *  (`ShowtimeDecodeSpec` pins the showtime's own shapes.) */
class ScrapeArchiveCodecsSpec extends AnyFlatSpec with Matchers {

  private val macroRegistry = fromRegistries(
    fromCodecs(JavaTimeCodecs.localDateTime, ShowtimeCodec),
    fromProviders(PersistedCodecs.omittingNone[ScrapeArchiveCodecs.OmittingNone]*),
    DEFAULT_CODEC_REGISTRY)

  private def both(json: String): (Try[StoredScrapeDto], Try[StoredScrapeDto]) = {
    def read(codec: Codec[StoredScrapeDto]) =
      Try(codec.decode(new BsonDocumentReader(BsonDocument.parse(json)), DecoderContext.builder().build()))
    (read(macroRegistry.get(classOf[StoredScrapeDto])), read(ScrapeArchiveCodecs.registry.get(classOf[StoredScrapeDto])))
  }

  private val at = """{ "$date": "2026-12-17T13:00:00Z" }"""
  private def film(showtimes: String*) =
    s"""{ "movie": { "title": "Lalka" }, "cast": [], "director": [], "showtimes": [${showtimes.mkString(", ")}], "externalIds": {} }"""
  private val rows = Seq(
    s"""{ "_id": "Helios", "scrapedAt": $at, "listingComplete": true, "films": [${film(
      s"""{ "dateTime": $at, "bookingUrl": "https://x/1", "room": "Sala 3", "format": ["2D", "NAP"] }""",
      s"""{ "dateTime": $at }""",
      s"""{ "dateTime": $at, "bookingUrl": null, "room": null, "extra": 1 }""")}, ${film()}] }""",
    s"""{ "_id": "Helios", "lastBarren": { "at": $at, "outcome": "error" } }""",
    s"""{ "_id": "Helios", "films": [${film(s"""{ "bookingUrl": "https://x/1" }""")}] }""")

  "the archive's registry" should "read and write showtimes with the hand-written codec" in {
    ScrapeArchiveCodecs.registry.get(classOf[Showtime]) shouldBe ShowtimeCodec
  }

  "an archived listing" should "read every stored shape exactly as the macro codecs read it" in {
    rows.foreach { json =>
      val (viaMacro, streamed) = both(json)
      withClue(json) {
        streamed.isSuccess shouldBe viaMacro.isSuccess
        streamed.toOption shouldBe viaMacro.toOption
      }
    }
    both(rows.head)._2.get.films.get.head.showtimes should have size 3
  }

  it should "be written exactly as the macro codecs write it" in {
    val row = both(rows.head)._1.get
    def written(codec: Codec[StoredScrapeDto]) = {
      val out = new BsonDocument(); codec.encode(new BsonDocumentWriter(out), row, EncoderContext.builder().build()); out
    }
    written(ScrapeArchiveCodecs.registry.get(classOf[StoredScrapeDto])) shouldBe written(macroRegistry.get(classOf[StoredScrapeDto]))
  }
}
