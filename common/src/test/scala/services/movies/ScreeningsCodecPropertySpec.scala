package services.movies

import org.bson.codecs.{DecoderContext, EncoderContext}
import org.bson.{BsonDocument, BsonDocumentReader, BsonDocumentWriter}
import org.scalacheck.Gen
import services.IdentityPropertySpec
import services.ShowtimeGenerators.{genShowtime, genText}

import java.time.Instant

/** Every `screenings` row the worker writes reads back as written, over generated rows — its
 *  booking URLs stored split at whatever prefix they share and read back through the streaming
 *  codec, so a split the reader cannot undo would hand the read model a wrong link. */
class ScreeningsCodecPropertySpec extends IdentityPropertySpec {

  private val genRow: Gen[StoredScreeningsDto] = for {
    id         <- genText
    filmId     <- genText
    slotKey    <- genText
    showtimes  <- Gen.listOf(genShowtime)
    // A BSON date holds milliseconds.
    updatedAt  <- Gen.choose(0L, 4_000_000_000_000L).map(Instant.ofEpochMilli)
    listingKey <- Gen.option(genText)
  } yield StoredScreeningsDto(id, filmId, slotKey, showtimes, updatedAt, listingKey)

  "a screenings row" should "read back as the row written, every booking URL as it was spelled" in {
    val codec = MovieCodecs.registry.get(classOf[StoredScreeningsDto])
    forAll(genRow) { row =>
      val out = new BsonDocument()
      codec.encode(new BsonDocumentWriter(out), row, EncoderContext.builder().build())
      val read = codec.decode(new BsonDocumentReader(out), DecoderContext.builder().build())
      read shouldBe row
      read.showtimes.map(_.bookingUrl) shouldBe row.showtimes.map(_.bookingUrl)
    }
  }
}
