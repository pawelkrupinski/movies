package services.readmodel

import models.{ResolvedMovie, ResolvedRatings}
import org.bson.codecs.{DecoderContext, EncoderContext}
import org.bson.{BsonDocument, BsonDocumentReader, BsonDocumentWriter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The read-model full-scan reads (`findAllMovies` / `findAllScreenings`) decode each row
 * PER-DOCUMENT so one malformed/legacy `web_movies` doc — e.g. missing the required,
 * non-Option `ratings` field — is skipped rather than sinking the whole keyset page (up to
 * `findAllBatchSize` valid films). Exercises `decodeTolerant` directly with an in-memory
 * `BsonDocument` batch (no Mongo). Fails before the fix (the whole-page decode threw on the
 * bad doc); passes after (the bad doc is skipped, the valid ones survive).
 */
class MongoReadModelRepositoryDecodeSpec extends AnyFlatSpec with Matchers {

  private val codec = ReadModelCodecs.registry.get(classOf[ResolvedMovie])
  // sharedDb = None → a no-op repository (no Mongo I/O); only `decodeTolerant` is under test.
  private val repo  = new MongoReadModelRepository(None)

  private def movie(id: String): ResolvedMovie = ResolvedMovie(
    _id = id, title = "T", originalTitle = None, posterUrl = None, fallbackPosterUrls = Seq.empty,
    runtimeMinutes = None, releaseYear = None, genres = Seq.empty, countries = Seq.empty,
    directors = Seq.empty, cast = Seq.empty, synopsis = None, trailerUrls = Seq.empty,
    ratings = ResolvedRatings(None, None, None, "https://mc", None, "https://rt", None, "https://fw"),
    weightedRating = 0.0)

  private def encode(m: ResolvedMovie): BsonDocument = {
    val out = new BsonDocument()
    codec.encode(new BsonDocumentWriter(out), m, EncoderContext.builder().build())
    out
  }

  "decodeTolerant" should "skip a document missing the required `ratings` field, keeping the valid ones" in {
    val good1 = encode(movie("a|1"))
    val good2 = encode(movie("c|3"))
    val bad   = encode(movie("b|2"))
    bad.remove("ratings") // a legacy/malformed row: `ratings` is required (non-Option)

    // Sanity: the bad doc really is undecodable through the codec — this is the throw the
    // batch `find().toFuture()` used to raise, which sank the whole keyset page.
    an[Exception] should be thrownBy
      codec.decode(new BsonDocumentReader(bad), DecoderContext.builder().build())

    repo.decodeTolerant(Seq(good1, bad, good2), codec, "test").map(_._id) shouldBe Seq("a|1", "c|3")
  }
}
