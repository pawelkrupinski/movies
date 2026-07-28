package services.readmodel

import models.{ResolvedMovie, ResolvedRatings}
import org.bson.codecs.{DecoderContext, EncoderContext}
import org.bson.{BsonDocument, BsonDocumentReader, BsonDocumentWriter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The read-model full-scan reads (`findAllMovies` / `findAllScreenings`) decode each row
 * PER-DOCUMENT so one malformed `web_movies` doc is skipped rather than sinking the whole
 * keyset page (up to `findAllBatchSize` valid films). Exercises `decodeTolerant` directly
 * with an in-memory `BsonDocument` batch (no Mongo).
 *
 * This is the row-level BACKSTOP, and it now pairs with `DefaultingCodec`, which fills a
 * field a stored document simply LACKS. The two do different jobs and the distinction is
 * worth keeping straight:
 *   - a row missing a field is an older SHAPE — it decodes with defaults and is SERVED,
 *     which is strictly better than dropping a film that is fine apart from, say, having
 *     no ratings block yet;
 *   - a row whose field is the wrong TYPE is corrupt, no default can rescue it, and it is
 *     skipped so it cannot take the page down.
 * The fixture below is therefore type-corrupt rather than merely missing a field, which is
 * what it used to be.
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

  "decodeTolerant" should "skip a CORRUPT document, keeping the valid ones" in {
    val good1 = encode(movie("a|1"))
    val good2 = encode(movie("c|3"))
    val bad   = encode(movie("b|2"))
    bad.put("ratings", new org.bson.BsonString("not-a-ratings-document"))  // wrong TYPE

    // Sanity: the bad doc really is undecodable — this is the throw the batch
    // `find().toFuture()` used to raise, which sank the whole keyset page.
    an[Exception] should be thrownBy
      codec.decode(new BsonDocumentReader(bad), DecoderContext.builder().build())

    repo.decodeTolerant(Seq(good1, bad, good2), codec, "test").map(_._id) shouldBe Seq("a|1", "c|3")
  }

  // The improvement `DefaultingCodec` buys: an older row that simply LACKS a field is no
  // longer a casualty. It used to be undecodable, so `decodeTolerant` dropped it and the
  // film vanished from the served corpus; now it decodes with defaults and is served.
  it should "SERVE a document that merely lacks a field, rather than dropping the film" in {
    val older = encode(movie("b|2"))
    older.remove("ratings")

    repo.decodeTolerant(Seq(older), codec, "test").map(_._id) shouldBe Seq("b|2")
  }
}
