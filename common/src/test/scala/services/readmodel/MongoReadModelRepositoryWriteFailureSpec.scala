package services.readmodel

import models.{CityScreening, ResolvedMovie, ResolvedRatings}
import org.mongodb.scala.MongoClient
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A read-model write that FAILED must reach its caller, not be logged and returned as `Unit`.
 *
 * `ReadModelProjector` remembers what it wrote (`lastMovie`, `lastScreenings`) and skips an
 * identical projection next time. It forgets a card whose write THREW, so the next change
 * retries it — but these writes caught every failure, so the projector remembered a card
 * `web_movies` never took: every later identical projection skipped it, and only the sweep's
 * heal (a missing card or venue, 30 minutes late) or the daily content slice (a changed one)
 * ever wrote it.
 *
 * Points at a client that never connects (unroutable port, 200 ms server selection), so each
 * write fails fast without a Mongo.
 */
class MongoReadModelRepositoryWriteFailureSpec extends AnyFlatSpec with Matchers {

  private val repository = new MongoReadModelRepository(
    Some(MongoClient("mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=200").getDatabase("test")))

  private val movie = ResolvedMovie(
    _id = "a|1", title = "T", originalTitle = None, posterUrl = None, fallbackPosterUrls = Seq.empty,
    runtimeMinutes = None, releaseYear = None, genres = Seq.empty, countries = Seq.empty,
    directors = Seq.empty, cast = Seq.empty, synopsis = None, trailerUrls = Seq.empty,
    ratings = ResolvedRatings(None, None, None, "https://mc", None, "https://rt", None, "https://fw"),
    weightedRating = 0.0)

  "a read-model write that fails" should "throw to its caller, for every write" in {
    an[Exception] should be thrownBy repository.upsertMovie(movie)
    an[Exception] should be thrownBy repository.deleteMovie("a|1")
    an[Exception] should be thrownBy repository.deleteScreening("a|1#poznan#x")
    an[Exception] should be thrownBy repository.upsertScreening(
      CityScreening(_id = "a|1#poznan#x", filmId = "a|1", city = "poznan", cinema = "x", filmUrl = None, showtimes = Seq.empty))
  }
}
