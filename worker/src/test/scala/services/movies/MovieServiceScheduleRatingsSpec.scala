package services.movies

import clients.TmdbClient
import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{DomainEvent, ImdbIdMissing, InProcessEventBus, TmdbResolved}
import tools.RoutingHttpFetch

import scala.collection.mutable.ListBuffer

/**
 * `scheduleRatingsForNewMovie` is how a film freshly PROMOTED out of staging
 * (vs merged into an existing movie) gets its first-time ratings. It re-publishes
 * the same resolution event the direct scrape path fires, so the existing
 * `RatingEnqueuer` queues IMDb/Filmweb/RT/Metacritic — no new path. This pins the
 * gate: a resolved promotion publishes, a `tmdbNoMatch` one stays silent.
 */
class MovieServiceScheduleRatingsSpec extends AnyFlatSpec with Matchers {

  private val deadTmdb = new TmdbClient(http = RoutingHttpFetch.dead("unused"), apiKey = None)

  private def serviceWith(bus: InProcessEventBus): MovieService =
    new MovieService(new CaffeineMovieCache(new InMemoryMovieRepository()), bus, deadTmdb)

  private def captureEvents(): (InProcessEventBus, ListBuffer[DomainEvent]) = {
    val bus  = new InProcessEventBus()
    val seen = ListBuffer.empty[DomainEvent]
    bus.subscribe { case e => seen += e }
    (bus, seen)
  }

  "scheduleRatingsForNewMovie" should "publish TmdbResolved (→ all ratings) for a promotion with an imdbId" in {
    val (bus, seen) = captureEvents()
    serviceWith(bus).scheduleRatingsForNewMovie(
      CacheKey("Kumotry", Some(2026)), MovieRecord(tmdbId = Some(1454157), imdbId = Some("tt1454157")))

    seen.toSeq shouldBe Seq(TmdbResolved("Kumotry", Some(2026), "tt1454157"))
  }

  it should "publish ImdbIdMissing (→ the non-IMDb ratings) for a promotion resolved without an imdbId" in {
    val (bus, seen) = captureEvents()
    serviceWith(bus).scheduleRatingsForNewMovie(
      CacheKey("Kumotry", Some(2026)), MovieRecord(tmdbId = Some(1454157), imdbId = None))

    seen.toSeq should matchPattern { case Seq(ImdbIdMissing("Kumotry", Some(2026), _)) => }
  }

  it should "publish nothing for a tmdbNoMatch promotion (no id to query ratings against)" in {
    val (bus, seen) = captureEvents()
    serviceWith(bus).scheduleRatingsForNewMovie(
      CacheKey("Obscure Local Premiere", Some(2026)), MovieRecord(tmdbNoMatch = true))

    seen shouldBe empty
  }
}
