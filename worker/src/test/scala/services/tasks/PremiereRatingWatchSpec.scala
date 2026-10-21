package services.tasks

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cadence.InMemoryRatingCadenceStore
import services.freshness.FreshnessKind
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository, LatestTitleYear, ListingSeed}
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.{Clock, Instant, LocalDate, ZoneOffset}
import scala.concurrent.duration._

class PremiereRatingWatchSpec extends AnyFlatSpec with Matchers {

  private given LatestTitleYear = LatestTitleYear(at("2026-10-23T00:00:00Z"))

  // "Jak żyć, żeby nie zwariować": PL release 2026-10-23, no Filmweb rating yet.
  private val TmdbId  = 1489938
  private val Release = LocalDate.parse("2026-10-23")
  private val FwKey   = s"${FreshnessKind.FilmwebRating.label}|tmdb:$TmdbId"
  private val ImdbKey = s"${FreshnessKind.ImdbRating.label}|tmdb:$TmdbId"
  private val RtKey   = s"${FreshnessKind.RtRating.label}|tmdb:$TmdbId"

  private def at(instant: String): Clock = Clock.fixed(Instant.parse(instant), ZoneOffset.UTC)

  private def watch(clock: Clock, releases: Map[Int, LocalDate] = Map(TmdbId -> Release),
                    country: Country = Country.default): PremiereRatingWatch = {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer),
      normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    // IMDb already rated it (a festival screening); Filmweb, RT and Metacritic have nothing.
    ListingSeed.row(cache, "Jak żyć, żeby nie zwariować")(_.copy(tmdbId = Some(TmdbId), imdbId = Some("tt1"), imdbRating = Some(6.8)))
    new PremiereRatingWatch(cache, ids => releases.view.filterKeys(ids.toSet).toMap, country, clock)
  }

  "PremiereRatingWatch" should "flag a still-unrated source of a film the day before its release" in {
    val w = watch(at("2026-10-22T09:00:00Z"))
    w.awaitingFirstRating(FwKey) shouldBe true
    w.awaitingFirstRating(RtKey) shouldBe true
  }

  it should "leave alone a source that already has a rating" in {
    watch(at("2026-10-23T09:00:00Z")).awaitingFirstRating(ImdbKey) shouldBe false
  }

  it should "flag nothing two days before the release or eight days after it" in {
    watch(at("2026-10-21T09:00:00Z")).awaitingFirstRating(FwKey) shouldBe false
    watch(at("2026-10-30T20:00:00Z")).awaitingFirstRating(FwKey) shouldBe true
    watch(at("2026-10-31T09:00:00Z")).awaitingFirstRating(FwKey) shouldBe false
  }

  it should "flag nothing for a film with no known release date" in {
    watch(at("2026-10-23T09:00:00Z"), releases = Map.empty).awaitingFirstRating(FwKey) shouldBe false
  }

  it should "never flag a source the country does not refresh (Filmweb outside Poland)" in {
    val uk = Country.all.find(!_.filmwebEnabled).get
    watch(at("2026-10-23T09:00:00Z"), country = uk).awaitingFirstRating(FwKey) shouldBe false
  }

  // The schedule the reaper and every rating handler share: a quiet unrated film had backed
  // off to 32h, so a refresh early on its premiere day kept it waiting into the next.
  "RatingSchedule.dueWindow" should "make a backed-off, still-unrated film due again within the premiere interval" in {
    val cadence = new InMemoryRatingCadenceStore
    val checked = Instant.parse("2026-10-17T04:00:00Z")
    (0 until 12).foldLeft(checked)((t, _) => { cadence.record(FwKey, None, t); t.plusSeconds(8 * 3600) })
    val last = Instant.parse("2026-10-23T04:00:00Z")
    services.cadence.RatingCadence.intervalFor(cadence.statsFor(FwKey)) shouldBe 32.hours

    // Around the premiere: due again once the 4h premiere interval has run.
    val premiere = RatingSchedule.dueWindow(cadence, watch(at("2026-10-23T09:00:00Z")).awaitingFirstRating)
    premiere.isDue(FwKey, Some(last), last.plusSeconds(4 * 3600)) shouldBe true
    // Past the window: the plain 32h cadence, boundary for boundary.
    val after = RatingSchedule.dueWindow(cadence, watch(at("2026-10-31T09:00:00Z")).awaitingFirstRating)
    val plain = new DueWindow(32.hours)
    (1 to 40).map(h => last.plusSeconds(h * 3600L)).foreach { t =>
      after.isDue(FwKey, Some(last), t) shouldBe plain.isDue(FwKey, Some(last), t)
    }
    (1 to 40).exists(h => !after.isDue(FwKey, Some(last), last.plusSeconds(h * 3600L))) shouldBe true
  }
}
