package services.metrics

import services.movies.ListedShowtimes

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{Country, KinoMuranow, Showtime, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{InMemoryScreeningsRepository, InMemorySlotsRepository, VenueRoster}

import java.time.{Clock, Instant, LocalDateTime, ZoneOffset}

/**
 * Locks the watchdog over side rows a venue left behind when it was dropped from the roster.
 *
 * "Kino Etiuda OBK" left the PL roster and 3 films with 8 future showtimes stayed filed under it
 * in `screenings`/`movie_slots`: no sweep removes them (the stranded-row sweep convicts by FILM,
 * and the film is alive), the read path ignores them, and every census reading the side
 * collections raw counted them. What this pins is that such rows are counted per collection,
 * their FUTURE showtimes separately, a roster venue's rows never, and a read that failed never
 * publishes a count it did not make.
 */
class RetiredVenueCensusSpec extends AnyFlatSpec with Matchers {

  private val now     = Instant.parse("2026-09-23T10:00:00Z")   // 12:00 in Warsaw
  private val retired = "Kino Etiuda OBK"

  private def at(local: String) = Showtime(LocalDateTime.parse(local), None)

  private def census(screenings: InMemoryScreeningsRepository, slots: InMemorySlotsRepository,
                     roster: Set[String] = VenueRoster.venuesOf(Country.Poland)) = {
    val registry = new PrometheusRegistry()
    val (rows, future) = RetiredVenueCensus.gauges(registry)
    (new RetiredVenueCensus(screenings, slots, roster, rows, future, Country.Poland, Clock.fixed(now, ZoneOffset.UTC)),
      rows, future)
  }

  "RetiredVenueCensus" should "count the side rows filed under a venue the roster no longer lists, and their future showtimes" in {
    val screenings = new InMemoryScreeningsRepository
    val slots      = new InMemorySlotsRepository
    // A live venue: never counted, however many rows.
    screenings.upsertSlot("foo|2026", s"${KinoMuranow.displayName}␟foo", ListedShowtimes(Seq(at("2026-09-24T18:00")), None))
    slots.upsertSlot("foo|2026", s"${KinoMuranow.displayName}␟foo", SourceData(title = Some("Foo")))
    // The retired venue: two films, three showtimes of which two are still ahead (Warsaw time).
    screenings.upsertSlot("foo|2026", s"$retired␟foo", ListedShowtimes(Seq(at("2026-09-22T18:00"), at("2026-09-24T18:00")), None))
    screenings.upsertSlot("bar|2026", s"$retired␟bar", ListedShowtimes(Seq(at("2026-09-25T20:00")), None))
    slots.upsertSlot("foo|2026", s"$retired␟foo", SourceData(title = Some("Foo")))

    val (c, rows, future) = census(screenings, slots)
    c.sample()

    rows.labelValues("pl", "screenings").get() shouldBe 2.0
    rows.labelValues("pl", "movie_slots").get() shouldBe 1.0
    future.labelValues("pl").get() shouldBe 2.0
  }

  // "Kino Etiuda" is live while "Kino Etiuda OBK" is not: a prefix match would read one as the
  // other and either hide the retired rows or convict the live ones.
  it should "compare a row's venue to the roster by equality, never by prefix" in {
    val screenings = new InMemoryScreeningsRepository
    screenings.upsertSlot("foo|2026", s"${KinoMuranow.displayName} OBK␟foo", ListedShowtimes(Seq(at("2026-09-24T18:00")), None))
    screenings.upsertSlot("foo|2026", KinoMuranow.displayName, ListedShowtimes(Seq(at("2026-09-24T18:00")), None))   // a legacy bare key

    val (c, rows, _) = census(screenings, new InMemorySlotsRepository)
    c.sample()

    rows.labelValues("pl", "screenings").get() shouldBe 1.0
  }

  it should "read zero on a clean store" in {
    val (c, rows, future) = census(new InMemoryScreeningsRepository, new InMemorySlotsRepository)
    c.sample()
    rows.labelValues("pl", "screenings").get() shouldBe 0.0
    rows.labelValues("pl", "movie_slots").get() shouldBe 0.0
    future.labelValues("pl").get() shouldBe 0.0
  }

  // A read that failed is not an empty store, and an empty roster is a roster that failed to
  // load, not a country whose every venue retired: neither may publish a count.
  it should "publish nothing when a read failed or the roster is empty" in {
    val screenings = new InMemoryScreeningsRepository {
      override def rowIdsChecked(): (Set[String], Boolean) = (Set.empty, false)
    }
    val slots = new InMemorySlotsRepository
    slots.upsertSlot("foo|2026", s"$retired␟foo", SourceData(title = Some("Foo")))

    val (c, rows, future) = census(screenings, slots)
    rows.labelValues("pl", "screenings").set(7.0)   // the last good reading
    future.labelValues("pl").set(3.0)
    c.sample()
    rows.labelValues("pl", "screenings").get() shouldBe 7.0
    future.labelValues("pl").get() shouldBe 3.0
    rows.labelValues("pl", "movie_slots").get() shouldBe 1.0   // the store that did read still reports

    val (empty, emptyRows, _) = census(new InMemoryScreeningsRepository, slots, roster = Set.empty)
    emptyRows.labelValues("pl", "movie_slots").set(5.0)
    empty.sample()
    emptyRows.labelValues("pl", "movie_slots").get() shouldBe 5.0
  }
}
