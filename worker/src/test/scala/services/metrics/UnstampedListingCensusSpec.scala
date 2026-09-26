package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{CinemaShowing, CineworldChain, Country, KinoMuranow, Showtime, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{InMemoryScreeningsRepository, InMemorySlotsRepository, ListedShowtimes, ListingKey, UnreadableScreeningsRepository}

import java.time.LocalDateTime

/**
 * The gauge the identity migration's dual reads wait on: venue side rows that carry no
 * `listingKey`. The exemptions are the ones the write path leaves unstamped on purpose — an
 * enrichment slot, a chain's network detail slot, a retired venue — and a failed read publishes
 * nothing.
 */
class UnstampedListingCensusSpec extends AnyFlatSpec with Matchers {

  private val show  = Showtime(LocalDateTime.of(2099, 3, 1, 18, 0), None)
  private val belle = CinemaShowing(KinoMuranow, "belle").displayName
  private val slot  = SourceData(title = Some("Belle"), filmUrl = Some("https://muranow.pl/belle"))
  private val belleKey = ListingKey.ofSlotRow(belle, slot)

  "UnstampedListingCensus" should "count venue rows with no listingKey per collection, and never an exempt row" in {
    val screenings = new InMemoryScreeningsRepository
    val slots      = new InMemorySlotsRepository
    slots.upsertSlot("belle|2013", belle, slot)                                              // stamped by the write
    slots.upsertSlot("belle|2013", Tmdb.displayName, SourceData(title = Some("Belle")))     // enrichment: exempt
    screenings.upsertSlot("belle|2013", belle, ListedShowtimes(Seq(show), belleKey))              // stamped
    screenings.upsertSlot("dune|2021", belle, ListedShowtimes(Seq(show), None))              // UNSTAMPED
    screenings.upsertSlot("dune|2021", CineworldChain.displayName, ListedShowtimes(Seq(show), None))  // chain detail: exempt
    screenings.upsertSlot("dune|2021", "Kino That Closed␟dune", ListedShowtimes(Seq(show), None))    // retired venue: exempt

    val gauge = UnstampedListingCensus.gauge(new PrometheusRegistry())
    new UnstampedListingCensus(screenings, slots, gauge, Country.Poland).sample()

    gauge.labelValues("pl", "screenings").get() shouldBe 1.0
    gauge.labelValues("pl", "movie_slots").get() shouldBe 0.0
  }

  it should "publish nothing for a store it could not read" in {
    val gauge = UnstampedListingCensus.gauge(new PrometheusRegistry())
    gauge.labelValues("pl", "screenings").set(7.0)
    new UnstampedListingCensus(new UnreadableScreeningsRepository, new InMemorySlotsRepository, gauge, Country.Poland).sample()
    gauge.labelValues("pl", "screenings").get() shouldBe 7.0
  }
}
