package services.movies

import models.Poznan
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.observations.ObservationStore
import tools.{FixtureTestWiring, ReadModelSnapshot, TestWiring}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.time.{Clock, LocalDateTime, ZoneOffset}

/**
 * The identity program's shadow capture must not change what the pipeline makes
 * (docs/design/identity-resolver.md, "Phase 1"). The whole recorded corpus, booted with capture
 * ON, must render byte-identically to the snapshots `FilmScheduleEndToEndSpec` pins with it OFF
 * — `expected-schedules.txt` and the read-model snapshot — while the store fills with every
 * listing, external lookup and venue detail the boot made.
 */
class ObservationCaptureEndToEndSpec extends AnyFlatSpec with Matchers {

  private val now = LocalDateTime.of(2026, 6, 8, 0, 0)

  private lazy val store = ObservationStore.inMemory(Clock.fixed(TestWiring.FixedInstant, ZoneOffset.UTC))

  private lazy val wiring: FixtureTestWiring = {
    val w = new FixtureTestWiring("08-06-2026") {
      override lazy val observationStore: Option[ObservationStore] = Some(store)
    }
    w.bootStartup()
    w
  }

  "shadow capture" should "leave the whole-corpus schedules byte-identical to the capture-off snapshot" in {
    val expected = new String(Files.readAllBytes(Paths.get("test/resources/fixtures/08-06-2026/expected-schedules.txt")),
      StandardCharsets.UTF_8)
    ScheduleCorpusText.of(wiring, Poznan, now) shouldBe expected
  }

  it should "leave the projected read model identical to the capture-off snapshot" in {
    // Up to film ids, exactly as FilmScheduleEndToEndSpec compares it: an id follows which
    // spelling the parallel scrape landed first, with or without capture.
    def modIds(json: String) = ReadModelSnapshot.render(ReadModelSnapshot.orderIndependent(ReadModelSnapshot.parse(json)))
    val actual   = ReadModelSnapshot.render(ReadModelSnapshot.capture(wiring.readModelRepository))
    val expected = new String(Files.readAllBytes(ReadModelSnapshot.DefaultPath), StandardCharsets.UTF_8)
    modIds(actual) shouldBe modIds(expected)
  }

  it should "have observed every scraped listing, the external lookups and the venue details the boot made" in {
    wiring
    val listings = store.currentListings()
    val scraped  = wiring.cinemaScrapers.flatMap(s => scala.util.Try(s.fetch()).toOption.toSeq.flatten.map(ListingKey.of(s.cinema, _)))
    withClue(s"${listings.size} listing observations for ${scraped.distinct.size} scraped listings\n") {
      listings.map(_.key).toSet shouldBe scraped.toSet
    }
    val lookups = store.currentLookups()
    lookups.map(_.query.host).toSet should contain allOf ("api.themoviedb.org", "caching.graphql.imdb.com", "www.filmweb.pl", "www.rottentomatoes.com")
    lookups.count(_.query.key.startsWith("DETAIL ")) should be > 0
  }
}
