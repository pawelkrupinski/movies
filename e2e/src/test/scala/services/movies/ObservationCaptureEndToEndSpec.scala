package services.movies

import models.Poznan
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{ShadowLookupRound, ShadowTick}
import services.observations.ObservationStore
import services.scrapes.{InMemoryScrapeArchiveRepository, ScrapeArchiveRepository}
import settings.ProcessConfiguration
import tools.{Env, FixtureTestWiring, HttpFetch, ReadModelSnapshot, TestWiring}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.time.{Clock, LocalDateTime, ZoneOffset}

/**
 * The identity program's shadow capture, shadow run and live lookup fill must not change what the
 * pipeline makes (docs/design/identity-resolver.md §8, §9a, §17). The whole recorded corpus, booted
 * with capture ON and the shadow run and its fill switched on — a tick, a fill round and a tick
 * after the boot — must render byte-identically to the
 * snapshots `FilmScheduleEndToEndSpec` pins with both OFF — `expected-schedules.txt` and the
 * read-model snapshot — while the store fills with every listing, identity lookup and venue
 * detail the boot made, and the shadow run resolves the corpus from those alone.
 */
class ObservationCaptureEndToEndSpec extends AnyFlatSpec with Matchers {

  private val now = LocalDateTime.of(2026, 6, 8, 0, 0)

  private lazy val store = ObservationStore.inMemory(Clock.fixed(TestWiring.FixedInstant, ZoneOffset.UTC))

  private val requests = new java.util.concurrent.atomic.AtomicLong()

  private lazy val shadow: (FixtureTestWiring, ShadowTick, Long) = {
    val w = new FixtureTestWiring("08-06-2026") {
      override lazy val configuration: ProcessConfiguration =
        new ProcessConfiguration(Env.of("KINOWO_IDENTITY_SHADOW" -> "true", "KINOWO_IDENTITY_SHADOW_LOOKUPS" -> "true"))
      // The fill paces its live asks in real time; the replay need not wait.
      override protected def shadowLookupSleep: Long => Unit = _ => ()
      override lazy val observationStore: Option[ObservationStore] = Some(store)
      // The fixture wiring's archive is Mongo's, disabled here: an in-memory one keeps the scrapes
      // the shadow run reads its listing set from.
      override lazy val scrapeArchive: ScrapeArchiveRepository = new InMemoryScrapeArchiveRepository
      // Every request the wiring makes, counted — the fixture replay underneath is unchanged.
      override lazy val httoFetch: HttpFetch = new HttpFetch {
        private val replay = new clients.tools.FakeHttpFetch(fixture)
        override def get(url: String): String = { requests.incrementAndGet(); replay.get(url) }
        override def get(url: String, headers: Map[String, String]): String = { requests.incrementAndGet(); replay.get(url, headers) }
        override def getBytes(url: String): Array[Byte] = { requests.incrementAndGet(); replay.getBytes(url) }
        override def post(url: String, body: String, contentType: String): String = { requests.incrementAndGet(); replay.post(url, body, contentType) }
      }
    }
    w.bootStartup()
    val before = requests.get()
    val tick   = w.shadowIdentityReaper.getOrElse(fail("the shadow run is not wired")).tick()
    (w, tick, requests.get() - before)
  }
  private def wiring: FixtureTestWiring = shadow._1

  /** A live lookup fill round after the tick, then the next tick over what it filed — the whole
   *  shadow cycle ran before the snapshots below are compared. */
  private lazy val filled: (ShadowLookupRound, ShadowTick) = {
    val w     = wiring
    val round = w.shadowLookupFill.getOrElse(fail("the lookup fill is not wired")).round()
    (round, w.shadowIdentityReaper.get.tick())
  }

  "shadow capture" should "leave the whole-corpus schedules byte-identical to the capture-off snapshot" in {
    val _ = filled
    val expected = new String(Files.readAllBytes(Paths.get("test/resources/fixtures/08-06-2026/expected-schedules.txt")),
      StandardCharsets.UTF_8)
    ScheduleCorpusText.of(wiring, Poznan, now) shouldBe expected
  }

  it should "leave the projected read model identical to the capture-off snapshot" in {
    val _ = filled
    // Up to film ids, exactly as FilmScheduleEndToEndSpec compares it: an id follows which
    // spelling the parallel scrape landed first, with or without capture.
    def modIds(json: String) = ReadModelSnapshot.render(ReadModelSnapshot.orderIndependent(ReadModelSnapshot.parse(json)))
    val actual   = ReadModelSnapshot.render(ReadModelSnapshot.capture(wiring.readModelRepository))
    val expected = new String(Files.readAllBytes(ReadModelSnapshot.DefaultPath), StandardCharsets.UTF_8)
    modIds(actual) shouldBe modIds(expected)
  }

  it should "have observed every scraped listing, the identity lookups and the venue details the boot made" in {
    wiring
    val listings = store.currentListings()
    val scraped  = wiring.cinemaScrapers.flatMap(s => scala.util.Try(s.fetch()).toOption.toSeq.flatten.map(ListingKey.of(s.cinema, _)))
    withClue(s"${listings.size} listing observations for ${scraped.distinct.size} scraped listings\n") {
      listings.map(_.key).toSet shouldBe scraped.toSet
    }
    val lookups = store.currentLookups()
    // The resolver's lookups only: the TMDB client's and the venues' details — no rating page.
    val (details, external) = lookups.partition(_.query.key.startsWith("DETAIL "))
    external.map(_.query.host).toSet shouldBe Set("api.themoviedb.org")
    details.size should be > 0
  }

  it should "have resolved the corpus in shadow from the observations alone, without a request" in {
    val (_, tick, requests) = shadow
    requests shouldBe 0
    tick.crossings shouldBe 0
    val run = tick.run.getOrElse(fail("no shadow run"))
    info(s"shadow run over the capture: ${tick.listings} listings → ${run.clusters.size} clusters " +
      s"(${tick.films.toSeq.sortBy(_._1.ordinal).map { case (r, n) => s"${r.label} $n" }.mkString(", ")}), " +
      s"${run.clusters.count(_.decision.film.isDefined)} matched; ${tick.gaps} lookups the capture never observed")
    tick.listings should be > 0
    run.clusters.map(_.decision.members.size).sum shouldBe tick.listings
  }

  it should "have filled the shadow's gaps with live asks into the store alone, within the fill's allowance" in {
    val (round, next) = filled
    val (_, first, _) = shadow
    info(s"fill round: asked ${round.asked} (${round.answered} answered, ${round.failed} failed), deferred ${round.deferred}; " +
      s"shadow gaps ${first.gaps} → ${next.gaps}; matched clusters ${first.run.fold(0)(_.clusters.count(_.decision.film.isDefined))} → " +
      s"${next.run.fold(0)(_.clusters.count(_.decision.film.isDefined))}; the first tick's gaps by kind (every one a question the " +
      s"pipeline, captured from an empty store, never asked): ${first.gapsByKind.toSeq.sortBy(-_._2).mkString(", ")}")
    round.asked should be > 0
    round.asked should be <= round.rate.allowanceOver(wiring.identityShadowInterval.value)
    next.gaps should be < first.gaps
  }
}
