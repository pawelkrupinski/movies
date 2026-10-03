package tools

import clients.tools.FakeHttpFetch

import services.movies.{CountingScreeningsRepository, CountingSlotsRepository, InMemoryMovieRepository, InMemoryScreeningsRepository, InMemorySlotsRepository}
import services.readmodel.{InMemoryReadModelRepository, ReadModelReader, ReadModelWriter}

class FixtureTestWiring(val fixture: String) extends TestWiring {
  override lazy val httoFetch: HttpFetch = new FakeHttpFetch(fixture)
  // Enrichment (TMDB/IMDb/RT/…) now draws from a SEPARATE phase-labelled chain in
  // production; in fixture replay it must replay from the SAME `FakeHttpFetch`, or
  // the metadata clients would fall through to the real network. Point it at the
  // one fake so every cinema-site AND enrichment call is served from the fixtures.
  override lazy val enrichmentFetch: HttpFetch = httoFetch
  // PRODUCTION'S STORAGE SHAPE, not the simplified one. A film's showtimes live in
  // `screenings` and its per-cinema slots in `movie_slots`, both keyed by film id, and the
  // `movies` row keeps neither once they land. Every end-to-end spec that goes through this
  // wiring therefore exercises the seam prod actually has.
  //
  // Wired here because a fake without it cannot express the bug class that cost the most in
  // 2026-07: a merge or a re-key is a RENAME, and a renamed film's showtimes and slots stay
  // filed under its OLD id — so anything that writes the winner and deletes the loser
  // destroys them. With everything inline a fold unions the records and carries them for
  // free, which is exactly why every merge/re-key spec stayed green while prod lost showtimes.
  //
  // Counted, because the in-memory stores ring their change listeners only on a REAL change:
  // an identical rewrite — which Mongo still pays for in full — is visible to a fixpoint
  // pass (`FixpointPass.ledger`) only as a write call reaching the store.
  override lazy val screeningsRepository = new CountingScreeningsRepository(new InMemoryScreeningsRepository)
  override lazy val slotsRepository      = new CountingSlotsRepository(new InMemorySlotsRepository)
  override lazy val movieRepository =
    new InMemoryMovieRepository(screenings = Some(screeningsRepository), slots = Some(slotsRepository), normalizer = titleNormalizer)

  // Mongo-free read model: the worker projects the scraped corpus into this
  // in-memory store, and the web's `WebReadModel` serves from it — the same
  // worker→read-model→web seam as production, minus Mongo. Specs build their
  // `MovieControllerService` from `webReadModel` (not the raw cache).
  override lazy val readModelRepository: ReadModelReader & ReadModelWriter = new InMemoryReadModelRepository()

  // The identity model's input, without Mongo: the scrape archive (what the old path last landed,
  // which the intake reads for a venue with no accepted listing yet) and the intake's own accepted
  // listings, in memory, keeping what the boot's scrapes publish.
  override lazy val scrapeArchive: services.scrapes.ScrapeArchiveRepository     = new services.scrapes.InMemoryScrapeArchiveRepository
  override lazy val acceptedListings: services.scrapes.ScrapeArchiveRepository  = new services.scrapes.InMemoryScrapeArchiveRepository

  // The fixture's capture day, parsed from a `dd-MM-yyyy` directory name (e.g.
  // "08-06-2026" → 2026-06-08). `None` for fixtures named for something else
  // ("multikino"), which aren't date-keyed. MUST be `lazy` — the super
  // constructor reads it via the `heliosToday` override (WorkerWiring builds
  // `cinemaScraperCatalog` during init) BEFORE this subclass's fields would
  // otherwise initialize; a plain `val` reads as null there (NPE).
  lazy val fixtureDate: Option[java.time.LocalDate] =
    scala.util.Try(
      java.time.LocalDate.parse(fixture, java.time.format.DateTimeFormatter.ofPattern("dd-MM-yyyy"))
    ).toOption

  // Pin Helios's REST date to the fixture's capture day. Helios bakes the date
  // window into its `/screening` + `/event` URLs; without this the live
  // `LocalDate.now` makes those URLs miss the recorded fixtures, dropping Helios
  // room/format enrichment and breaking the whole-corpus snapshot on every day
  // after capture.
  override protected def heliosToday: java.time.LocalDate =
    fixtureDate.getOrElse(super.heliosToday)

  // The CLIENT's notion of "today" (shared.js `dateBounds()`) for every page-test
  // render off this wiring — the in-JVM PageJsBehaviourSpec / PageSnapshotSpec
  // renders AND the FixtureServerMain (Playwright + mobile LocalServer) server,
  // each of which hands it to the template as `pinnedToday`. The rendered film cards
  // carry absolute fixture dates (June 2026), but the browser's real clock keeps
  // advancing, so a `?date=today`/`tomorrow`/`week` filter matches ZERO aged-out
  // cards a few weeks after capture — silently failing the day-filter JS specs.
  // `_sharedJsConfig` emits `window.KINOWO_PINNED_TODAY` ONLY when it is given one;
  // production passes none and keeps the real `new Date()` (correct for pages cached
  // across midnight).
  def pinnedToday: Option[java.time.LocalDate] = fixtureDate

  // Every cinema-egress route (Multikino, biletyna, ck105's Zyte seam, Flicks, Vue,
  // Odeon) replays from this same `FakeHttpFetch` without an override of its own:
  // `TestWiring` refuses their paid legs, and a route with neither a proxy nor a Zyte
  // leg IS its direct leg — `httoFetch`.

  /** Convenience: scrape every cinema once into the identity model's intake and project its films
   *  until a projection writes nothing — the rest production's projection interval reaches, as the
   *  venue pages and ids a projection's enrichment fetched are taken in by the next — then project
   *  the read model. After this returns the cache is in the shape production reaches a few
   *  projections after boot, so the rest of the test can assert against `movieCache.snapshot()`
   *  directly (the serving transform — `MovieControllerService.toSchedules` — is the web app's job
   *  now and is tested there). Projection is a one-shot reconcile + reload (no change-stream or
   *  scheduler) to keep the test deterministic and thread-free. */
  def bootStartup(): Unit = {
    bootCutover()
    Iterator.continually(projectIdentity()).take(FixtureTestWiring.SettleProjections).find(_.wroteNothing)
    readModelProjector.reconcile()
    webReadModel.reload()
  }

  /** Warm `webReadModel` the cheap way: load the checked-in read-model snapshot
   *  (the deterministic output of `bootStartup`) straight into
   *  the read-model repository, skipping scrape→resolve→project entirely.
   *  This is all the page-test servers (FixtureServerMain, PageSnapshotSpec,
   *  PageJsBehaviourSpec) need — they only ever read through `webReadModel`.
   *
   *  Falls back to the full `bootStartup` when the snapshot is absent, so a fresh
   *  fixture or a deleted snapshot is merely slow, never wrong. The snapshot's
   *  correctness is guarded by `FilmScheduleEndToEndSpec` (boots the real
   *  pipeline and asserts it equals the file). See `ReadModelSnapshot`. */
  def bootFromSnapshotOrPipeline(): Unit =
    if (ReadModelSnapshot.exists()) {
      ReadModelSnapshot.loadInto(readModelRepository, ReadModelSnapshot.read())
      webReadModel.reload()
    } else {
      System.err.println(
        "[FixtureTestWiring] no read-model snapshot — booting the full pipeline " +
          "(slow). Run FilmScheduleEndToEndSpec to (re)generate it.")
      bootStartup()
    }
}

object FixtureTestWiring {
  /** Projections after the boot's first that a fixture boot may take to reach rest. */
  val SettleProjections = 4
}
