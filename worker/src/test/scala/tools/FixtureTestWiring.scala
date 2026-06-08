package tools

import clients.tools.FakeHttpFetch
import services.events.MovieRecordCreated
import services.movies.InMemoryMovieRepo

class FixtureTestWiring(val fixture: String) extends TestWiring {
  override lazy val httoFetch: HttpFetch = new FakeHttpFetch(fixture)
  override lazy val movieRepo = new InMemoryMovieRepo()

  // Pin Helios's REST date to the fixture's capture day when the `fixture` dir
  // is named `dd-MM-yyyy` (e.g. "08-06-2026"). Helios bakes the date window into
  // its `/screening` + `/event` URLs; without this the live `LocalDate.now`
  // makes those URLs miss the recorded fixtures, dropping Helios room/format
  // enrichment and breaking the whole-corpus snapshot on every day after
  // capture. Fixtures named for something else ("multikino") aren't date-keyed,
  // so fall back to the real date for them.
  override protected def heliosToday: java.time.LocalDate =
    scala.util.Try(
      java.time.LocalDate.parse(fixture, java.time.format.DateTimeFormatter.ofPattern("dd-MM-yyyy"))
    ).getOrElse(super.heliosToday)

  // Route Multikino through the same `FakeHttpFetch` as every other cinema —
  // single override point. The base `TestWiring` inherits production's
  // `MultikinoClient.fetchFor(httoFetch)` so `ClientIntegrationSpec`'s
  // live-network smoke still goes through Zyte.
  override lazy val multikinoFetch: HttpFetch = httoFetch

  // Same single override point for biletyna (Kino Kameralne): replay the
  // fixture rather than hitting Zyte. Without this, CI — where ZYTE_API_KEY is
  // set — routes the fixture-replay scrape through real Zyte → biletyna,
  // breaking hermetic end-to-end specs (FilmScheduleEndToEndSpec).
  override lazy val biletynaFetch: HttpFetch = httoFetch

  /** Mirror of `ShowtimeCache.refreshOne`, sequenced so a test doesn't race
   *  the production scheduler. Catches per-scraper failures the same way
   *  production does so one missing-fixture cinema can't take down the
   *  whole tick. Shared across specs that exercise the fixture end-to-end. */
  def runOneScrapeTick(): Unit = {
    // TWO phases, on purpose. Production publishes MovieRecordCreated inline as
    // each cinema is recorded, so async enrichment for a film can start while a
    // LATER cinema in the same tick is still merging into that same cache row —
    // the enrichment write (e.g. TMDB cast) then races the scrape-merge,
    // making whole-corpus snapshots (PageSnapshotSpec, FilmScheduleEndToEndSpec)
    // non-deterministic. Tests don't need that interleaving: record EVERY
    // cinema first so each row reaches its final scraped shape, THEN publish all
    // the create events so enrichment runs against settled rows. Combined with
    // tmdbMaxRetries=0 and the full cascade drain, this makes the rendered
    // corpus deterministic run-to-run.
    val created = collection.mutable.ListBuffer.empty[MovieRecordCreated]
    cinemaScrapers.foreach { scraper =>
      val cinema = scraper.cinema
      try {
        val movies  = scraper.fetch()
        val touched = movieCache.recordCinemaScrape(cinema, movies)
        touched.foreach { case (cm, key, isNew) =>
          if (isNew)
            created += MovieRecordCreated(key.cleanTitle, key.year, cm.movie.originalTitle, if (cm.director.nonEmpty) Some(cm.director.mkString(", ")) else None)
        }
      } catch {
        case _: Exception => () // mirror ShowtimeCache.refreshOne's catch
      }
    }
    created.foreach(eventBus.publish)
  }

  /** Convenience: scrape every cinema once, drain the cascade, then run
   *  the daily `UnscreenedCleanup` pass. After this returns the cache is in
   *  the same shape it would be ~20s into production boot, so the rest of
   *  the test can assert against `movieCache.snapshot()` directly (the serving
   *  transform — `MovieControllerService.toSchedules` — is the web app's job
   *  now and is tested there). */
  def bootStartup(): Unit = {
    runOneScrapeTick()
    drainServices()
    unscreenedCleanup.removeUnscreened()
  }
}
