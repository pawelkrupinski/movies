package tools

import clients.tools.FakeHttpFetch
import play.api.Mode
import play.api.mvc.ControllerComponents
import play.api.test.Helpers.stubControllerComponents
import services.events.MovieRecordCreated
import services.movies.InMemoryMovieRepo

class FixtureTestWiring(val fixture: String) extends TestWiring {
  override lazy val httoFetch: HttpFetch = new FakeHttpFetch(fixture)
  override lazy val movieRepo = new InMemoryMovieRepo()
  override val controllerComponents: ControllerComponents = stubControllerComponents()
  override def environmentMode: Mode = Mode.Test

  // Route Multikino through the same `FakeHttpFetch` as every other cinema —
  // single override point. The base `TestWiring` inherits production's
  // `MultikinoClient.fetchFor(httoFetch)` so `ClientIntegrationSpec`'s
  // live-network smoke still goes through ScrapingAnt and isn't blocked
  // by Multikino's datacenter-IP guard.
  override lazy val multikinoFetch: HttpFetch = httoFetch

  /** Mirror of `ShowtimeCache.refreshOne`, sequenced so a test doesn't race
   *  the production scheduler. Catches per-scraper failures the same way
   *  production does so one missing-fixture cinema can't take down the
   *  whole tick. Shared across specs that exercise the fixture end-to-end. */
  def runOneScrapeTick(): Unit = {
    cinemaScrapers.foreach { scraper =>
      val cinema = scraper.cinema
      try {
        val movies = scraper.fetch()
        val touched = movieCache.recordCinemaScrape(cinema, movies)
        touched.foreach { case (cm, key, isNew) =>
          if (isNew)
            eventBus.publish(MovieRecordCreated(key.cleanTitle, key.year, cm.movie.originalTitle, cm.director))
        }
      } catch {
        case _: Exception => () // mirror ShowtimeCache.refreshOne's catch
      }
    }
  }

  /** Convenience: scrape every cinema once, drain the cascade, then run
   *  the daily `UnscreenedCleanup` pass. After this returns the cache is in
   *  the same shape it would be ~20s into production boot, so the rest of
   *  the test can assert against `movieControllerService.toSchedules(now)`
   *  / `.toCinemaSchedules(now)` / `movieCache.snapshot()` directly. */
  def bootStartup(): Unit = {
    runOneScrapeTick()
    drainServices()
    unscreenedCleanup.removeUnscreened()
  }
}
