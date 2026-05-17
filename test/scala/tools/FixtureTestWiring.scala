package tools

import clients.tools.FakeHttpFetch
import play.api.Mode
import play.api.mvc.ControllerComponents
import play.api.test.Helpers.stubControllerComponents
import services.cinemas.ScrapingAntClient
import services.movies.InMemoryMovieRepo

class FixtureTestWiring(val fixture: String) extends TestWiring {
  override lazy val httoFetch: HttpFetch = new FakeHttpFetch(fixture)
  override lazy val movieRepo = new InMemoryMovieRepo()
  override val controllerComponents: ControllerComponents = stubControllerComponents()
  override def environmentMode: Mode = Mode.Test

  // Bypass ScrapingAnt so MultikinoClient falls back to the injected
  // FakeHttpFetch and replays the recorded fixture. The base `TestWiring`
  // keeps ScrapingAnt routing wired — `ClientIntegrationSpec` shares that
  // wiring and needs ScrapingAnt to bypass Multikino's datacenter-IP
  // block when hitting the live API.
  override def multikinoScrapingAnt: Option[ScrapingAntClient] = None
}
