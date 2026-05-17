package tools

import clients.tools.FakeHttpFetch
import play.api.Mode
import play.api.mvc.ControllerComponents
import play.api.test.Helpers.stubControllerComponents
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
}
