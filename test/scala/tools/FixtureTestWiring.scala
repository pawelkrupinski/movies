package tools

import clients.tools.FakeHttpFetch
import play.api.Mode
import play.api.mvc.ControllerComponents
import play.api.test.Helpers.stubControllerComponents
import services.movies.InMemoryMovieRepo

class FixtureTestWiring(val fixture: String) extends TestWiring {
  override lazy val httoFetch: HttpFetch = new FakeHttpFetch(fixture)
  // Production wires Multikino through its own session-handling fetch; in
  // fixture tests we want it to read from the same FakeHttpFetch as every
  // other client so the recorded `multikino/...` fixtures are served.
  override lazy val multikinoFetch: HttpFetch = httoFetch
  override lazy val movieRepo = new InMemoryMovieRepo()
  override val controllerComponents: ControllerComponents = stubControllerComponents()
  override def environmentMode: Mode = Mode.Test
}
