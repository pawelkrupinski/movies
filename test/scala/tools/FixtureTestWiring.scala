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
}
