package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.FilmwebClient

/**
 * Live test of FilmwebClient against the real (unauthenticated) Filmweb JSON API.
 * No API key needed — Filmweb's /api/v1 endpoints are open.
 */
class FilmwebIntegrationSpec extends AnyFlatSpec with Matchers {

  private val client = new FilmwebClient()

  "FilmwebClient.lookup" should "resolve a recent Polish-titled film to a URL + rating" in {
    val fw = client.lookup("Wartość sentymentalna", Some(2025))
    fw should not be empty
    fw.get.url should include ("filmweb.pl/film/")
    fw.get.url should include ("10058855")
    fw.get.rating.foreach(r => r should (be > 0.0 and be <= 10.0))
    info(s"  Filmweb → ${fw.get.url} (rate=${fw.get.rating.getOrElse("—")})")
  }

  it should "disambiguate by year when given one" in {
    // Pulp Fiction (1994) — Tarantino's classic. Year-disambiguation should pick
    // the correct one even though Filmweb has multiple "Pulp Fiction" titled hits.
    val fw = client.lookup("Pulp Fiction", Some(1994))
    fw should not be empty
    fw.get.rating.get should be > 7.0
  }

  it should "still return a URL when there's no year supplied" in {
    val fw = client.lookup("Wartość sentymentalna", None)
    fw should not be empty
  }
}
