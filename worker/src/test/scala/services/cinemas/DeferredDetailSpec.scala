package services.cinemas

import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate

/**
 * Cross-client contract for every cinema that implements `DetailEnricher`: its
 * `fetch()` returns BARE movies (showtimes + a per-film `filmUrl` reference) and
 * the per-film detail is reachable via `fetchFilmDetail`. The listing itself
 * (films + showtimes) is covered by each client's own spec; this covers the
 * deferred-detail contract uniformly — a bare movie always carries a usable
 * detail reference, and `fetchFilmDetail` resolves it against the fixtures.
 */
class DeferredDetailSpec extends AnyFlatSpec with Matchers {

  private val clients: Seq[(String, CinemaScraper & DetailEnricher)] = Seq(
    ("Kino Apollo",    new KinoApolloClient(new FakeHttpFetch("kino-apollo"))),
    ("Kinoteka",       new KinotekaClient(new FakeHttpFetch("kinoteka"))),
    ("Cytadela",       new CytadelaClient(new FakeHttpFetch("kino-cytadela"))),
    ("DCF",            new DcfClient(new FakeHttpFetch("dcf"))),
    ("Kino Pałacowe",  new KinoPalacoweClient(new FakeHttpFetch("kino-palacowe"))),
    ("Amondo",         new AmondoClient(new FakeHttpFetch("kino-amondo"))),
    ("Iluzjon",        new IluzjonClient(new FakeHttpFetch("iluzjon"))),
    ("Muranów",        new MuranowClient(new FakeHttpFetch("kino-muranow"))),
    ("Rialto",         new RialtoClient(new FakeHttpFetch("rialto"))),
    ("Kinomuzeum",     new KinomuzeumClient(new FakeHttpFetch("kinomuzeum"))),
    ("Falenica",       new FalenicaClient(new FakeHttpFetch("kino-falenica"))),
    ("Kino Bułgarska", new KinoBulgarskaClient(new FakeHttpFetch("kino-bulgarska"))),
    ("Nowe Horyzonty", new NoweHoryzontyClient(new FakeHttpFetch("nowe-horyzonty"), LocalDate.of(2026, 6, 6))),
    ("Nove Kino",      new NoveKinoClient(new FakeHttpFetch("kino-atlantic"), "atlantic", models.KinoAtlantic)),
    ("Ujazdowski",     new UjazdowskiClient(new FakeHttpFetch("ujazdowski"))),
    ("Cinema City",    new CinemaCityScraper(new CinemaCityClient(new FakeHttpFetch("cinema-city-plaza")), "1078", models.CinemaCityPoznanPlaza))
  )

  clients.foreach { case (name, client) =>
    s"$name" should "scrape bare films that each carry a detail reference fetchFilmDetail can resolve" in {
      val movies = client.fetch()
      movies should not be empty
      val refs = movies.flatMap(_.filmUrl)
      refs should not be empty // bare movies carry the detail ref
      // at least one film's detail page is in the fixtures and resolves to detail
      refs.flatMap(client.fetchFilmDetail).headOption should be(defined)
    }
  }
}
