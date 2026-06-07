package services.cinemas

import clients.tools.FakeHttpFetch
import models.CinemaMovie
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Cross-client contract for the deferred-detail conversion: for every cinema
 * that implements `DetailEnricher`, scraping with `deferDetail = true` must
 * preserve the listing (same films + showtimes as the inline path) while moving
 * the per-film detail behind `fetchFilmDetail`. This is the gate that a client's
 * bare/inline split is correct — the inline path is covered by each client's own
 * spec; this covers the new bare path uniformly.
 */
class DeferredDetailSpec extends AnyFlatSpec with Matchers {

  // (name, fixtureDir, factory(deferDetail) => CinemaScraper & DetailEnricher)
  private val clients: Seq[(String, Boolean => CinemaScraper & DetailEnricher)] = Seq(
    ("Kino Apollo",   d => new KinoApolloClient(new FakeHttpFetch("kino-apollo"), d)),
    ("Kinoteka",      d => new KinotekaClient(new FakeHttpFetch("kinoteka"), d)),
    ("Cytadela",      d => new CytadelaClient(new FakeHttpFetch("kino-cytadela"), d)),
    ("DCF",           d => new DcfClient(new FakeHttpFetch("dcf"), d)),
    ("Kino Pałacowe", d => new KinoPalacoweClient(new FakeHttpFetch("kino-palacowe"), d)),
    ("Amondo",        d => new AmondoClient(new FakeHttpFetch("kino-amondo"), d)),
    ("Iluzjon",       d => new IluzjonClient(new FakeHttpFetch("iluzjon"), deferDetail = d)),
    ("Muranów",       d => new MuranowClient(new FakeHttpFetch("kino-muranow"), d)),
    ("Rialto",        d => new RialtoClient(new FakeHttpFetch("rialto"), d)),
    ("Kinomuzeum",    d => new KinomuzeumClient(new FakeHttpFetch("kinomuzeum"), deferDetail = d)),
    ("Falenica",      d => new FalenicaClient(new FakeHttpFetch("kino-falenica"), d)),
    ("Kino Bułgarska",d => new KinoBulgarskaClient(new FakeHttpFetch("kino-bulgarska"), d))
  )

  private def titlesAndShowtimes(ms: Seq[CinemaMovie]) =
    (ms.map(_.movie.title).toSet, ms.flatMap(_.showtimes).size)

  clients.foreach { case (name, mk) =>
    s"$name deferred-detail" should "return the same listing (films + showtimes) as the inline path" in {
      titlesAndShowtimes(mk(true).fetch()) shouldBe titlesAndShowtimes(mk(false).fetch())
    }

    it should "carry a per-film detail reference that fetchFilmDetail can resolve" in {
      val client = mk(true)
      val refs   = client.fetch().flatMap(_.filmUrl)
      refs should not be empty // bare movies carry the detail ref
      // at least one film's detail page is in the fixtures and resolves to detail
      refs.flatMap(client.fetchFilmDetail).headOption should be(defined)
    }
  }
}
