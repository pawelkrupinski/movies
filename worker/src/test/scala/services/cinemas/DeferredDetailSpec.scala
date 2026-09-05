package services.cinemas

import org.scalatest.matchers.should.Matchers
import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.common.{CinemaScraper, DetailEnricher}
import services.cinemas.pl._

import java.time.LocalDate
import services.movies.SingleCountryNormalizer.titleNormalizer

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
    ("Kino Apollo",    new KinoApolloClient(new FakeHttpFetch("kino-apollo"), titles = titleNormalizer)),
    ("Kinoteka",       new KinotekaClient(new FakeHttpFetch("kinoteka"), titles = titleNormalizer)),
    ("Cytadela",       new CytadelaClient(new FakeHttpFetch("kino-cytadela"))),
    ("DCF",            new DcfClient(new FakeHttpFetch("dcf"))),
    ("Kino Pałacowe",  new KinoPalacoweClient(new FakeHttpFetch("kino-palacowe"), titles = titleNormalizer)),
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
    ("Cinema City",    new CinemaCityScraper(new CinemaCityClient(new FakeHttpFetch("cinema-city-plaza"), titles = titleNormalizer), "1078", models.CinemaCityPoznanPlaza))
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

  /** Every `DetailEnricher` that opts OUT of deferral, with the fixture its
   *  listing and detail pages are recorded under. */
  private val optOutOfDeferral: Seq[(String, CinemaScraper & DetailEnricher)] = Seq(
    ("Alternatywy",       new AlternatywyClient(new FakeHttpFetch("alternatywy"),
                            today = LocalDate.of(2026, 6, 7), titles = titleNormalizer)),
    ("Ekobilet",          new EkobiletClient(new FakeHttpFetch("kino-meduza"), "opolskielamy",
                            models.KinoMeduza, today = LocalDate.of(2026, 6, 8))),
    ("Kino Pod Baranami", new KinoPodBaranamiClient(new FakeHttpFetch("kino-pod-baranami"),
                            models.KinoPodBaranami, LocalDate.of(2026, 6, 7))),
    ("Kino Paradox",      new KinoParadoxClient(new FakeHttpFetch("kino-paradox"), models.KinoParadox)),
    ("Kino Muza",         new KinoMuzaClient(new FakeHttpFetch("kino-muza"), titles = titleNormalizer))
  )

  // `defersTmdbResolution = false` says "resolve this row from its listing, don't
  // wait for the detail page". That is only safe when waiting would GAIN nothing:
  // either the listing already carries a TMDB-identity hint, or the detail carries
  // none. Get it wrong and the row resolves on its title alone while the director
  // and year that would have disambiguated it sit on a page fetched moments later —
  // and by then `settleResolved` has stamped the guess's year into the row's key,
  // which is what made prod's five mis-resolved films permanent.
  //
  // Alternatywy was exactly that: its own comment read "the listing carries only
  // the title + poster … the detail page adds synopsis, director, and production
  // countries + year", and it opted out anyway.
  optOutOfDeferral.foreach { case (name, client) =>
    s"$name" should "only skip TMDB deferral when waiting for its detail would gain nothing" in {
      withClue(s"$name sets defersTmdbResolution = false: ") { client.defersTmdbResolution shouldBe false }
      val movies = client.fetch()
      movies should not be empty
      // What `resolveTmdbId` can actually search on: an original title, a director,
      // or a year. Kino Pod Baranami publishes only the first of those on its
      // listing and that is enough — the search candidates include it.
      val listingHint = movies.exists(m =>
        m.director.nonEmpty || m.movie.releaseYear.isDefined || m.movie.originalTitle.nonEmpty)
      val detailHint  = movies.flatMap(_.filmUrl).flatMap(client.fetchFilmDetail)
        .exists(d => d.director.nonEmpty || d.releaseYear.isDefined || d.originalTitle.nonEmpty)
      info(s"$name listing hints: director/year/originalTitle present = $listingHint; detail hints = $detailHint")
      withClue(s"$name resolves from its listing, but only its DETAIL carries a director/year — " +
               "it should defer TMDB resolution until that detail lands: ") {
        (listingHint || !detailHint) shouldBe true
      }
    }
  }
}
