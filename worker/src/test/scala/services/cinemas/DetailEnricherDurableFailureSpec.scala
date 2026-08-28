package services.cinemas

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{DetailEnricher, DetailFetchOutcome}
import services.cinemas.pl._
import services.cinemas.uk.CineworldClient
import tools.{HttpFetch, HttpStatusException}
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * One contract, asserted across EVERY deferred-detail cinema: a detail page that
 * is permanently gone must reach the enrichment handler as `Gone`, and one that
 * merely failed must reach it as `Failed`.
 *
 * Why a family-wide spec rather than a case in each client's own: the difference
 * is not about any client's parsing, it is about whether the client SWALLOWS the
 * status on the way out. Every one of them folded a 404 into `None`, which the
 * handler cannot tell from a timeout — so it never stamped the film, and
 * `DueWindow.isDue` (unconditionally true with no stamp) had `DetailReaper`
 * re-enqueueing it every tick, forever. That livelock cost the live Cinema City
 * chain row ~90% failures on two withdrawn films. It could have started in any of
 * these clients; this spec is what keeps a new one from reintroducing it.
 */
class DetailEnricherDurableFailureSpec extends AnyFlatSpec with Matchers {

  /** Fails every fetch with `status`, so only the client's own error handling —
   *  not its parser — decides the outcome. */
  private class AlwaysFails(status: Int) extends HttpFetch {
    override def get(url: String): String = throw new HttpStatusException(status, "GET", url, None)
    override def post(url: String, body: String, contentType: String): String = get(url)
  }

  /** Every deferred-detail cinema, built against a fetch that always fails. The
   *  constructor args beyond `http` don't matter here — nothing is parsed. */
  private def enrichers(http: HttpFetch): Seq[(String, DetailEnricher)] = Seq(
    "Alternatywy"        -> new AlternatywyClient(http, titles = titleNormalizer),
    "Amondo"             -> new AmondoClient(http),
    "Bilety24Organizer"  -> new Bilety24OrganizerClient(http, "https://x/org", KinoApollo, titles = titleNormalizer),
    "CinemaCity"         -> new CinemaCityScraper(new CinemaCityClient(http, titles = titleNormalizer), "1081", CinemaCityKinepolis),
    "Cineworld"          -> new CineworldClient(http, "001", KinoApollo),
    "Cytadela"           -> new CytadelaClient(http),
    "Dcf"                -> new DcfClient(http),
    "Ekobilet"           -> new EkobiletClient(http, "slug", KinoApollo),
    "Falenica"           -> new FalenicaClient(http),
    "Iluzjon"            -> new IluzjonClient(http),
    "KinoApollo"         -> new KinoApolloClient(http, titles = titleNormalizer),
    "KinoBulgarska"      -> new KinoBulgarskaClient(http),
    "KinoFenomen"        -> new KinoFenomenClient(http),
    "KinoMuza"           -> new KinoMuzaClient(http, titles = titleNormalizer),
    "KinoPalacowe"       -> new KinoPalacoweClient(http, titles = titleNormalizer),
    "KinoParadox"        -> new KinoParadoxClient(http, KinoApollo),
    "KinoPodBaranami"    -> new KinoPodBaranamiClient(http, KinoApollo),
    "KinoSfinks"         -> new KinoSfinksClient(http, KinoApollo),
    "Kinomuzeum"         -> new KinomuzeumClient(http),
    "Kinoteka"           -> new KinotekaClient(http, titles = titleNormalizer),
    "Muranow"            -> new MuranowClient(http),
    "NoveKino"           -> new NoveKinoClient(http, "slug", KinoApollo),
    "NoweHoryzonty"      -> new NoweHoryzontyClient(http),
    "Pionier"            -> new PionierClient(http),
    "Rialto"             -> new RialtoClient(http),
    "Ujazdowski"         -> new UjazdowskiClient(http)
  )

  "every deferred-detail cinema" should "report a 404 detail page as Gone, so the handler stamps it" in {
    val failed = enrichers(new AlwaysFails(404)).collect {
      case (name, e) if e.fetchDetail("https://example.test/film") != DetailFetchOutcome.Gone(404) => name
    }
    withClue(s"these clients still swallow a durable 404 into an indistinguishable failure: ${failed.mkString(", ")} — ") {
      failed shouldBe empty
    }
  }

  it should "report a 410 detail page as Gone too" in {
    val failed = enrichers(new AlwaysFails(410)).collect {
      case (name, e) if e.fetchDetail("https://example.test/film") != DetailFetchOutcome.Gone(410) => name
    }
    failed shouldBe empty
  }

  // The other half of the split, and the reason this isn't just "stamp every
  // failure": a 503 describes the moment, so the film must stay un-stamped and be
  // retried on the next tick exactly as before.
  it should "report a 503 detail page as Failed, so it still retries next tick" in {
    val wrong = enrichers(new AlwaysFails(503)).collect {
      case (name, e) if e.fetchDetail("https://example.test/film") != DetailFetchOutcome.Failed => name
    }
    withClue(s"these clients wrongly treat a transient failure as permanent: ${wrong.mkString(", ")} — ") {
      wrong shouldBe empty
    }
  }

  it should "cover every DetailEnricher the catalogue can build" in {
    // Guards the list above against a new deferred-detail client being added and
    // silently skipped here — the spec is only worth its runtime if it is complete.
    val covered = enrichers(new AlwaysFails(404)).size
    covered shouldBe 26
  }
}
