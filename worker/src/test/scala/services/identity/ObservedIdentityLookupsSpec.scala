package services.identity

import clients.TmdbClient
import clients.tools.FakeHttpFetch
import models.{Cinema, KinoMuza, Source}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{DetailEnricher, FilmDetail}
import services.movies.ListingKey
import services.observations.{LookupAnswer, LookupQuery, ObservationStore, ObservingDetailEnricher, ObservingHttpFetch}
import tools.{HttpStatusException, TestWiring}

import java.time.{Clock, ZoneOffset}

/** The shadow run's lookups answer from the observation store ALONE: exactly what the pipeline's
 *  observed calls received, a gap for anything never observed or observed only as a failed read,
 *  and no request to any service. */
class ObservedIdentityLookupsSpec extends AnyFlatSpec with Matchers {

  private def store() = ObservationStore.inMemory(Clock.fixed(TestWiring.FixedInstant, ZoneOffset.UTC))

  "a lookup the pipeline's observed client made" should "be answered from the store exactly as the service answered it" in {
    val observations = store()
    // The pipeline's client, observed — on the recorded tree, with its own key.
    val pipeline = new TmdbIdentityLookups(new TmdbClient(new ObservingHttpFetch(new FakeHttpFetch("08-06-2026", strict = true), observations),
      apiKey = Some(settings.TmdbApiKey("the-pipelines-key")), retrySleep = (_: Long) => ()), Nil)
    val film = pipeline.film(1018)
    film.toOption.flatten shouldBe defined

    // The shadow run's, over the store alone: another key (credentials are masked in the query).
    val (shadow, gaps) = ObservedIdentityLookups.over(observations, new TmdbClient(_, apiKey = Some(settings.TmdbApiKey("the-shadows-key"))), Nil)
    shadow.film(1018) shouldBe film
    gaps.total shouldBe 0
  }

  "a lookup never observed" should "be Unknown, and counted — never an empty answer" in {
    val (shadow, gaps) = ObservedIdentityLookups.over(store(), new TmdbClient(_, apiKey = Some(settings.TmdbApiKey("k"))), Nil)
    shadow.film(1018) shouldBe Answer.Unknown
    shadow.candidates(CandidateQuery.Title("Lalka")) shouldBe Answer.Unknown
    gaps.total should be >= 2L
  }

  "the observed fetch" should "answer a body, rethrow a definitive failure, and treat a failed read as a gap" in {
    val observations = store()
    val (ok, gone, busy) = ("https://api.example/ok?api_key=a", "https://api.example/gone", "https://api.example/busy")
    observations.observeLookup(LookupQuery.of("GET", ok), LookupAnswer.Body("{\"ok\":true}"))
    observations.observeLookup(LookupQuery.of("GET", gone), LookupAnswer.Failed(Some(404), "GET", "HTTP 404"))
    observations.observeLookup(LookupQuery.of("GET", busy), LookupAnswer.Failed(Some(503), "GET", "HTTP 503"))
    val gaps  = new ObservationGaps
    val fetch = new ObservedHttpFetch(observations, gaps)

    fetch.get("https://api.example/ok?api_key=b") shouldBe "{\"ok\":true}"
    intercept[HttpStatusException](fetch.get(gone)).code shouldBe 404
    intercept[ObservationGap](fetch.get(busy))
    intercept[ObservationGap](fetch.post(ok, "{}", "application/json"))
    gaps.total shouldBe 2
  }

  "a venue's detail" should "be the observed one, and a gap when the venue was never asked" in {
    val observations = store()
    val detail = FilmDetail(director = Seq("Maciej Kawalski"), releaseYear = Some(2026), runtimeMinutes = Some(150))
    val venue  = new DetailEnricher {
      def cinema: Cinema                              = KinoMuza
      def detailGroup: String                         = "muza"
      override def detailTarget: Source               = KinoMuza
      def fetchFilmDetail(ref: String): Option[FilmDetail] = Option.when(ref.endsWith("/lalka"))(detail)
    }
    new ObservingDetailEnricher(venue, observations).fetchFilmDetail("https://kinomuza.pl/lalka")

    val (shadow, gaps) = ObservedIdentityLookups.over(observations, new TmdbClient(_, apiKey = None), Seq(venue))
    def listing(page: String) = Listing(KinoMuza, ListingKey.Native(KinoMuza.displayName, page, "Lalka"), "Lalka", "Lalka", "Lalka",
      None, Nil, None, Some(page), None)
    shadow.detail(listing("https://kinomuza.pl/lalka")) shouldBe
      Answer.Known(Some(DetailFacts(Some(2026), Seq("Maciej Kawalski"), Some(150), None)))
    shadow.detail(listing("https://kinomuza.pl/inna")) shouldBe Answer.Unknown
    gaps.total shouldBe 1
  }
}
