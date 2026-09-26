package modules

import models.{Cinema, CinemaCityChain, CineworldChain, City, Country, RegalChain}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.MongoConnection
import services.cinemas.CinemaScraperCatalog
import services.metrics.PrometheusExposition
import services.movies.ScreeningTokens
import tools.{HttpFetch, ObjectGraph, SameThreadExecutionBudget}

import java.util.Locale

/**
 * One worker, one country — checked for EVERY country, over the whole wired graph.
 *
 * Country leakage has been fixed a dozen times, each time in the one component that
 * leaked: Cineworld's chain-wide detail enricher wired into every country's worker off the
 * global catalog (f4c7ac583), Poland's `LEK` voice-over token inherited by Germany and Spain
 * through a default (47cec8241), Polish TMDB copy in the UK corpus through a defaulted
 * language. This spec asks the question once for the whole composition root instead: boot
 * `WorkerWiring(country)` for each country and walk everything it built
 * ([[tools.ObjectGraph]]), so a component added tomorrow with a defaulted `Country` fails
 * here without anyone listing it.
 *
 * What each country's wiring must hold, and nothing else:
 *   - one `Country` — its own;
 *   - cities and cinemas of that country only (scrapers, detail enrichers, censuses,
 *     the /uptime client markers and source links);
 *   - its own language wherever a locale is held (TMDB, IMDb, cache country names) — no
 *     sibling deployment's language;
 *   - its own voice-over token;
 *   - `country="<its code>"` on every metric series it registers.
 *
 * The global catalog (`CinemaScraperCatalog`) is the one thing entered opaque: it indexes
 * every country's scrapers by design, and each consumer is expected to take its own
 * country's slice — which is what the rest of the walk checks. No currency is modelled
 * anywhere in the app, so there is none to check.
 */
class CountryIsolationMatrixSpec extends AnyFlatSpec with Matchers {

  /** A wiring over a disabled Mongo whose network leaf refuses every call. */
  private class IsolationProbe(c: Country) extends WorkerWiring(c, new SameThreadExecutionBudget) {
    override lazy val mongoConnection: MongoConnection =
      new MongoConnection(uri = None, dbName = settings.MongoDatabaseName("unused"), required = services.MongoRequirement.Optional)
    override protected def realHttpLeaf: HttpFetch = new HttpFetch {
      def get(url: String): String = throw new java.io.IOException(s"no network in this spec: $url")
      def post(url: String, body: String, contentType: String): String = get(url)
    }
    override protected lazy val filmwebFallbackIds: Map[Cinema, Int] = Map.empty

    /** Build EVERY member the wiring declares — not a hand-kept list of the ones someone
     *  thought of, which a component added tomorrow would not be on — and name any that could
     *  not be built (the network leaf refuses every call). */
    def forceBoot(): Seq[(String, Throwable)] = {
      val failed = ObjectGraph.forceLazyMembers(this)
      registerCacheMetrics()
      failed
    }
    /** Every cinema name this wiring tags into its /uptime at boot or on a fallback flip. */
    def taggedCinemas: Set[String] = clientMarkers.keySet ++ sourceUrls.keySet ++ filmwebOnlyCinemas
  }

  /** The network-level chain sources are not venues of any city; each belongs to one country. */
  private val ChainCountry: Map[Cinema, Country] =
    Map(CinemaCityChain -> Country.Poland, CineworldChain -> Country.UnitedKingdom, RegalChain -> Country.UnitedStates)

  private def ownCinemas(country: Country): Set[Cinema] =
    country.cities.flatMap(_.cinemas).toSet ++ ChainCountry.collect { case (c, `country`) => c }

  private def countryOf(cinema: Cinema): Option[Country] =
    City.forCinema(cinema).map(_.country).orElse(ChainCountry.get(cinema))

  /** A sibling deployment's language, i.e. one a locale in `country`'s graph must never be. */
  private def foreignLanguages(country: Country): Set[Locale] =
    Country.all.filterNot(_ == country).map(_.language).toSet - country.language

  private val CountryLabel = """country="([^"]*)"""".r

  Country.all.foreach { country =>
    val code = country.code

    s"The $code worker wiring" should "hold no other country, city or cinema anywhere in its graph" in {
      val wiring = new IsolationProbe(country)
      try {
        val unbuilt = wiring.forceBoot()
        withClue(s"members the walk cannot see because they failed to build: ${unbuilt.map { case (n, e) => s"$n: $e" }.mkString("; ")}\n") {
          unbuilt shouldBe empty
        }
        val graph = ObjectGraph.collect(wiring, opaque = _.isInstanceOf[CinemaScraperCatalog]) {
          case c: Country => c
          case c: City    => c
          case c: Cinema  => c
        }
        val strays = graph.collect {
          case (path, c: Country) if c != country                                   => s"$path -> country ${c.code}"
          case (path, c: City) if c.country != country                              => s"$path -> city ${c.slug} (${c.country.code})"
          case (path, c: Cinema) if !countryOf(c).contains(country)                 =>
            s"$path -> cinema ${c.displayName} (${countryOf(c).fold("no country")(_.code)})"
        }
        withClue(s"$code wiring reaches another country's values:\n${strays.take(40).mkString("\n")}\n") {
          strays shouldBe empty
        }
        // Positive control: the walk does reach this country's own scrapers.
        graph.map(_._2) should contain (country)
        wiring.cinemaScrapers should not be empty
      } finally wiring.stop()
    }

    it should "wire only its own country's cinemas as scrapers and detail enrichers" in {
      val wiring = new IsolationProbe(country)
      try {
        wiring.cinemaScrapers.map(_.cinema).filterNot(ownCinemas(country)) shouldBe empty
        wiring.detailEnrichers.map(_.cinema).filterNot(ownCinemas(country)) shouldBe empty
      } finally wiring.stop()
    }

    // Regression: the markers, source links and Filmweb-only set were built off the GLOBAL
    // catalog, so every worker tagged every country's venues (thousands of rows) into its
    // own /uptime tag store at boot, and each non-Polish worker resolved Poland's Filmweb
    // source links over HTTP to do it.
    it should "tag only its own country's cinemas into /uptime" in {
      val wiring = new IsolationProbe(country)
      try {
        val ownNames = ownCinemas(country).map(_.displayName)
        wiring.taggedCinemas should not be empty
        wiring.taggedCinemas.filterNot(ownNames).toSeq.sorted.take(20) shouldBe empty
      } finally wiring.stop()
    }

    it should "hold its own language and voice-over token, never a sibling's" in {
      val wiring = new IsolationProbe(country)
      try {
        wiring.forceBoot()
        wiring.tmdbClient.language shouldBe country.language
        wiring.screeningTokens.voiceover shouldBe country.voiceoverToken
        val held = ObjectGraph.collect(wiring, opaque = _.isInstanceOf[CinemaScraperCatalog]) {
          case l: Locale           => l
          case t: ScreeningTokens  => t.voiceover
        }
        val foreignLocales = held.collect { case (path, l: Locale) if foreignLanguages(country)(l) => s"$path -> ${l.toLanguageTag}" }
        withClue(s"$code wiring holds a sibling deployment's language:\n${foreignLocales.mkString("\n")}\n") {
          foreignLocales shouldBe empty
        }
        val tokens = held.collect { case (path, t: Option[?]) if t != country.voiceoverToken => s"$path -> $t" }
        withClue(s"$code wiring holds another country's voice-over token:\n${tokens.mkString("\n")}\n") {
          tokens shouldBe empty
        }
      } finally wiring.stop()
    }

    it should s"label every metric series it registers country=\"$code\"" in {
      val wiring = new IsolationProbe(country)
      try {
        wiring.forceBoot()
        // Drive one call through each phase chain so the per-call series exist too.
        scala.util.Try(wiring.httoFetch.get("https://cinema.example/listing"))
        scala.util.Try(wiring.enrichmentFetch.get("https://api.themoviedb.org/3/movie/1"))
        val labels = CountryLabel.findAllMatchIn(PrometheusExposition.render(wiring.workerMetrics.registry))
          .map(_.group(1)).toSet
        labels shouldBe Set(code)
      } finally wiring.stop()
    }

    it should "keep each of its cities in a time zone of its own" in {
      val zones = country.cities.map(_.zoneId.getId).toSet
      val foreign = Country.all.filterNot(_ == country).flatMap(_.cities.map(_.zoneId.getId)).toSet
      // Siblings share no zone: a Polish city ticking on London time, or a Spanish one on
      // Warsaw's, would roll "today" over at the wrong midnight for every showtime in it.
      withClue(s"$code cities on a zone another country uses: ")((zones intersect foreign) shouldBe empty)
    }
  }
}
