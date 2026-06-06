package services.cinemas

import models._
import tools.HttpFetch

import java.time.{LocalDate, ZoneId}

/**
 * The single place that constructs every cinema's live scraper and groups them
 * by city. Extracted out of `modules.WorkerWiring` so the scraper graph can be
 * built without the worker's Mongo / event-bus / enrichment machinery — a
 * diagnostic such as `tools.FilmwebDiff` needs only `(http) => Seq[CinemaScraper]`,
 * not the whole write composition root.
 *
 * Takes the three seams the worker (and its fixture-replay test wiring) vary:
 *   - `http`     — the shared `HttpFetch` every cinema fetches through.
 *   - `mkFetch`  — Multikino's fetch path, passed by `WorkerWiring` (production
 *                  routes it through Zyte via `MultikinoClient.fetchFor`; the
 *                  fixture wiring overrides it back to `http`). A diagnostic that
 *                  doesn't care uses the secondary constructor below, which
 *                  defaults `mkFetch` to the Zyte-routed path.
 *   - `today`    — the date Helios bakes into its REST URLs.
 *
 * Returns RAW scrapers. `WorkerWiring` wraps each in a `RetryingCinemaScraper`
 * for production scrape ticks; a diagnostic uses them bare.
 */
class CinemaScraperCatalog(
  http:    HttpFetch,
  mkFetch: HttpFetch,
  today:   LocalDate
) {

  /** Diagnostic ctor: Multikino's fetch defaults to the Zyte-routed path derived
   *  from `http` (a clean body-derived default, not the old `null`-param
   *  workaround — Scala can't reference `http` in a primary-constructor default,
   *  but a secondary constructor can). `WorkerWiring` uses the primary ctor to
   *  inject its (possibly fixture-overridden) `multikinoFetch`. */
  def this(http: HttpFetch, today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))) =
    this(http, MultikinoClient.fetchFor(http), today)

  // Shared per-source helper clients the scrapers below reuse.
  val cinemaCityClient: CinemaCityClient = new CinemaCityClient(http)
  val kinoMuzaClient:   KinoMuzaClient   = new KinoMuzaClient(http)

  private val poznanScrapers: Seq[CinemaScraper] = Seq(
    new MultikinoClient(mkFetch),
    new CharlieMonroeClient(http),
    new KinoPalacoweClient(http),
    new HeliosClient(http, today = today),
    new CinemaCityScraper(cinemaCityClient, "1078", CinemaCityPoznanPlaza),
    new CinemaCityScraper(cinemaCityClient, "1081", CinemaCityKinepolis),
    kinoMuzaClient,
    new KinoBulgarskaClient(http),
    new KinoApolloClient(http),
    new RialtoClient(http),
  )

  private val wroclawScrapers: Seq[CinemaScraper] = Seq(
    new CinemaCityScraper(cinemaCityClient, "1097", CinemaCityWroclavia),
    new CinemaCityScraper(cinemaCityClient, "1067", CinemaCityKorona),
    new MultikinoClient(mkFetch, "0010", MultikinoPasazGrunwaldzki),
    new HeliosClient(http, HeliosNuxt.Magnolia, today),
    new HeliosClient(http, HeliosNuxt.AlejaBielany, today),
    new NoweHoryzontyClient(http),
    new DcfClient(http),
  )

  private val warszawaScrapers: Seq[CinemaScraper] = Seq(
    new CinemaCityScraper(cinemaCityClient, "1074", CinemaCityArkadia),
    new CinemaCityScraper(cinemaCityClient, "1061", CinemaCityBemowo),
    new CinemaCityScraper(cinemaCityClient, "1096", CinemaCityGaleriaPolnocna),
    new CinemaCityScraper(cinemaCityClient, "1069", CinemaCityJanki),
    new CinemaCityScraper(cinemaCityClient, "1070", CinemaCityMokotow),
    new CinemaCityScraper(cinemaCityClient, "1068", CinemaCityPromenada),
    new CinemaCityScraper(cinemaCityClient, "1060", CinemaCitySadyba),
    new MultikinoClient(mkFetch, "0013", MultikinoZloteTarasy),
    new MultikinoClient(mkFetch, "0040", MultikinoMlociny),
    new MultikinoClient(mkFetch, "0052", MultikinoReduta),
    new MultikinoClient(mkFetch, "0024", MultikinoTargowek),
    new MultikinoClient(mkFetch, "0025", MultikinoWolaPark),
    new HeliosClient(http, HeliosNuxt.BlueCity, today),
    new MuranowClient(http),
    new Bilety24Client(http, "https://kinoluna.bilety24.pl", KinoLuna),
    new Bilety24Client(http, "https://kinoelektronik.pl", KinoElektronik, "/"),
    new IluzjonClient(http),
    new KinoGramClient(http),
    new KinoKulturaClient(http),
    new AmondoClient(http),
    new BokClient(http, "kino-na-boku", KinoNaBoku),
    new BokClient(http, "kino-glebocka-66", KinoGlebocka66),
    new KinomuzeumClient(http),
    new SwitClient(http),
    new PromKepaClient(http),
    new FalenicaClient(http),
    new SdkClient(http),
    new NoveKinoClient(http, "atlantic", KinoAtlantic),
    new KinotekaClient(http),
    new UjazdowskiClient(http),
    new CytadelaClient(http),
  )

  private val krakowScrapers: Seq[CinemaScraper] = Seq(
    new CinemaCityScraper(cinemaCityClient, "1090", CinemaCityBonarka),
    new CinemaCityScraper(cinemaCityClient, "1076", CinemaCityKazimierz),
    new CinemaCityScraper(cinemaCityClient, "1064", CinemaCityZakopianka),
    new MultikinoClient(mkFetch, "0005", MultikinoKrakow),
    new KinoMikroClient(http, "Kino Mikro", KinoMikro),
    new KinoMikroClient(http, "Mikro Bronowice", MikroBronowice),
    new KinoSfinksClient(http, KinoSfinks),
  )

  private val lodzScrapers: Seq[CinemaScraper] = Seq(
    new CinemaCityScraper(cinemaCityClient, "1080", CinemaCityManufaktura),
    new MultikinoClient(mkFetch, "0023", MultikinoLodz),
    new HeliosClient(http, HeliosNuxt.Lodz, today),
    new CharlieClient(http, KinoCharlie),
  )

  private val trojmiastoScrapers: Seq[CinemaScraper] = Seq(
    new MultikinoClient(mkFetch, "0004", MultikinoGdansk),
    new HeliosClient(http, HeliosNuxt.Metropolia, today),
    new HeliosClient(http, HeliosNuxt.Forum, today),
    new HeliosClient(http, HeliosNuxt.Riviera, today),
    new KinoSpektrumClient(http, KinoSpektrum),
    new KinoKameralneClient(http, KinoKameralne),
    new KinoIkmClient(http, KinoIkm),
    new KinoMuzeumGdanskClient(http, KinoMuzeumGdansk),
    new KinoZakClient(http, KinoZak),
    new KinoPortClient(http, KinoPort),
  )

  /** Raw scrapers grouped by city slug — same slugs `City.slug` uses, so a
   *  caller can scope by city without re-spelling the membership. */
  val byCity: Map[String, Seq[CinemaScraper]] = Map(
    "poznan"     -> poznanScrapers,
    "wroclaw"    -> wroclawScrapers,
    "warszawa"   -> warszawaScrapers,
    "krakow"     -> krakowScrapers,
    "lodz"       -> lodzScrapers,
    "trojmiasto" -> trojmiastoScrapers,
  )

  /** Every raw scraper across every city, in city order. */
  val all: Seq[CinemaScraper] =
    City.all.flatMap(c => byCity.getOrElse(c.slug, Nil))
}
