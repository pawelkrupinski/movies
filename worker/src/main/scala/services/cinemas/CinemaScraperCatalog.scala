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
  bnFetch: HttpFetch,
  today:   LocalDate
) {

  /** Diagnostic ctor: the Zyte-routed fetches (Multikino's API, biletyna's venue
   *  pages) default to the path derived from `http` (a clean body-derived
   *  default, not the old `null`-param workaround — Scala can't reference `http`
   *  in a primary-constructor default, but a secondary constructor can).
   *  `WorkerWiring` uses the primary ctor to inject its (possibly
   *  fixture-overridden) `multikinoFetch` / `biletynaFetch`. */
  def this(http: HttpFetch, today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))) =
    this(http, MultikinoClient.fetchFor(http), ZyteFallback.fetchFor(http), today)

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
    new NoveKinoClient(http, "wisla", KinoWisla),
  )

  private val krakowScrapers: Seq[CinemaScraper] = Seq(
    new CinemaCityScraper(cinemaCityClient, "1090", CinemaCityBonarka),
    new CinemaCityScraper(cinemaCityClient, "1076", CinemaCityKazimierz),
    new CinemaCityScraper(cinemaCityClient, "1064", CinemaCityZakopianka),
    new MultikinoClient(mkFetch, "0005", MultikinoKrakow),
    new KinoMikroClient(http, "Kino Mikro", KinoMikro),
    new KinoMikroClient(http, "Mikro Bronowice", MikroBronowice),
    new KinoSfinksClient(http, KinoSfinks),
    new KinoPodBaranamiClient(http, KinoPodBaranami, today),
    new KinoKijowClient(http, KinoKijow, today),
    new KinoKikaClient(http, KinoKika),
    new KinoAgrafkaClient(http, KinoAgrafka),
    new KinoParadoxClient(http, KinoParadox),
  )

  private val lodzScrapers: Seq[CinemaScraper] = Seq(
    new CinemaCityScraper(cinemaCityClient, "1080", CinemaCityManufaktura),
    new MultikinoClient(mkFetch, "0023", MultikinoLodz),
    new HeliosClient(http, HeliosNuxt.Lodz, today),
    new CharlieClient(http, KinoCharlie),
    new KinematografLodzClient(http, KinematografLodz, today),
    new NckfClient(http, Nckf, today),
  )

  private val katowiceScrapers: Seq[CinemaScraper] = Seq(
    new CinemaCityScraper(cinemaCityClient, "1065", CinemaCityPunkt44),
    new CinemaCityScraper(cinemaCityClient, "1079", CinemaCitySilesia),
    new MultikinoClient(mkFetch, "0035", MultikinoKatowice),
    new HeliosClient(http, HeliosNuxt.Katowice, today),
    // Silesia Film's art-house pair, both Bilety24-hosted (same platform as
    // Kino Luna / Elektronik): listing at `/repertuar/` linking per-film
    // `/wydarzenie/?id=N` pages, so they reuse the shared Bilety24Client.
    // (Kinoteatr Rialto, the third Silesia Film venue, programmes only
    // concerts/theatre right now — no film repertoire — so it's not wired.)
    new Bilety24Client(http, "https://kinokosmos.bilety24.pl", KinoKosmos),
    new Bilety24Client(http, "https://swiatowid-katowice.bilety24.pl", KinoSwiatowid),
  )

  private val szczecinScrapers: Seq[CinemaScraper] = Seq(
    new HeliosClient(http, HeliosNuxt.Szczecin, today),
    new MultikinoClient(mkFetch, "0007", MultikinoSzczecin),
    new PionierClient(http, KinoPionier),
    new HeliosClient(http, HeliosNuxt.SzczecinOutletPark, today),
    new KinoZamekClient(http, KinoZamekSzczecin, today),
  )

  private val bialystokScrapers: Seq[CinemaScraper] = Seq(
    new HeliosClient(http, HeliosNuxt.Alfa, today),
    new HeliosClient(http, HeliosNuxt.Biala, today),
    new HeliosClient(http, HeliosNuxt.Jurowiecka, today),
    new KinoForumClient(http, today),
  )

  private val trojmiastoScrapers: Seq[CinemaScraper] = Seq(
    new MultikinoClient(mkFetch, "0004", MultikinoGdansk),
    new HeliosClient(http, HeliosNuxt.Metropolia, today),
    new HeliosClient(http, HeliosNuxt.Forum, today),
    new HeliosClient(http, HeliosNuxt.Riviera, today),
    new KinoSpektrumClient(http, KinoSpektrum),
    // biletyna.pl 403s our datacenter IP, so route through `bnFetch` — Zyte's
    // residential egress in production, the fixture fake in tests. See
    // WorkerWiring.biletynaFetch / ZyteFallback.
    new KinoKameralneClient(bnFetch, KinoKameralne),
    new KinoIkmClient(http, KinoIkm),
    new KinoMuzeumGdanskClient(http, KinoMuzeumGdansk),
    new KinoZakClient(http, KinoZak),
    new KinoPortClient(http, KinoPort),
    new Cinema1Client(http, Cinema1Gdansk, today),
  )

  private val bydgoszczScrapers: Seq[CinemaScraper] = Seq(
    new CinemaCityScraper(cinemaCityClient, "1086", CinemaCityBydgoszcz),
    new MultikinoClient(mkFetch, "0006", MultikinoBydgoszcz),
    new HeliosClient(http, HeliosNuxt.Bydgoszcz, today),
    new KinoOrzelClient(http, KinoOrzel),
  )

  private val lublinScrapers: Seq[CinemaScraper] = Seq(
    new CinemaCityScraper(cinemaCityClient, "1094", CinemaCityLublinFelicity),
    new CinemaCityScraper(cinemaCityClient, "1084", CinemaCityLublinPlaza),
    new MultikinoClient(mkFetch, "0034", MultikinoLublin),
    new KinoBajkaClient(http, KinoBajka),
    new Bilety24Client(http, "https://ck-lublin.bilety24.pl", KinoCkLublin),
  )

  private val czestochowaScrapers: Seq[CinemaScraper] = Seq(
    new CinemaCityScraper(cinemaCityClient, "1089", CinemaCityCzestochowaJurajska),
    new CinemaCityScraper(cinemaCityClient, "1075", CinemaCityCzestochowaWolnosc),
    new OkfIluzjaClient(http, OkfIluzja, today),
  )

  private val radomScrapers: Seq[CinemaScraper] = Seq(
    new HeliosClient(http, HeliosNuxt.Radom, today),
    new MultikinoClient(mkFetch, "0026", MultikinoRadom),
    new McswElektrowniaCinemaClient(http, McswElektrowniaCinema, today),
  )

  private val sosnowiecScrapers: Seq[CinemaScraper] = Seq(
    new HeliosClient(http, HeliosNuxt.Sosnowiec, today),
    new CinemaCityScraper(cinemaCityClient, "1083", CinemaCitySosnowiec),
  )

  private val torunScrapers: Seq[CinemaScraper] = Seq(
    new CinemaCityScraper(cinemaCityClient, "1077", CinemaCityTorunCzerwonaDroga),
    new CinemaCityScraper(cinemaCityClient, "1093", CinemaCityTorunPlaza),
    new KinoCentrumCswClient(http, KinoCentrumCsw, today),
  )

  private val kielceScrapers: Seq[CinemaScraper] = Seq(
    new HeliosClient(http, HeliosNuxt.Kielce, today),
    new MultikinoClient(mkFetch, "0029", MultikinoKielce),
    new KinoFenomenClient(http, KinoFenomen),
    new KinoMoskwaClient(http, KinoMoskwa, today),
  )

  private val rzeszowScrapers: Seq[CinemaScraper] = Seq(
    new HeliosClient(http, HeliosNuxt.Rzeszow, today),
    new MultikinoClient(mkFetch, "0028", MultikinoRzeszow),
    new KinoZorzaClient(http, KinoZorza),
    new KinoZaRogiemCafeClient(http, KinoZaRogiemCafe, today),
  )

  private val gliwiceScrapers: Seq[CinemaScraper] = Seq(
    new CinemaCityScraper(cinemaCityClient, "1085", CinemaCityGliwice),
    new KinoAmokClient(http, KinoAmok, today),
  )

  private val zabrzeScrapers: Seq[CinemaScraper] = Seq(
    new MultikinoClient(mkFetch, "0003", MultikinoZabrze),
    new KinoRomaClient(http, KinoRoma, today),
  )

  // ── New mid-size cities — national-chain branches only ──────────────────────
  // Multikino codes verified against multikino.pl's cinema-list API, Cinema City
  // ids against cinema-city.pl's data-api-service, Helios sourceIds/slugs against
  // restapi.helios.pl. Each city's local independent screen, where one exists,
  // joins later as a bespoke scraper.
  private val olsztynScrapers      = Seq(new HeliosClient(http, HeliosNuxt.Olsztyn, today), new MultikinoClient(mkFetch, "0036", MultikinoOlsztyn))
  private val bielskoBialaScrapers = Seq(new HeliosClient(http, HeliosNuxt.BielskoBiala, today), new CinemaCityScraper(cinemaCityClient, "1088", CinemaCityBielskoBiala))
  private val opoleScrapers        = Seq(new HeliosClient(http, HeliosNuxt.OpoleKarolinka, today), new HeliosClient(http, HeliosNuxt.OpoleSolaris, today))
  private val rybnikScrapers       = Seq(new MultikinoClient(mkFetch, "0014", MultikinoRybnik), new CinemaCityScraper(cinemaCityClient, "1082", CinemaCityRybnik))
  private val gorzowScrapers       = Seq(new HeliosClient(http, HeliosNuxt.Gorzow, today), new MultikinoClient(mkFetch, "0047", MultikinoGorzow))
  private val elblagScrapers       = Seq(new MultikinoClient(mkFetch, "0037", MultikinoElblag), new CinemaCityScraper(cinemaCityClient, "1099", CinemaCityElblag))
  private val koszalinScrapers     = Seq(new HeliosClient(http, HeliosNuxt.Koszalin, today), new MultikinoClient(mkFetch, "0015", MultikinoKoszalin))
  private val kaliszScrapers       = Seq(new HeliosClient(http, HeliosNuxt.Kalisz, today), new MultikinoClient(mkFetch, "0042", MultikinoKalisz))
  private val zielonaGoraScrapers  = Seq(new CinemaCityScraper(cinemaCityClient, "1087", CinemaCityZielonaGora))
  private val tychyScrapers        = Seq(new MultikinoClient(mkFetch, "0053", MultikinoTychy))
  private val walbrzychScrapers    = Seq(new CinemaCityScraper(cinemaCityClient, "1091", CinemaCityWalbrzych))
  private val tarnowScrapers       = Seq(new MultikinoClient(mkFetch, "0050", MultikinoTarnow))
  private val wloclawekScrapers    = Seq(new MultikinoClient(mkFetch, "0008", MultikinoWloclawek))
  private val legnicaScrapers      = Seq(new HeliosClient(http, HeliosNuxt.Legnica, today))
  private val plockScrapers        = Seq(new HeliosClient(http, HeliosNuxt.Plock, today))
  private val bytomScrapers        = Seq(new CinemaCityScraper(cinemaCityClient, "1092", CinemaCityBytom))
  private val dabrowaGorniczaScrapers = Seq(new HeliosClient(http, HeliosNuxt.DabrowaGornicza, today))
  private val nowySaczScrapers     = Seq(new HeliosClient(http, HeliosNuxt.NowySacz, today))
  private val slupskScrapers       = Seq(new MultikinoClient(mkFetch, "0030", MultikinoSlupsk))
  private val jeleniaGoraScrapers  = Seq(new HeliosClient(http, HeliosNuxt.JeleniaGora, today))
  private val przemyslScrapers     = Seq(new HeliosClient(http, HeliosNuxt.Przemysl, today))
  private val koninScrapers        = Seq(new HeliosClient(http, HeliosNuxt.Konin, today))

  /** Raw scrapers grouped by city slug — same slugs `City.slug` uses, so a
   *  caller can scope by city without re-spelling the membership. */
  val byCity: Map[String, Seq[CinemaScraper]] = Map(
    "poznan"     -> poznanScrapers,
    "wroclaw"    -> wroclawScrapers,
    "warszawa"   -> warszawaScrapers,
    "krakow"     -> krakowScrapers,
    "lodz"       -> lodzScrapers,
    "katowice"   -> katowiceScrapers,
    "szczecin"   -> szczecinScrapers,
    "bialystok"  -> bialystokScrapers,
    "trojmiasto" -> trojmiastoScrapers,
    "bydgoszcz"  -> bydgoszczScrapers,
    "lublin"     -> lublinScrapers,
    "czestochowa" -> czestochowaScrapers,
    "radom"      -> radomScrapers,
    "sosnowiec"  -> sosnowiecScrapers,
    "torun"      -> torunScrapers,
    "kielce"     -> kielceScrapers,
    "rzeszow"    -> rzeszowScrapers,
    "gliwice"    -> gliwiceScrapers,
    "zabrze"     -> zabrzeScrapers,
    "olsztyn"    -> olsztynScrapers,
    "bielsko-biala" -> bielskoBialaScrapers,
    "opole"      -> opoleScrapers,
    "rybnik"     -> rybnikScrapers,
    "gorzow-wielkopolski" -> gorzowScrapers,
    "elblag"     -> elblagScrapers,
    "koszalin"   -> koszalinScrapers,
    "kalisz"     -> kaliszScrapers,
    "zielona-gora" -> zielonaGoraScrapers,
    "tychy"      -> tychyScrapers,
    "walbrzych"  -> walbrzychScrapers,
    "tarnow"     -> tarnowScrapers,
    "wloclawek"  -> wloclawekScrapers,
    "legnica"    -> legnicaScrapers,
    "plock"      -> plockScrapers,
    "bytom"      -> bytomScrapers,
    "dabrowa-gornicza" -> dabrowaGorniczaScrapers,
    "nowy-sacz"  -> nowySaczScrapers,
    "slupsk"     -> slupskScrapers,
    "jelenia-gora" -> jeleniaGoraScrapers,
    "przemysl"   -> przemyslScrapers,
    "konin"      -> koninScrapers,
  )

  /** Every raw scraper across every city, in city order. */
  val all: Seq[CinemaScraper] =
    City.all.flatMap(c => byCity.getOrElse(c.slug, Nil))
}
