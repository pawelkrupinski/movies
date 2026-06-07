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
    // biletyna.pl 403s our datacenter IP (Cloudflare waiting-room), so route
    // through `bnFetch` — Zyte's residential egress in prod, the fixture fake
    // in tests. Same seam as Kino Kameralne below.
    new AdaKinoStudyjneClient(bnFetch, AdaKinoStudyjne),
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
    new FilmwebShowtimesClient(http, 2305, KinoTatry, today = today),
  )

  private val katowiceScrapers: Seq[CinemaScraper] = Seq(
    new CinemaCityScraper(cinemaCityClient, "1065", CinemaCityPunkt44),
    new CinemaCityScraper(cinemaCityClient, "1079", CinemaCitySilesia),
    new MultikinoClient(mkFetch, "0035", MultikinoKatowice),
    new HeliosClient(http, HeliosNuxt.Katowice, today),
    // Silesia Film's art-house trio, all Bilety24-hosted: listing at `/repertuar/`
    // linking per-film `/wydarzenie/?id=N` pages, so they reuse the shared Bilety24Client.
    new Bilety24Client(http, "https://kinokosmos.bilety24.pl", KinoKosmos),
    new Bilety24Client(http, "https://swiatowid-katowice.bilety24.pl", KinoSwiatowid),
    new Bilety24Client(http, "https://kinoteatrrialto.bilety24.pl", KinoteatrRialto),
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
    new GdynskieCentrumFilmoweClient(http, GdynskieCentrumFilmowe),
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
    new KinoCskClient(http, KinoCskLublin),
    new FilmwebShowtimesClient(http, 109, KinoChatkaZaka, today = today),
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

  // ── New mid-size cities ─────────────────────────────────────────────────────
  // Chain ids verified against each chain's own cinema-list API. Each city's
  // independent screen, where one has a machine-readable source, is added via a
  // shared platform client: FilmwebShowtimesClient (Filmweb's seances JSON, by
  // internal cinema id — verified to return non-empty seances), Bilety24Client
  // (a bilety24.pl venue), or NoveKinoClient.
  private val olsztynScrapers      = Seq(new HeliosClient(http, HeliosNuxt.Olsztyn, today), new MultikinoClient(mkFetch, "0036", MultikinoOlsztyn), new FilmwebShowtimesClient(http, 1527, KinoAwangarda2, today = today))
  private val bielskoBialaScrapers = Seq(new HeliosClient(http, HeliosNuxt.BielskoBiala, today), new CinemaCityScraper(cinemaCityClient, "1088", CinemaCityBielskoBiala), new FilmwebShowtimesClient(http, 3044, KinoKreska, today = today))
  private val opoleScrapers        = Seq(new HeliosClient(http, HeliosNuxt.OpoleKarolinka, today), new HeliosClient(http, HeliosNuxt.OpoleSolaris, today), new FilmwebShowtimesClient(http, 1716, KinoMeduza, today = today))
  private val rybnikScrapers       = Seq(new MultikinoClient(mkFetch, "0014", MultikinoRybnik), new CinemaCityScraper(cinemaCityClient, "1082", CinemaCityRybnik))
  private val gorzowScrapers       = Seq(new HeliosClient(http, HeliosNuxt.Gorzow, today), new MultikinoClient(mkFetch, "0047", MultikinoGorzow), new FilmwebShowtimesClient(http, 609, Kino60Krzesel, today = today))
  private val elblagScrapers       = Seq(new MultikinoClient(mkFetch, "0037", MultikinoElblag), new CinemaCityScraper(cinemaCityClient, "1099", CinemaCityElblag))
  private val koszalinScrapers     = Seq(new HeliosClient(http, HeliosNuxt.Koszalin, today), new MultikinoClient(mkFetch, "0015", MultikinoKoszalin), new FilmwebShowtimesClient(http, 280, KinoKryterium, today = today))
  private val kaliszScrapers       = Seq(new HeliosClient(http, HeliosNuxt.Kalisz, today), new MultikinoClient(mkFetch, "0042", MultikinoKalisz))
  private val zielonaGoraScrapers  = Seq(new CinemaCityScraper(cinemaCityClient, "1087", CinemaCityZielonaGora))
  private val tychyScrapers        = Seq(new MultikinoClient(mkFetch, "0053", MultikinoTychy))
  private val walbrzychScrapers    = Seq(new CinemaCityScraper(cinemaCityClient, "1091", CinemaCityWalbrzych), new Bilety24Client(http, "https://kino-apollo.bilety24.pl", KinoApolloWalbrzych))
  private val tarnowScrapers       = Seq(new MultikinoClient(mkFetch, "0050", MultikinoTarnow), new FilmwebShowtimesClient(http, 438, KinoMillenium, today = today))
  private val wloclawekScrapers    = Seq(new MultikinoClient(mkFetch, "0008", MultikinoWloclawek))
  private val legnicaScrapers      = Seq(new HeliosClient(http, HeliosNuxt.Legnica, today), new Bilety24Client(http, "https://kino-piast.bilety24.pl", KinoPiast))
  private val plockScrapers        = Seq(new HeliosClient(http, HeliosNuxt.Plock, today), new NoveKinoClient(http, "przedwiosnie", KinoPrzedwiosnie))
  private val bytomScrapers        = Seq(new CinemaCityScraper(cinemaCityClient, "1092", CinemaCityBytom))
  private val dabrowaGorniczaScrapers = Seq(new HeliosClient(http, HeliosNuxt.DabrowaGornicza, today), new FilmwebShowtimesClient(http, 1140, KinoKadr, today = today))
  private val nowySaczScrapers     = Seq(new HeliosClient(http, HeliosNuxt.NowySacz, today), new FilmwebShowtimesClient(http, 171, KinoSokol, today = today))
  private val slupskScrapers       = Seq(new MultikinoClient(mkFetch, "0030", MultikinoSlupsk), new FilmwebShowtimesClient(http, 447, KinoRejs, today = today))
  private val jeleniaGoraScrapers  = Seq(new HeliosClient(http, HeliosNuxt.JeleniaGora, today), new Bilety24Client(http, "https://kino-lot.bilety24.pl", KinoLot))
  private val przemyslScrapers     = Seq(new HeliosClient(http, HeliosNuxt.Przemysl, today))
  // Konin + its catchment: Helios via the chain client, Oskard via Bilety24, and
  // the remaining independents Filmweb serves by internal cinema id (verified
  // non-empty seances 2026-06). Września's Kino Trójka (1698) is intentionally
  // not wired.
  private val koninScrapers        = Seq(
    new HeliosClient(http, HeliosNuxt.Konin, today),
    new Bilety24Client(http, "https://ckis-konin.bilety24.pl", KinoOskard),
    new FilmwebShowtimesClient(http, 2405, KinoZacheta,  today = today),   // Kleczew
    new FilmwebShowtimesClient(http, 1526, KinoNadWarta, today = today),   // Koło
    new FilmwebShowtimesClient(http, 2417, KinoHel,      today = today),   // Pleszew
    new FilmwebShowtimesClient(http, 1694, KinoSokolnia, today = today),   // Słupca
    new FilmwebShowtimesClient(http, 1523, KinoTur,      today = today),   // Turek
    new FilmwebShowtimesClient(http, 1851, KinoMok,      today = today),   // Zagórów
  )

  /** Raw scrapers grouped by city slug — same slugs `City.slug` uses, so a
   *  caller can scope by city without re-spelling the membership. */
  // Filmweb catchment cinemas (nearby towns), each by Filmweb internal cinema id.
  // Merged into byCity below so every city's catchment is scraped without
  // touching its hand-written scraper group.
  private val filmwebExtra: Map[String, Seq[CinemaScraper]] = Map(
    "wroclaw" -> Seq(new FilmwebShowtimesClient(http, 2328, KinoAstra, today = today), new FilmwebShowtimesClient(http, 1645, KinoDyskusyjnyKlubFilmowyPolitechnika, today = today)),
    "warszawa" -> Seq(new FilmwebShowtimesClient(http, 2180, KinoMazowieckiTeatrMuzycznyImJanaKiepuryKinoPraha, today = today), new FilmwebShowtimesClient(http, 2130, KinoPlanetariumCentrumNaukiKopernik, today = today)),
    "lodz" -> Seq(new FilmwebShowtimesClient(http, 2403, KinoSpojnia, today = today), new FilmwebShowtimesClient(http, 2443, KinoStaryMlyn, today = today)),
    "katowice" -> Seq(new FilmwebShowtimesClient(http, 388, CinemaCity, today = today), new FilmwebShowtimesClient(http, 352, KinoPatria, today = today)),
    "szczecin" -> Seq(new FilmwebShowtimesClient(http, 117, KinoKawiarnia, today = today), new FilmwebShowtimesClient(http, 2363, KinoPDK, today = today), new FilmwebShowtimesClient(http, 1941, KinoSCK, today = today)),
    "bialystok" -> Seq(new FilmwebShowtimesClient(http, 1659, KinoSokolSokolka, today = today)),
    "trojmiasto" -> Seq(new FilmwebShowtimesClient(http, 2100, KinoNaSzekspirowskim, today = today), new FilmwebShowtimesClient(http, 1464, MultikinoRumia, today = today)),
    "bydgoszcz" -> Seq(new FilmwebShowtimesClient(http, 1124, KinoKinomax, today = today), new FilmwebShowtimesClient(http, 3121, KinoRondo, today = today)),
    "lublin" -> Seq(new FilmwebShowtimesClient(http, 1697, KinoLewart, today = today), new FilmwebShowtimesClient(http, 285, KinoMetalowiec, today = today)),
    "czestochowa" -> Seq(new FilmwebShowtimesClient(http, 1714, KinoDKFRumcajs, today = today), new FilmwebShowtimesClient(http, 1719, KinoKarolinka, today = today), new FilmwebShowtimesClient(http, 1732, KinoMDK, today = today), new FilmwebShowtimesClient(http, 1525, KinoMOKCentrum, today = today), new FilmwebShowtimesClient(http, 2350, KinoZacisze, today = today)),
    "radom" -> Seq(new FilmwebShowtimesClient(http, 1845, HeliosStarachowice, today = today), new FilmwebShowtimesClient(http, 1705, KinoCentrumSkarzyskoKamienna, today = today), new FilmwebShowtimesClient(http, 3064, KinoGornik, today = today), new FilmwebShowtimesClient(http, 1913, KinoKozienickiDomKultury, today = today), new FilmwebShowtimesClient(http, 1713, KinoKuznica, today = today), new FilmwebShowtimesClient(http, 2342, KinoSwitZwolen, today = today)),
    "torun" -> Seq(new FilmwebShowtimesClient(http, 3119, KinoMiejskieCentrumKultury, today = today), new FilmwebShowtimesClient(http, 1771, KinoZdroj, today = today)),
    "kielce" -> Seq(new FilmwebShowtimesClient(http, 2351, KinoCK, today = today), new FilmwebShowtimesClient(http, 3128, KinoKoneckieCentrumKultury, today = today)),
    "rzeszow" -> Seq(new FilmwebShowtimesClient(http, 2014, HeliosKrosno, today = today), new FilmwebShowtimesClient(http, 1122, KinoArtKino, today = today), new FilmwebShowtimesClient(http, 2335, KinoJednosc, today = today), new FilmwebShowtimesClient(http, 2344, KinoMCK, today = today), new FilmwebShowtimesClient(http, 1500, KinoSniezka, today = today), new FilmwebShowtimesClient(http, 1477, KinoSokolBrzozow, today = today), new FilmwebShowtimesClient(http, 2346, KinoWarszawa, today = today)),
    "gliwice" -> Seq(new FilmwebShowtimesClient(http, 1494, KinoScenaKultura, today = today)),
    "olsztyn" -> Seq(new FilmwebShowtimesClient(http, 2357, KinoCinemaLumiere, today = today), new FilmwebShowtimesClient(http, 2354, KinoIgnacy, today = today), new FilmwebShowtimesClient(http, 2355, KinoNarie, today = today)),
    "bielsko-biala" -> Seq(new FilmwebShowtimesClient(http, 157, KinoJanosik, today = today), new FilmwebShowtimesClient(http, 3248, KinoPckulKino, today = today), new FilmwebShowtimesClient(http, 135, KinoSwitCzechowiceDziedzice, today = today), new FilmwebShowtimesClient(http, 3141, KinoTeatrElektryczny, today = today), new FilmwebShowtimesClient(http, 1490, KinoWislaBrzeszcze, today = today), new FilmwebShowtimesClient(http, 1776, MultikinoCzechowiceDziedzice, today = today)),
    "opole" -> Seq(new FilmwebShowtimesClient(http, 1703, HeliosKedzierzynKozle, today = today), new FilmwebShowtimesClient(http, 2320, KinoBajkaKluczbork, today = today), new FilmwebShowtimesClient(http, 619, KinoChemik, today = today), new FilmwebShowtimesClient(http, 2343, KinoDiana, today = today), new FilmwebShowtimesClient(http, 1681, KinoKrapkowice, today = today), new FilmwebShowtimesClient(http, 431, KinoStudio, today = today), new FilmwebShowtimesClient(http, 1672, KinoTwierdza, today = today)),
    "rybnik" -> Seq(new FilmwebShowtimesClient(http, 2326, HeliosZory, today = today), new FilmwebShowtimesClient(http, 168, KinoBaltyk, today = today), new FilmwebShowtimesClient(http, 514, KinoCentrum, today = today), new FilmwebShowtimesClient(http, 588, KinoNaStarowce, today = today), new FilmwebShowtimesClient(http, 3148, KinoPegaz, today = today), new FilmwebShowtimesClient(http, 3140, KinoTeatrZiemiRybnickiej, today = today)),
    "elblag" -> Seq(new FilmwebShowtimesClient(http, 1673, HeliosTczew, today = today), new FilmwebShowtimesClient(http, 2352, KinoBaszta, today = today), new FilmwebShowtimesClient(http, 1798, KinoPowisle, today = today), new FilmwebShowtimesClient(http, 3131, KinoZulawskiOsrodekKultury, today = today)),
    "koszalin" -> Seq(new FilmwebShowtimesClient(http, 255, KinoBajkaDarlowo, today = today), new FilmwebShowtimesClient(http, 1675, KinoCentrumBialogard, today = today), new FilmwebShowtimesClient(http, 2365, KinoDK, today = today), new FilmwebShowtimesClient(http, 2414, KinoGOK, today = today), new FilmwebShowtimesClient(http, 1864, KinoGoplana, today = today), new FilmwebShowtimesClient(http, 276, KinoWybrzeze, today = today)),
    "kalisz" -> Seq(new FilmwebShowtimesClient(http, 2372, HeliosOstrowWlkp, today = today), new FilmwebShowtimesClient(http, 1513, KinoCentrum3D, today = today), new FilmwebShowtimesClient(http, 1484, KinoEcho, today = today), new FilmwebShowtimesClient(http, 2359, KinoPiastOstrzeszow, today = today), new FilmwebShowtimesClient(http, 1121, KinoPrzedwiosnieKrotoszyn, today = today)),
    "zielona-gora" -> Seq(new FilmwebShowtimesClient(http, 1955, KinoEuropa, today = today), new FilmwebShowtimesClient(http, 2171, KinoMaxKino, today = today), new FilmwebShowtimesClient(http, 1430, KinoPionierZary, today = today), new FilmwebShowtimesClient(http, 2331, KinoSDKSwiebodzin, today = today)),
    "tychy" -> Seq(new FilmwebShowtimesClient(http, 1480, KinoNaszeKino, today = today), new FilmwebShowtimesClient(http, 1528, KinoPlanetCinema, today = today)),
    "walbrzych" -> Seq(new FilmwebShowtimesClient(http, 1493, KinoMOKNowaRuda, today = today), new FilmwebShowtimesClient(http, 197, KinoMOKiS, today = today), new FilmwebShowtimesClient(http, 2410, KinoSleza, today = today), new FilmwebShowtimesClient(http, 1530, KinoZbyszek, today = today), new FilmwebShowtimesClient(http, 2990, MultikinoKlodzko, today = today), new FilmwebShowtimesClient(http, 2993, MultikinoSwidnica, today = today)),
    "tarnow" -> Seq(new FilmwebShowtimesClient(http, 2315, KinoFarys, today = today), new FilmwebShowtimesClient(http, 2411, KinoGCK, today = today), new FilmwebShowtimesClient(http, 2404, KinoKolory, today = today), new FilmwebShowtimesClient(http, 1481, KinoPlaneta, today = today), new FilmwebShowtimesClient(http, 2419, KinoPromien, today = today), new FilmwebShowtimesClient(http, 1294, KinoRegis, today = today), new FilmwebShowtimesClient(http, 1488, KinoSokolDabrowaTarnowska, today = today)),
    "wloclawek" -> Seq(new FilmwebShowtimesClient(http, 2341, KinoJutrzenka, today = today), new FilmwebShowtimesClient(http, 3130, KinoNawojka, today = today), new FilmwebShowtimesClient(http, 3246, KinoNoweKinoWarszawa, today = today), new FilmwebShowtimesClient(http, 1949, KinoZaRogiem, today = today)),
    "legnica" -> Seq(new FilmwebShowtimesClient(http, 1420, HeliosLubin, today = today), new FilmwebShowtimesClient(http, 1718, KinoAurum, today = today), new FilmwebShowtimesClient(http, 2313, KinoCyfroweKino, today = today), new FilmwebShowtimesClient(http, 373, KinoForumBoleslawiec, today = today), new FilmwebShowtimesClient(http, 288, KinoMuzaLubin, today = today), new FilmwebShowtimesClient(http, 1139, KinoPCA, today = today)),
    "plock" -> Seq(new FilmwebShowtimesClient(http, 1134, KinoKDK, today = today), new FilmwebShowtimesClient(http, 1702, KinoKalejdoskop, today = today), new FilmwebShowtimesClient(http, 2128, KinoODEON, today = today)),
    "nowy-sacz" -> Seq(new FilmwebShowtimesClient(http, 561, KinoJaworzyna, today = today), new FilmwebShowtimesClient(http, 1137, KinoKlaps, today = today)),
    "slupsk" -> Seq(new FilmwebShowtimesClient(http, 2123, KinoFregata, today = today)),
    "jelenia-gora" -> Seq(new FilmwebShowtimesClient(http, 1721, KinoWawel, today = today)),
    "przemysl" -> Seq(new FilmwebShowtimesClient(http, 1786, KinoCentrum3DPrzemysl, today = today), new FilmwebShowtimesClient(http, 1707, KinoIkar, today = today), new FilmwebShowtimesClient(http, 2172, KinoNaBiegunach, today = today), new FilmwebShowtimesClient(http, 2118, KinoSDK, today = today)),
  )

  private val baseByCity: Map[String, Seq[CinemaScraper]] = Map(
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

  /** Per-city scrapers plus any Filmweb-catchment venues for that city. */
  val byCity: Map[String, Seq[CinemaScraper]] =
    baseByCity.map { case (slug, scrapers) => slug -> (scrapers ++ filmwebExtra.getOrElse(slug, Nil)) }

  /** Every raw scraper across every city, in city order. */
  val all: Seq[CinemaScraper] =
    City.all.flatMap(c => byCity.getOrElse(c.slug, Nil))
}
