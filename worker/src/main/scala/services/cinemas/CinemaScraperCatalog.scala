package services.cinemas

import models._
import tools.{CachingDetailFetch, HttpFetch}

import java.time.{LocalDate, ZoneId}
import scala.concurrent.duration._

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
 * (retry) + `UptimeRecordingScraper` (record the outcome) for production scrape
 * ticks; a diagnostic uses them bare.
 */
class CinemaScraperCatalog(
  http:    HttpFetch,
  mkFetch: HttpFetch,
  bnFetch: HttpFetch,
  today:   LocalDate,
  // When true, cinemas that implement DetailEnricher return BARE movies from
  // fetch() and their per-film detail is fetched later via EnrichDetails tasks.
  // Required (no default) because the secondary diagnostic ctor below already
  // carries defaults, and Scala allows only one overloaded ctor to do so; the
  // secondary passes false (inline detail).
  deferDetail: Boolean,
  // Builds the per-chain detail-page cache (Helios / Cinema City). The worker
  // injects a Mongo-backed cache so chain detail is deduped across servers; the
  // diagnostic ctor + tests default to the in-process CachingDetailFetch.
  chainDetailCache: (HttpFetch, FiniteDuration) => HttpFetch
) {

  /** Diagnostic ctor: the Zyte-routed fetches (Multikino's API, biletyna's venue
   *  pages) default to the path derived from `http` (a clean body-derived
   *  default, not the old `null`-param workaround — Scala can't reference `http`
   *  in a primary-constructor default, but a secondary constructor can).
   *  `WorkerWiring` uses the primary ctor to inject its (possibly
   *  fixture-overridden) `multikinoFetch` / `biletynaFetch`. */
  def this(http: HttpFetch, today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))) =
    this(http, MultikinoClient.fetchFor(http), ZyteFallback.fetchFor(http), today, false,
      (h, ttl) => new CachingDetailFetch(h, ttl))

  // Per-film detail bodies are static between passes and IDENTICAL across a
  // chain's locations, so each chain shares ONE 6h CachingDetailFetch: a film's
  // detail (Helios `/api/movie/{id}`, Cinema City film page) is fetched once per
  // chain per 6h instead of once per location per pass. Live listing/screening
  // fetches stay on `http`.
  private val ChainDetailTtl = 6.hours
  private val heliosDetailHttp:     HttpFetch = chainDetailCache(http, ChainDetailTtl)
  private val cinemaCityDetailHttp: HttpFetch = chainDetailCache(http, ChainDetailTtl)
  private def helios(cfg: HeliosCinema): HeliosClient =
    new HeliosClient(http, cfg, today, Some(heliosDetailHttp))

  // Shared per-source helper clients the scrapers below reuse.
  val cinemaCityClient: CinemaCityClient = new CinemaCityClient(http, Some(cinemaCityDetailHttp))
  // One per Cinema City venue, threading the catalogue's deferDetail through.
  private def cinemaCity(cinemaId: String, cinema: Cinema): CinemaCityScraper =
    new CinemaCityScraper(cinemaCityClient, cinemaId, cinema, deferDetail)
  val kinoMuzaClient:   KinoMuzaClient   = new KinoMuzaClient(http, today)

  private val poznanScrapers: Seq[CinemaScraper] = Seq(
    new MultikinoClient(mkFetch),
    new CharlieMonroeClient(http),
    new KinoPalacoweClient(http, deferDetail),
    helios(HeliosNuxt.Poznan),
    cinemaCity("1078", CinemaCityPoznanPlaza),
    cinemaCity("1081", CinemaCityKinepolis),
    kinoMuzaClient,
    new KinoBulgarskaClient(http, today, deferDetail = deferDetail),
    new KinoApolloClient(http, deferDetail),
    new RialtoClient(http, deferDetail),
  )

  private val wroclawScrapers: Seq[CinemaScraper] = Seq(
    cinemaCity("1097", CinemaCityWroclavia),
    cinemaCity("1067", CinemaCityKorona),
    new MultikinoClient(mkFetch, "0010", MultikinoPasazGrunwaldzki),
    helios(HeliosNuxt.Magnolia),
    helios(HeliosNuxt.AlejaBielany),
    new NoweHoryzontyClient(http, today, deferDetail = deferDetail),
    new DcfClient(http, deferDetail),
  )

  private val warszawaScrapers: Seq[CinemaScraper] = Seq(
    cinemaCity("1074", CinemaCityArkadia),
    cinemaCity("1061", CinemaCityBemowo),
    cinemaCity("1096", CinemaCityGaleriaPolnocna),
    cinemaCity("1069", CinemaCityJanki),
    cinemaCity("1070", CinemaCityMokotow),
    cinemaCity("1068", CinemaCityPromenada),
    cinemaCity("1060", CinemaCitySadyba),
    new MultikinoClient(mkFetch, "0013", MultikinoZloteTarasy),
    new MultikinoClient(mkFetch, "0040", MultikinoMlociny),
    new MultikinoClient(mkFetch, "0052", MultikinoReduta),
    new MultikinoClient(mkFetch, "0024", MultikinoTargowek),
    new MultikinoClient(mkFetch, "0025", MultikinoWolaPark),
    helios(HeliosNuxt.BlueCity),
    new MuranowClient(http, today, deferDetail = deferDetail),
    new Bilety24Client(http, "https://kinoluna.bilety24.pl", KinoLuna),
    new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-elektronik-631", KinoElektronik),
    new IluzjonClient(http, today, deferDetail = deferDetail),
    new KinoGramClient(http),
    new KinoKulturaClient(http),
    new AmondoClient(http, deferDetail),
    new BokClient(http, "kino-na-boku", KinoNaBoku, today),
    new BokClient(http, "kino-glebocka-66", KinoGlebocka66, today),
    new KinomuzeumClient(http, today, deferDetail = deferDetail),
    new SwitClient(http),
    new PromKepaClient(http),
    new FalenicaClient(http, deferDetail),
    new SdkClient(http),
    new NoveKinoClient(http, "atlantic", KinoAtlantic, deferDetail),
    new KinotekaClient(http, deferDetail),
    new UjazdowskiClient(http, deferDetail),
    new CytadelaClient(http, deferDetail),
    new NoveKinoClient(http, "wisla", KinoWisla, deferDetail),
    // biletyna.pl 403s our datacenter IP (Cloudflare waiting-room), so route
    // through `bnFetch` — Zyte's residential egress in prod, the fixture fake
    // in tests. Same seam as Kino Kameralne below.
    new BiletynaClient(bnFetch, "https://www.biletyna.pl/Warszawa/ADA-Kino-Studyjne", AdaKinoStudyjne),
    new AlternatywyClient(http, today),
  )

  private val krakowScrapers: Seq[CinemaScraper] = Seq(
    cinemaCity("1090", CinemaCityBonarka),
    cinemaCity("1076", CinemaCityKazimierz),
    cinemaCity("1064", CinemaCityZakopianka),
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
    cinemaCity("1080", CinemaCityManufaktura),
    new MultikinoClient(mkFetch, "0023", MultikinoLodz),
    helios(HeliosNuxt.Lodz),
    new CharlieClient(http, KinoCharlie),
    new KinematografLodzClient(http, KinematografLodz, today),
    new NckfClient(http, Nckf, today),
    new FilmwebShowtimesClient(http, 2305, KinoTatry, today = today),
  )

  private val katowiceScrapers: Seq[CinemaScraper] = Seq(
    cinemaCity("1065", CinemaCityPunkt44),
    cinemaCity("1079", CinemaCitySilesia),
    new MultikinoClient(mkFetch, "0035", MultikinoKatowice),
    helios(HeliosNuxt.Katowice),
    // Silesia Film's art-house trio, all Bilety24-hosted: listing at `/repertuar/`
    // linking per-film `/wydarzenie/?id=N` pages, so they reuse the shared Bilety24Client.
    new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-kosmos-1501", KinoKosmos),
    new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-swiatowid-1503", KinoSwiatowid),
    new Bilety24Client(http, "https://kinoteatrrialto.bilety24.pl", KinoteatrRialto),
  )

  private val szczecinScrapers: Seq[CinemaScraper] = Seq(
    helios(HeliosNuxt.Szczecin),
    new MultikinoClient(mkFetch, "0007", MultikinoSzczecin),
    new PionierClient(http, KinoPionier),
    helios(HeliosNuxt.SzczecinOutletPark),
    new KinoZamekClient(http, KinoZamekSzczecin, today),
  )

  private val bialystokScrapers: Seq[CinemaScraper] = Seq(
    helios(HeliosNuxt.Alfa),
    helios(HeliosNuxt.Biala),
    helios(HeliosNuxt.Jurowiecka),
    new KinoForumClient(http, today),
  )

  private val trojmiastoScrapers: Seq[CinemaScraper] = Seq(
    new MultikinoClient(mkFetch, "0004", MultikinoGdansk),
    helios(HeliosNuxt.Metropolia),
    helios(HeliosNuxt.Forum),
    helios(HeliosNuxt.Riviera),
    new KinoSpektrumClient(http, KinoSpektrum),
    // biletyna.pl 403s our datacenter IP, so route through `bnFetch` — Zyte's
    // residential egress in production, the fixture fake in tests. See
    // WorkerWiring.biletynaFetch / ZyteFallback.
    new BiletynaClient(bnFetch, "https://biletyna.pl/Gdansk/Kino-Kameralne-Cafe", KinoKameralne),
    new KinoIkmClient(http, KinoIkm),
    new KinoMuzeumGdanskClient(http, KinoMuzeumGdansk),
    new KinoZakClient(http, KinoZak),
    // KinoPort (CSW Łaźnia, Gdańsk) dropped its stable gcsw.pl/kino/ programme
    // alias in a 2026-06 site rebuild, so we read its seances off Filmweb's
    // listing for the venue (Gdańsk id 1735) like the other small cinemas.
    new FilmwebShowtimesClient(http, 1735, KinoPort, today = today),
    new MsiClient(http, "https://bilety.cinemaone.pl", Cinema1Gdansk, today),
    new GdynskieCentrumFilmoweClient(http, GdynskieCentrumFilmowe),
  )

  private val bydgoszczScrapers: Seq[CinemaScraper] = Seq(
    cinemaCity("1086", CinemaCityBydgoszcz),
    new MultikinoClient(mkFetch, "0006", MultikinoBydgoszcz),
    helios(HeliosNuxt.Bydgoszcz),
    new KinoOrzelClient(http, KinoOrzel),
  )

  private val lublinScrapers: Seq[CinemaScraper] = Seq(
    cinemaCity("1094", CinemaCityLublinFelicity),
    cinemaCity("1084", CinemaCityLublinPlaza),
    new MultikinoClient(mkFetch, "0034", MultikinoLublin),
    new KinoBajkaClient(http, KinoBajka),
    new Bilety24Client(http, "https://ck-lublin.bilety24.pl", KinoCkLublin),
    new KinoCskClient(http, KinoCskLublin),
    new FilmwebShowtimesClient(http, 109, KinoChatkaZaka, today = today),
  )

  private val czestochowaScrapers: Seq[CinemaScraper] = Seq(
    cinemaCity("1089", CinemaCityCzestochowaJurajska),
    cinemaCity("1075", CinemaCityCzestochowaWolnosc),
    new OkfIluzjaClient(http, OkfIluzja, today),
  )

  private val radomScrapers: Seq[CinemaScraper] = Seq(
    helios(HeliosNuxt.Radom),
    new MultikinoClient(mkFetch, "0026", MultikinoRadom),
    new McswElektrowniaCinemaClient(http, McswElektrowniaCinema, today),
  )

  private val sosnowiecScrapers: Seq[CinemaScraper] = Seq(
    helios(HeliosNuxt.Sosnowiec),
    cinemaCity("1083", CinemaCitySosnowiec),
  )

  private val torunScrapers: Seq[CinemaScraper] = Seq(
    cinemaCity("1077", CinemaCityTorunCzerwonaDroga),
    cinemaCity("1093", CinemaCityTorunPlaza),
    new KinoCentrumCswClient(http, KinoCentrumCsw, today),
  )

  private val kielceScrapers: Seq[CinemaScraper] = Seq(
    helios(HeliosNuxt.Kielce),
    new MultikinoClient(mkFetch, "0029", MultikinoKielce),
    new KinoFenomenClient(http, KinoFenomen),
    new KinoMoskwaClient(http, KinoMoskwa, today),
  )

  private val rzeszowScrapers: Seq[CinemaScraper] = Seq(
    helios(HeliosNuxt.Rzeszow),
    new MultikinoClient(mkFetch, "0028", MultikinoRzeszow),
    new KinoZorzaClient(http, KinoZorza),
    new KinoZaRogiemCafeClient(http, KinoZaRogiemCafe, today),
  )

  private val gliwiceScrapers: Seq[CinemaScraper] = Seq(
    cinemaCity("1085", CinemaCityGliwice),
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
  private val olsztynScrapers      = Seq(helios(HeliosNuxt.Olsztyn), new MultikinoClient(mkFetch, "0036", MultikinoOlsztyn), new FilmwebShowtimesClient(http, 1527, KinoAwangarda2, today = today))
  private val bielskoBialaScrapers = Seq(helios(HeliosNuxt.BielskoBiala), cinemaCity("1088", CinemaCityBielskoBiala), new FilmwebShowtimesClient(http, 3044, KinoKreska, today = today))
  private val opoleScrapers        = Seq(helios(HeliosNuxt.OpoleKarolinka), helios(HeliosNuxt.OpoleSolaris), new FilmwebShowtimesClient(http, 1716, KinoMeduza, today = today))
  private val rybnikScrapers       = Seq(new MultikinoClient(mkFetch, "0014", MultikinoRybnik), cinemaCity("1082", CinemaCityRybnik))
  private val gorzowScrapers       = Seq(helios(HeliosNuxt.Gorzow), new MultikinoClient(mkFetch, "0047", MultikinoGorzow), new FilmwebShowtimesClient(http, 609, Kino60Krzesel, today = today))
  private val elblagScrapers       = Seq(new MultikinoClient(mkFetch, "0037", MultikinoElblag), cinemaCity("1099", CinemaCityElblag))
  private val koszalinScrapers     = Seq(helios(HeliosNuxt.Koszalin), new MultikinoClient(mkFetch, "0015", MultikinoKoszalin), new FilmwebShowtimesClient(http, 280, KinoKryterium, today = today))
  private val kaliszScrapers       = Seq(helios(HeliosNuxt.Kalisz), new MultikinoClient(mkFetch, "0042", MultikinoKalisz))
  private val zielonaGoraScrapers  = Seq(cinemaCity("1087", CinemaCityZielonaGora))
  private val tychyScrapers        = Seq(new MultikinoClient(mkFetch, "0053", MultikinoTychy))
  private val walbrzychScrapers    = Seq(cinemaCity("1091", CinemaCityWalbrzych), new Bilety24Client(http, "https://kino-apollo.bilety24.pl", KinoApolloWalbrzych))
  private val tarnowScrapers       = Seq(new MultikinoClient(mkFetch, "0050", MultikinoTarnow), new FilmwebShowtimesClient(http, 438, KinoMillenium, today = today))
  private val wloclawekScrapers    = Seq(new MultikinoClient(mkFetch, "0008", MultikinoWloclawek))
  private val legnicaScrapers      = Seq(helios(HeliosNuxt.Legnica), new Bilety24Client(http, "https://kino-piast.bilety24.pl", KinoPiast))
  private val plockScrapers        = Seq(helios(HeliosNuxt.Plock), new NoveKinoClient(http, "przedwiosnie", KinoPrzedwiosnie, deferDetail))
  private val bytomScrapers        = Seq(cinemaCity("1092", CinemaCityBytom))
  private val dabrowaGorniczaScrapers = Seq(helios(HeliosNuxt.DabrowaGornicza), new FilmwebShowtimesClient(http, 1140, KinoKadr, today = today))
  private val nowySaczScrapers     = Seq(helios(HeliosNuxt.NowySacz), new FilmwebShowtimesClient(http, 171, KinoSokol, today = today))
  private val slupskScrapers       = Seq(new MultikinoClient(mkFetch, "0030", MultikinoSlupsk), new FilmwebShowtimesClient(http, 447, KinoRejs, today = today))
  private val jeleniaGoraScrapers  = Seq(helios(HeliosNuxt.JeleniaGora), new Bilety24Client(http, "https://kino-lot.bilety24.pl", KinoLot))
  private val przemyslScrapers     = Seq(helios(HeliosNuxt.Przemysl))
  // Konin + its catchment: Helios via the chain client, Oskard via Bilety24, and
  // the remaining independents Filmweb serves by internal cinema id (verified
  // non-empty seances 2026-06). Września's Kino Trójka (1698) is intentionally
  // not wired.
  private val koninScrapers        = Seq(
    helios(HeliosNuxt.Konin),
    new Bilety24Client(http, "https://ckis-konin.bilety24.pl", KinoOskard),
    new FilmwebShowtimesClient(http, 2405, KinoZacheta,  today = today),   // Kleczew
    new FilmwebShowtimesClient(http, 1526, KinoNadWarta, today = today),   // Koło
    new FilmwebShowtimesClient(http, 2417, KinoHel,      today = today),   // Pleszew
    new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-w-slupcy-1423", KinoSokolnia),   // Słupca
    new FilmwebShowtimesClient(http, 1523, KinoTur,      today = today),   // Turek
    new FilmwebShowtimesClient(http, 1851, KinoMok,      today = today),   // Zagórów
  )

  /** Raw scrapers grouped by city slug — same slugs `City.slug` uses, so a
   *  caller can scope by city without re-spelling the membership. */
  // Catchment cinemas (nearby towns) and a few in-city venues that came in via a
  // Filmweb sweep, each by Filmweb internal cinema id — except the handful that
  // have since moved to their own site (Kino Spójnia, Kino Praha), which fed
  // Filmweb too thinly. Merged into byCity below so every city's catchment is
  // scraped without touching its hand-written scraper group.
  private val filmwebExtra: Map[String, Seq[CinemaScraper]] = Map(
    "wroclaw" -> Seq(new FilmwebShowtimesClient(http, 2328, KinoAstra, today = today), new FilmwebShowtimesClient(http, 1645, KinoDyskusyjnyKlubFilmowyPolitechnika, today = today)),
    "warszawa" -> Seq(new PrahaClient(http, KinoMazowieckiTeatrMuzycznyImJanaKiepuryKinoPraha), new FilmwebShowtimesClient(http, 2130, KinoPlanetariumCentrumNaukiKopernik, today = today)),
    "lodz" -> Seq(new KinoSpojniaClient(http, KinoSpojnia), new FilmwebShowtimesClient(http, 2443, KinoStaryMlyn, today = today)),
    "katowice" -> Seq(new FilmwebShowtimesClient(http, 388, CinemaCity, today = today), new FilmwebShowtimesClient(http, 352, KinoPatria, today = today)),
    "szczecin" -> Seq(new FilmwebShowtimesClient(http, 117, KinoKawiarnia, today = today), new FilmwebShowtimesClient(http, 2363, KinoPDK, today = today), new FilmwebShowtimesClient(http, 1941, KinoSCK, today = today)),
    "bialystok" -> Seq(new FilmwebShowtimesClient(http, 1659, KinoSokolSokolka, today = today)),
    "trojmiasto" -> Seq(new FilmwebShowtimesClient(http, 2100, KinoNaSzekspirowskim, today = today), new FilmwebShowtimesClient(http, 1464, MultikinoRumia, today = today)),
    "bydgoszcz" -> Seq(new FilmwebShowtimesClient(http, 1124, KinoKinomax, today = today), new FilmwebShowtimesClient(http, 3121, KinoRondo, today = today)),
    "lublin" -> Seq(new FilmwebShowtimesClient(http, 1697, KinoLewart, today = today), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/centrum-kultury-i-promocji-w-krasniku-1529", KinoMetalowiec)),
    "czestochowa" -> Seq(new FilmwebShowtimesClient(http, 1714, KinoDKFRumcajs, today = today), new KinoKarolinkaClient(http, KinoKarolinka), new FilmwebShowtimesClient(http, 1732, KinoMDK, today = today), new FilmwebShowtimesClient(http, 1525, KinoMOKCentrum, today = today), new FilmwebShowtimesClient(http, 2350, KinoZacisze, today = today)),
    "radom" -> Seq(new FilmwebShowtimesClient(http, 1845, HeliosStarachowice, today = today), new MsiClient(http, "https://bilet-mck.skarzysko.pl", KinoCentrumSkarzyskoKamienna, today), new FilmwebShowtimesClient(http, 3064, KinoGornik, today = today), new FilmwebShowtimesClient(http, 1913, KinoKozienickiDomKultury, today = today), new SystemBiletowyClient(http, "https://shd.systembiletowy.pl", KinoKuznica), new FilmwebShowtimesClient(http, 2342, KinoSwitZwolen, today = today)),
    "torun" -> Seq(new FilmwebShowtimesClient(http, 3119, KinoMiejskieCentrumKultury, today = today), new FilmwebShowtimesClient(http, 1771, KinoZdroj, today = today)),
    "kielce" -> Seq(new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/centrum-kultury-w-jedrzejowie-1458", KinoCK), new FilmwebShowtimesClient(http, 3128, KinoKoneckieCentrumKultury, today = today)),
    "rzeszow" -> Seq(new FilmwebShowtimesClient(http, 2014, HeliosKrosno, today = today), new FilmwebShowtimesClient(http, 1122, KinoArtKino, today = today), new KinoJednoscClient(http, KinoJednosc), new FilmwebShowtimesClient(http, 2344, KinoMCK, today = today), new FilmwebShowtimesClient(http, 1500, KinoSniezka, today = today), new KinoSokolBrzozowClient(http, KinoSokolBrzozow), new MsiClient(http, "https://bilety-kino.przeworsk.um.gov.pl", KinoWarszawa, today)),
    "gliwice" -> Seq(new FilmwebShowtimesClient(http, 1494, KinoScenaKultura, today = today)),
    "olsztyn" -> Seq(new FilmwebShowtimesClient(http, 2357, KinoCinemaLumiere, today = today), new FilmwebShowtimesClient(http, 2354, KinoIgnacy, today = today), new FilmwebShowtimesClient(http, 2355, KinoNarie, today = today)),
    "bielsko-biala" -> Seq(new FilmwebShowtimesClient(http, 157, KinoJanosik, today = today), new FilmwebShowtimesClient(http, 3248, KinoPckulKino, today = today), new FilmwebShowtimesClient(http, 135, KinoSwitCzechowiceDziedzice, today = today), new FilmwebShowtimesClient(http, 3141, KinoTeatrElektryczny, today = today), new FilmwebShowtimesClient(http, 1490, KinoWislaBrzeszcze, today = today), new FilmwebShowtimesClient(http, 1776, MultikinoCzechowiceDziedzice, today = today)),
    "opole" -> Seq(new FilmwebShowtimesClient(http, 1703, HeliosKedzierzynKozle, today = today), new FilmwebShowtimesClient(http, 2320, KinoBajkaKluczbork, today = today), new FilmwebShowtimesClient(http, 619, KinoChemik, today = today), new FilmwebShowtimesClient(http, 2343, KinoDiana, today = today), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/krapkowicki-dom-kultury-1244", KinoKrapkowice), new FilmwebShowtimesClient(http, 431, KinoStudio, today = today), new FilmwebShowtimesClient(http, 1672, KinoTwierdza, today = today)),
    "rybnik" -> Seq(new FilmwebShowtimesClient(http, 2326, HeliosZory, today = today), new FilmwebShowtimesClient(http, 168, KinoBaltyk, today = today), new FilmwebShowtimesClient(http, 514, KinoCentrum, today = today), new FilmwebShowtimesClient(http, 588, KinoNaStarowce, today = today), new BiletynaClient(bnFetch, "https://biletyna.pl/Wodzislaw-Slaski/Wodzislawskie-Centrum-Kultury", KinoPegaz), new TeatrZiemiRybnickiejClient(http)),
    "elblag" -> Seq(new FilmwebShowtimesClient(http, 1673, HeliosTczew, today = today), new FilmwebShowtimesClient(http, 2352, KinoBaszta, today = today), new MsiClient(http, "https://kinosztumbilety.pl", KinoPowisle, today), new FilmwebShowtimesClient(http, 3131, KinoZulawskiOsrodekKultury, today = today)),
    "koszalin" -> Seq(new FilmwebShowtimesClient(http, 255, KinoBajkaDarlowo, today = today), new FilmwebShowtimesClient(http, 1675, KinoCentrumBialogard, today = today), new FilmwebShowtimesClient(http, 2365, KinoDK, today = today), new MsiClient(http, "https://bilety.goktychowo.pl", KinoGOK, today), new FilmwebShowtimesClient(http, 1864, KinoGoplana, today = today), new FilmwebShowtimesClient(http, 276, KinoWybrzeze, today = today)),
    "kalisz" -> Seq(new FilmwebShowtimesClient(http, 2372, HeliosOstrowWlkp, today = today), new FilmwebShowtimesClient(http, 1513, KinoCentrum3D, today = today), new FilmwebShowtimesClient(http, 1484, KinoEcho, today = today), new FilmwebShowtimesClient(http, 2359, KinoPiastOstrzeszow, today = today), new FilmwebShowtimesClient(http, 1121, KinoPrzedwiosnieKrotoszyn, today = today)),
    "zielona-gora" -> Seq(new FilmwebShowtimesClient(http, 1955, KinoEuropa, today = today), new FilmwebShowtimesClient(http, 2171, KinoMaxKino, today = today), new FilmwebShowtimesClient(http, 1430, KinoPionierZary, today = today), new FilmwebShowtimesClient(http, 2331, KinoSDKSwiebodzin, today = today)),
    "tychy" -> Seq(new FilmwebShowtimesClient(http, 1480, KinoNaszeKino, today = today), new FilmwebShowtimesClient(http, 1528, KinoPlanetCinema, today = today)),
    "walbrzych" -> Seq(new MsiClient(http, "https://bilety.nowaruda.pl", KinoMOKNowaRuda, today), new FilmwebShowtimesClient(http, 197, KinoMOKiS, today = today), new KinoSlezaClient(http, KinoSleza), new FilmwebShowtimesClient(http, 1530, KinoZbyszek, today = today), new FilmwebShowtimesClient(http, 2990, MultikinoKlodzko, today = today), new FilmwebShowtimesClient(http, 2993, MultikinoSwidnica, today = today)),
    "tarnow" -> Seq(new SystemBiletowyClient(http, "https://kfb.systembiletowy.pl", KinoFarys), new FilmwebShowtimesClient(http, 2411, KinoGCK, today = today), new FilmwebShowtimesClient(http, 2404, KinoKolory, today = today), new FilmwebShowtimesClient(http, 1481, KinoPlaneta, today = today), new FilmwebShowtimesClient(http, 2419, KinoPromien, today = today), new FilmwebShowtimesClient(http, 1294, KinoRegis, today = today), new FilmwebShowtimesClient(http, 1488, KinoSokolDabrowaTarnowska, today = today)),
    "wloclawek" -> Seq(new FilmwebShowtimesClient(http, 2341, KinoJutrzenka, today = today), new FilmwebShowtimesClient(http, 3130, KinoNawojka, today = today), new MsiClient(http, "https://bilety.mck-gostynin.pl", KinoNoweKinoWarszawa, today), new FilmwebShowtimesClient(http, 1949, KinoZaRogiem, today = today)),
    "legnica" -> Seq(new FilmwebShowtimesClient(http, 1420, HeliosLubin, today = today), new KinoAurumClient(http, KinoAurum), new CyfroweKinoClient(http, KinoCyfroweKino), new FilmwebShowtimesClient(http, 373, KinoForumBoleslawiec, today = today), new FilmwebShowtimesClient(http, 288, KinoMuzaLubin, today = today), new FilmwebShowtimesClient(http, 1139, KinoPCA, today = today)),
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

  /** Union of every cinema scraper's HTTP hosts. `MonitoringHttpFetch`
   *  suppresses per-host uptime rows for these — each cinema's health is
   *  already tracked under its `displayName` by `UptimeRecordingScraper`, so a
   *  per-host row would be a duplicate landing in the uptime page's "Other"
   *  bucket. Single source of truth: a new cinema's client declares its host
   *  (forced by the abstract `CinemaScraper.scrapeHosts`) and is suppressed
   *  automatically — no hand-kept host list to drift. */
  val scrapeHosts: Set[String] = all.flatMap(_.scrapeHosts).toSet
}
