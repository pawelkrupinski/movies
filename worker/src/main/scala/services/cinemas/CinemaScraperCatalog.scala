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
 * Takes the seams the worker (and its fixture-replay test wiring) vary:
 *   - `http`     — the shared `HttpFetch` every cinema fetches through.
 *   - `mkFetch`  — Multikino's fetch path, passed by `WorkerWiring` (production
 *                  routes it through Zyte via `MultikinoClient.fetchFor`; the
 *                  fixture wiring overrides it back to `http`). A diagnostic that
 *                  doesn't care uses the secondary constructor below, which
 *                  defaults `mkFetch` to the Zyte-routed path.
 *   - `zyteFetch` — Zyte residential egress for venues whose origin firewall
 *                  blocks BOTH our Fly datacenter IP AND the Decodo proxy's
 *                  (datacenter-flavoured) ISP IPs. Kino Kryterium /
 *                  bilety.ck105.koszalin.pl times out the connection from Fly and
 *                  from every Decodo IP, but Zyte's true-residential network gets
 *                  through. `WorkerWiring` routes it through Zyte; the diagnostic
 *                  ctor defaults it to `ZyteFallback.fetchFor(http)`, and the
 *                  fixture wiring overrides it back to `http`.
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
  // Builds the per-chain detail-page cache (Helios / Cinema City). The worker
  // injects a Mongo-backed cache so chain detail is deduped across servers; the
  // diagnostic ctor + tests default to the in-process CachingDetailFetch.
  chainDetailCache: (HttpFetch, FiniteDuration) => HttpFetch,
  // Zyte residential egress for venues whose firewall blocks both our Fly IP and
  // the Decodo proxy (see the ctor doc). No primary-ctor default — Scala can't
  // reference `http` here — so the secondary ctor and WorkerWiring supply it.
  zyteFetch: HttpFetch
) {

  /** Diagnostic ctor: the Zyte-routed fetches (Multikino's API, biletyna's venue
   *  pages) default to the path derived from `http` (a clean body-derived
   *  default, not the old `null`-parameter workaround — Scala can't reference `http`
   *  in a primary-constructor default, but a secondary constructor can).
   *  `WorkerWiring` uses the primary ctor to inject its (possibly
   *  fixture-overridden) `multikinoFetch` / `biletynaFetch`. */
  def this(http: HttpFetch, today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))) =
    this(http, MultikinoClient.fetchFor(http), ZyteFallback.fetchFor(http), today,
      (h, ttl) => new CachingDetailFetch(h, ttl), zyteFetch = ZyteFallback.fetchFor(http))

  // Per-film detail bodies are static between passes and IDENTICAL across a
  // chain's locations, so each chain shares ONE CachingDetailFetch: a film's
  // detail (Helios `/api/movie/{id}`, Cinema City film page) is fetched once per
  // chain per TTL instead of once per location per pass. Live listing/screening
  // fetches stay on `http`. Helios refreshes detail more eagerly (2h) than
  // Cinema City (6h).
  val heliosDetailTtl:     FiniteDuration = 2.hours
  val cinemaCityDetailTtl: FiniteDuration = 6.hours
  private val heliosDetailHttp:     HttpFetch = chainDetailCache(http, heliosDetailTtl)
  private val cinemaCityDetailHttp: HttpFetch = chainDetailCache(http, cinemaCityDetailTtl)
  private def helios(config: HeliosCinema): HeliosClient =
    new HeliosClient(http, config, today, Some(heliosDetailHttp))

  // Shared per-source helper clients the scrapers below reuse.
  val cinemaCityClient: CinemaCityClient = new CinemaCityClient(http, Some(cinemaCityDetailHttp))
  // One per Cinema City venue.
  private def cinemaCity(cinemaId: String, cinema: Cinema): CinemaCityScraper =
    new CinemaCityScraper(cinemaCityClient, cinemaId, cinema)
  val kinoMuzaClient:   KinoMuzaClient   = new KinoMuzaClient(http, today)

  private val poznanScrapers: Seq[CinemaScraper] = Seq(
    new MultikinoClient(mkFetch),
    new CharlieMonroeClient(http),
    new KinoPalacoweClient(http),
    helios(HeliosNuxt.Poznan),
    cinemaCity("1078", CinemaCityPoznanPlaza),
    cinemaCity("1081", CinemaCityKinepolis),
    kinoMuzaClient,
    new KinoBulgarskaClient(http, today),
    new KinoApolloClient(http),
    new RialtoClient(http),
  )

  private val wroclawScrapers: Seq[CinemaScraper] = Seq(
    cinemaCity("1097", CinemaCityWroclavia),
    cinemaCity("1067", CinemaCityKorona),
    new MultikinoClient(mkFetch, "0010", MultikinoPasazGrunwaldzki),
    helios(HeliosNuxt.Magnolia),
    helios(HeliosNuxt.AlejaBielany),
    new NoweHoryzontyClient(http, today),
    new DcfClient(http),
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
    new MuranowClient(http, today),
    new Bilety24Client(http, "https://kinoluna.bilety24.pl", KinoLuna),
    new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-elektronik-631", KinoElektronik),
    new IluzjonClient(http, today),
    new KinoGramClient(http),
    new KinoKulturaClient(http),
    new AmondoClient(http),
    new BokClient(http, "kino-na-boku", KinoNaBoku, today),
    new BokClient(http, "kino-glebocka-66", KinoGlebocka66, today),
    new KinomuzeumClient(http, today),
    new SwitClient(http),
    new PromKepaClient(http),
    new FalenicaClient(http),
    new SdkClient(http),
    new NoveKinoClient(http, "atlantic", KinoAtlantic),
    new KinotekaClient(http),
    new UjazdowskiClient(http, today),
    new CytadelaClient(http),
    new NoveKinoClient(http, "wisla", KinoWisla),
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
    new KinoMikroClient(http, "Kino Mikro", KinoMikro, today),
    new KinoMikroClient(http, "Mikro Bronowice", MikroBronowice, today),
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
    new KinoTatryClient(http, KinoTatry, today),
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
    new KinoIkmClient(http, KinoIkm, today),
    new KinoMuzeumGdanskClient(http, KinoMuzeumGdansk),
    new KinoZakClient(http, KinoZak, today),
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
    new KinoZorzaClient(http, KinoZorza, today),
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
  private val olsztynScrapers      = Seq(helios(HeliosNuxt.Olsztyn), new MultikinoClient(mkFetch, "0036", MultikinoOlsztyn), new KinoAwangarda2Client(http, today))
  private val bielskoBialaScrapers = Seq(helios(HeliosNuxt.BielskoBiala), cinemaCity("1088", CinemaCityBielskoBiala), new KinoKreskaClient(http, KinoKreska, today))
  private val opoleScrapers        = Seq(helios(HeliosNuxt.OpoleKarolinka), helios(HeliosNuxt.OpoleSolaris), new EkobiletClient(http, "opolskielamy", KinoMeduza, today))
  private val rybnikScrapers       = Seq(new MultikinoClient(mkFetch, "0014", MultikinoRybnik), cinemaCity("1082", CinemaCityRybnik))
  private val gorzowScrapers       = Seq(helios(HeliosNuxt.Gorzow), new MultikinoClient(mkFetch, "0047", MultikinoGorzow), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/miejski-osrodek-sztuki-kino-60-krzesel-dkf-megaron-776", Kino60Krzesel))
  private val elblagScrapers       = Seq(new MultikinoClient(mkFetch, "0037", MultikinoElblag), cinemaCity("1099", CinemaCityElblag))
  private val koszalinScrapers     = Seq(helios(HeliosNuxt.Koszalin), new MultikinoClient(mkFetch, "0015", MultikinoKoszalin), new MsiClient(zyteFetch, "https://bilety.ck105.koszalin.pl", KinoKryterium, today))
  private val kaliszScrapers       = Seq(helios(HeliosNuxt.Kalisz), new MultikinoClient(mkFetch, "0042", MultikinoKalisz))
  private val zielonaGoraScrapers  = Seq(cinemaCity("1087", CinemaCityZielonaGora))
  private val tychyScrapers        = Seq(new MultikinoClient(mkFetch, "0053", MultikinoTychy))
  private val walbrzychScrapers    = Seq(cinemaCity("1091", CinemaCityWalbrzych), new Bilety24Client(http, "https://kino-apollo.bilety24.pl", KinoApolloWalbrzych))
  private val tarnowScrapers       = Seq(new MultikinoClient(mkFetch, "0050", MultikinoTarnow), new MsiClient(http, "https://bilety.csm.tarnow.pl", KinoMillenium, today, mvcPath = "/Kino/mvc/pl"))
  private val wloclawekScrapers    = Seq(new MultikinoClient(mkFetch, "0008", MultikinoWloclawek))
  private val legnicaScrapers      = Seq(helios(HeliosNuxt.Legnica), new Bilety24Client(http, "https://kino-piast.bilety24.pl", KinoPiast))
  private val plockScrapers        = Seq(helios(HeliosNuxt.Plock), new NoveKinoClient(http, "przedwiosnie", KinoPrzedwiosnie))
  private val bytomScrapers        = Seq(cinemaCity("1092", CinemaCityBytom))
  private val dabrowaGorniczaScrapers = Seq(helios(HeliosNuxt.DabrowaGornicza), new VisualTicketClient(http, "https://bilety.palac.art.pl", KinoKadr, locationId = 2))
  private val nowySaczScrapers     = Seq(helios(HeliosNuxt.NowySacz), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/malopolskie-centrum-kultury-sokol-w-nowym-saczu-1225", KinoSokol))
  private val slupskScrapers       = Seq(new MultikinoClient(mkFetch, "0030", MultikinoSlupsk), new EkobiletClient(http, "kinorejs", KinoRejs, today))
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
    new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/koninskie-centrum-kultury-1626", KinoNadWarta),   // Koło
    new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-hel-dom-kultury-w-pleszewie-1255", KinoHel),   // Pleszew
    new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-w-slupcy-1423", KinoSokolnia),   // Słupca
    new BiletynaClient(bnFetch, "https://biletyna.pl/Turek/Kino-Tur", KinoTur),   // Turek
    new BiletynaClient(bnFetch, "https://biletyna.pl/Zagorow/Gminny-Osrodek-Kultury", KinoMok),   // Zagórów
  )

  /** Raw scrapers grouped by city slug — same slugs `City.slug` uses, so a
   *  caller can scope by city without re-spelling the membership. */
  // Catchment cinemas (nearby towns) and a few in-city venues that came in via a
  // Filmweb sweep, each by Filmweb internal cinema id — except the handful that
  // have since moved to their own site (Kino Spójnia, Kino Praha), which fed
  // Filmweb too thinly. Merged into byCity below so every city's catchment is
  // scraped without touching its hand-written scraper group.
  private val filmwebExtra: Map[String, Seq[CinemaScraper]] = Map(
    "wroclaw" -> Seq(new Bilety24SubdomainClient(http, "https://kulturalne-oborniki.bilety24.pl/repertuar/", KinoAstra, today = today), new FilmwebShowtimesClient(http, 1645, KinoDyskusyjnyKlubFilmowyPolitechnika, today = today)),
    "warszawa" -> Seq(new PrahaClient(http, KinoMazowieckiTeatrMuzycznyImJanaKiepuryKinoPraha)),
    "lodz" -> Seq(new KinoSpojniaClient(http, KinoSpojnia), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/miejski-osrodek-kultury-stary-mlyn-w-zgierzu-1697", KinoStaryMlyn)),
    "katowice" -> Seq(cinemaCity("1062", CinemaCity), new KinoPatriaClient(http, KinoPatria, today)),
    "szczecin" -> Seq(new SystemBiletowyClient(http, "https://kgl.systembiletowy.pl", KinoKawiarnia), new BiletynaClient(bnFetch, "https://biletyna.pl/Pyrzyce/Pyrzycki-Dom-Kultury", KinoPDK), new BiletynaClient(bnFetch, "https://biletyna.pl/Stargard/Stargardzkie-Centrum-Kultury", KinoSCK)),
    "bialystok" -> Seq(new BiletynaClient(bnFetch, "https://biletyna.pl/Sokolka/Kino-Sokol", KinoSokolSokolka)),
    "trojmiasto" -> Seq(new BiletynaClient(bnFetch, "https://biletyna.pl/Gdansk/Kino-na-Szekspirowskim", KinoNaSzekspirowskim), new MultikinoClient(mkFetch, "0027", MultikinoRumia)),
    "bydgoszcz" -> Seq(new MsiClient(http, "https://bilety.kinomax.info.pl", KinoKinomax, today), new BiletynaClient(bnFetch, "https://biletyna.pl/Chelmno/Kinoteatr-Rondo", KinoRondo)),
    "lublin" -> Seq(new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-lewart-w-lubartowie-1382", KinoLewart), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/centrum-kultury-i-promocji-w-krasniku-1529", KinoMetalowiec)),
    "czestochowa" -> Seq(new KinoDKFRumcajsClient(http, KinoDKFRumcajs, today = today), new KinoKarolinkaClient(http, KinoKarolinka), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-w-radomsku-1546", KinoMDK), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/miejski-osrodek-kultury-centrum-im-adama-mickiewicza-w-zawierciu-1305", KinoMOKCentrum), new BiletynaClient(bnFetch, "https://biletyna.pl/Piekary-Slaskie/Kino-Zacisze", KinoZacisze)),
    "radom" -> Seq(helios(HeliosNuxt.Starachowice), new MsiClient(http, "https://bilet-mck.skarzysko.pl", KinoCentrumSkarzyskoKamienna, today), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-gornik-szydlowiec-1320", KinoGornik), new MsiClient(http, "https://bilety.dkkozienice.pl", KinoKozienickiDomKultury, today), new SystemBiletowyClient(http, "https://shd.systembiletowy.pl", KinoKuznica), new MsiClient(http, "https://bilety.switzwolen.pl", KinoSwitZwolen, today)),
    "torun" -> Seq(new BiletynaClient(bnFetch, "https://biletyna.pl/Aleksandrow-Kujawski/Miejskie-Centrum-Kultury", KinoMiejskieCentrumKultury), new BiletynaClient(bnFetch, "https://biletyna.pl/Ciechocinek/Kino-Zdroj", KinoZdroj)),
    "kielce" -> Seq(new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/centrum-kultury-w-jedrzejowie-1458", KinoCK), new BiletynaClient(bnFetch, "https://biletyna.pl/Konskie/Koneckie-Centrum-Kultury-sala-kinowa", KinoKoneckieCentrumKultury)),
    "rzeszow" -> Seq(helios(HeliosNuxt.Krosno), new FilmwebShowtimesClient(http, 1122, KinoArtKino, today = today), new KinoJednoscClient(http, KinoJednosc), new MsiClient(http, "https://bilety.kinolezajsk.pl", KinoMCK, today), new MsiClient(http, "https://bilety.mokdebica.pl", KinoSniezka, today), new KinoSokolBrzozowClient(http, KinoSokolBrzozow), new MsiClient(http, "https://bilety-kino.przeworsk.um.gov.pl", KinoWarszawa, today)),
    "gliwice" -> Seq(new KinoScenaKulturaClient(http, KinoScenaKultura)),
    "olsztyn" -> Seq(new MsiClient(http, "https://bilety.kinoszczytno.pl", KinoCinemaLumiere, today), new MsiClient(http, "https://www.biletyignacy.pl", KinoIgnacy, today), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-narie-w-moragu-1682", KinoNarie)),
    "bielsko-biala" -> Seq(new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-janosik-1500", KinoJanosik), new SystemBiletowyClient(http, "https://bilety.pckul.pl", KinoPckulKino), new BiletynaClient(bnFetch, "https://biletyna.pl/Czechowice-Dziedzice/Kino-Swit", KinoSwitCzechowiceDziedzice), new BiletynaClient(bnFetch, "https://biletyna.pl/Skoczow/Teatr-Elektryczny", KinoTeatrElektryczny), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-wisla-w-brzeszczach-1539", KinoWislaBrzeszcze), new MultikinoClient(mkFetch, "0033", MultikinoCzechowiceDziedzice)),
    "opole" -> Seq(helios(HeliosNuxt.KedzierzynKozle), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-bajka-w-kluczborku-1467", KinoBajkaKluczbork), new MsiClient(http, "https://bilety.mok.com.pl", KinoChemik, today, titlePrefix = Some("Chemik")), new KinoDianaClient(http, KinoDiana), new KdkKrapkowiceClient(http, KinoKrapkowice), new KinoStudioClient(http, KinoStudio, today), new MsiClient(http, "https://bilety.mok.com.pl", KinoTwierdza, today, titlePrefix = Some("TWIERDZA"))),
    "rybnik" -> Seq(helios(HeliosNuxt.Zory), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-baltyk-1499", KinoBaltyk), new EkobiletClient(http, "kino-centrum-jastrzebiezdrj", KinoCentrum, today), new SystemBiletowyClient(http, "https://bilety.mok.zory.pl", KinoNaStarowce), new BiletynaClient(bnFetch, "https://biletyna.pl/Wodzislaw-Slaski/Wodzislawskie-Centrum-Kultury", KinoPegaz), new TeatrZiemiRybnickiejClient(http)),
    "elblag" -> Seq(helios(HeliosNuxt.Tczew), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-baszta-477", KinoBaszta), new MsiClient(http, "https://kinosztumbilety.pl", KinoPowisle, today), new BiletynaClient(bnFetch, "https://biletyna.pl/Nowy-Dwor-Gdanski/Zulawski-Osrodek-Kultury", KinoZulawskiOsrodekKultury)),
    "koszalin" -> Seq(new MsiClient(http, "https://darlowo.vectorsoft.pl", KinoBajkaDarlowo, today), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-centrum-w-bialogardzie-1685", KinoCentrumBialogard), new BiletynaClient(bnFetch, "https://biletyna.pl/Slawno/Slawienski-Dom-Kultury", KinoDK), new MsiClient(http, "https://bilety.goktychowo.pl", KinoGOK, today), new MsiClient(http, "https://bilety.ckpolczyn.pl", KinoGoplana, today), new MsiClient(http, "https://bilety.rck.kolobrzeg.pl", KinoWybrzeze, today, titleSuffix = Some("KINO WYBRZEŻE"))),
    "kalisz" -> Seq(helios(HeliosNuxt.OstrowWielkopolski), new SystemBiletowyClient(http, "https://kck.systembiletowy.pl", KinoCentrum3D), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-echo-w-jarocinie-1159", KinoEcho), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-piast-w-ostrzeszowie-601", KinoPiastOstrzeszow), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/krotoszynski-osrodek-kultury-1668", KinoPrzedwiosnieKrotoszyn)),
    "zielona-gora" -> Seq(new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/nowosolski-dom-kultury-1679", KinoEuropa), new MsiClient(http, "https://repertuar.maxkino.eu", KinoMaxKino, today), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-pionier-w-zarach-1492", KinoPionierZary), new MsiClient(http, "https://bilety.kino.swiebodzin.pl:4433", KinoSDKSwiebodzin, today)),
    "tychy" -> Seq(new SystemBiletowyClient(http, "https://ock.systembiletowy.pl", KinoNaszeKino), new MsiClient(http, "https://oswiecim.planetcinema.pl", KinoPlanetCinema, today)),
    "walbrzych" -> Seq(new MsiClient(http, "https://bilety.nowaruda.pl", KinoMOKNowaRuda, today), new EkobiletClient(http, "mokis-bielawa", KinoMOKiS, today), new KinoSlezaClient(http, KinoSleza), new BiletynaClient(bnFetch, "https://biletyna.pl/Dzierzoniow/Kinoteatr-Zbyszek", KinoZbyszek), new MultikinoClient(mkFetch, "0041", MultikinoKlodzko), new MultikinoClient(mkFetch, "0043", MultikinoSwidnica)),
    "tarnow" -> Seq(new SystemBiletowyClient(http, "https://kfb.systembiletowy.pl", KinoFarys), new BiletynaClient(bnFetch, "https://biletyna.pl/Solec-Zdroj/Kino-Solec-Zdroj", KinoGCK), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/gorlickie-centrum-kultury-1581", KinoKolory), new MsiClient(http, "https://rezerwacja.planetabrzesko.pl", KinoPlaneta, today, mvcPath = "/Rezerwacja/mvc/pl"), new KinoPromienClient(http, KinoPromien, today), new SystemBiletowyClient(http, "https://bilety.kino.bochnia.pl", KinoRegis), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-sokol-w-dabrowie-tarnowskiej-1303", KinoSokolDabrowaTarnowska)),
    "wloclawek" -> Seq(new MsiClient(http, "https://kino.sierpc.pl", KinoJutrzenka, today), new BiletynaClient(bnFetch, "https://biletyna.pl/Lipno/Kino-Nawojka", KinoNawojka), new MsiClient(http, "https://bilety.mck-gostynin.pl", KinoNoweKinoWarszawa, today), new MsiClient(http, "https://bilety.pokis.pl", KinoZaRogiem, today)),
    "legnica" -> Seq(helios(HeliosNuxt.Lubin), new KinoAurumClient(http, KinoAurum), new CyfroweKinoClient(http, KinoCyfroweKino), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/boleslawiecki-osrodek-kultury-miedzynarodowe-centrum-ceramiki-kino-forum-1586", KinoForumBoleslawiec), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/centrum-kultury-muza-w-lubinie-1375", KinoMuzaLubin), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/centrum-kultury-w-polkowicach-1689", KinoPCA)),
    "plock" -> Seq(new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-kutnowskiego-domu-kultury-1474", KinoKDK), new SystemBiletowyClient(http, "https://ckp.systembiletowy.pl", KinoKalejdoskop), new MsiClient(http, "https://kinoodeon.eurobilet.pl", KinoODEON, today)),
    "nowy-sacz" -> Seq(new EkobiletClient(http, "kino-jaworzyna", KinoJaworzyna, today), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/limanowski-dom-kultury-1368", KinoKlaps)),
    "slupsk" -> Seq(new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/leborskie-centrum-kultury-kino-fregata-1683", KinoFregata)),
    "jelenia-gora" -> Seq(new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-wawel-w-lubaniu-1489", KinoWawel)),
    "przemysl" -> Seq(new FilmwebShowtimesClient(http, 1786, KinoCentrum3DPrzemysl, today = today), new MsiClient(http, "https://kinoikar.mok-jar.pl", KinoIkar, today), new MsiClient(http, "https://jaroslaw.kinonabiegunach.pl", KinoNaBiegunach, today), new KinoSDKSanokClient(http, KinoSDK, today)),
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
