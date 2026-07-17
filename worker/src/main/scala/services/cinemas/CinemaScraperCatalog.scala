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
    new KinoChatkaZakaClient(http, KinoChatkaZaka),
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
    // iframe639.biletyna.pl 403s our Fly datacenter IP (Cloudflare) on the
    // per-film /artist/view/id detail pages, so route through the biletyna seam
    // (residential proxy → Zyte) like every other biletyna venue — else the
    // deferred detail enrichment fetches all 403 and the enrichment bar goes red.
    new KinoFenomenClient(bnFetch, KinoFenomen),
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
    "szczecin" -> Seq(new SystemBiletowyClient(http, "https://kgl.systembiletowy.pl", KinoKawiarnia), new BiletynaClient(bnFetch, "https://biletyna.pl/Pyrzyce/Pyrzycki-Dom-Kultury", KinoPDK), new SckStargardClient(http, KinoSCK)),
    "bialystok" -> Seq(new KinoSokolSokolkaClient(http, KinoSokolSokolka)),
    "trojmiasto" -> Seq(new BiletynaClient(bnFetch, "https://biletyna.pl/Gdansk/Kino-na-Szekspirowskim", KinoNaSzekspirowskim), new MultikinoClient(mkFetch, "0027", MultikinoRumia)),
    "bydgoszcz" -> Seq(new MsiClient(http, "https://bilety.kinomax.info.pl", KinoKinomax, today), new BiletynaClient(bnFetch, "https://biletyna.pl/Chelmno/Kinoteatr-Rondo", KinoRondo)),
    "lublin" -> Seq(new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-lewart-w-lubartowie-1382", KinoLewart), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/centrum-kultury-i-promocji-w-krasniku-1529", KinoMetalowiec)),
    "czestochowa" -> Seq(new KinoDKFRumcajsClient(http, KinoDKFRumcajs, today = today), new KinoKarolinkaClient(http, KinoKarolinka), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-w-radomsku-1546", KinoMDK), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/miejski-osrodek-kultury-centrum-im-adama-mickiewicza-w-zawierciu-1305", KinoMOKCentrum), new KinoZaciszeClient(http, KinoZacisze)),
    "radom" -> Seq(helios(HeliosNuxt.Starachowice), new MsiClient(http, "https://bilet-mck.skarzysko.pl", KinoCentrumSkarzyskoKamienna, today), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-gornik-szydlowiec-1320", KinoGornik), new MsiClient(http, "https://bilety.dkkozienice.pl", KinoKozienickiDomKultury, today), new SystemBiletowyClient(http, "https://shd.systembiletowy.pl", KinoKuznica), new MsiClient(http, "https://bilety.switzwolen.pl", KinoSwitZwolen, today)),
    "torun" -> Seq(new BiletynaClient(bnFetch, "https://biletyna.pl/Aleksandrow-Kujawski/Miejskie-Centrum-Kultury", KinoMiejskieCentrumKultury), new BiletynaClient(bnFetch, "https://biletyna.pl/Ciechocinek/Kino-Zdroj", KinoZdroj)),
    "kielce" -> Seq(new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/centrum-kultury-w-jedrzejowie-1458", KinoCK), new BiletynaClient(bnFetch, "https://biletyna.pl/Konskie/Koneckie-Centrum-Kultury-sala-kinowa", KinoKoneckieCentrumKultury)),
    "rzeszow" -> Seq(helios(HeliosNuxt.Krosno), new ArtKinoKrosnoClient(http, KinoArtKino, today), new KinoJednoscClient(http, KinoJednosc), new MsiClient(http, "https://bilety.kinolezajsk.pl", KinoMCK, today), new MsiClient(http, "https://bilety.mokdebica.pl", KinoSniezka, today), new KinoSokolBrzozowClient(http, KinoSokolBrzozow), new MsiClient(http, "https://bilety-kino.przeworsk.um.gov.pl", KinoWarszawa, today)),
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
    "walbrzych" -> Seq(new MsiClient(http, "https://bilety.nowaruda.pl", KinoMOKNowaRuda, today), new EkobiletClient(http, "mokis-bielawa", KinoMOKiS, today), new KinoSlezaClient(http, KinoSleza), new KinoZbyszekClient(http, KinoZbyszek), new MultikinoClient(mkFetch, "0041", MultikinoKlodzko), new MultikinoClient(mkFetch, "0043", MultikinoSwidnica)),
    "tarnow" -> Seq(new SystemBiletowyClient(http, "https://kfb.systembiletowy.pl", KinoFarys), new BiletynaClient(bnFetch, "https://biletyna.pl/Solec-Zdroj/Kino-Solec-Zdroj", KinoGCK), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/gorlickie-centrum-kultury-1581", KinoKolory), new MsiClient(http, "https://rezerwacja.planetabrzesko.pl", KinoPlaneta, today, mvcPath = "/Rezerwacja/mvc/pl"), new KinoPromienClient(http, KinoPromien, today), new SystemBiletowyClient(http, "https://bilety.kino.bochnia.pl", KinoRegis), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-sokol-w-dabrowie-tarnowskiej-1303", KinoSokolDabrowaTarnowska)),
    "wloclawek" -> Seq(new MsiClient(http, "https://kino.sierpc.pl", KinoJutrzenka, today), new BiletynaClient(bnFetch, "https://biletyna.pl/Lipno/Kino-Nawojka", KinoNawojka), new MsiClient(http, "https://bilety.mck-gostynin.pl", KinoNoweKinoWarszawa, today), new MsiClient(http, "https://bilety.pokis.pl", KinoZaRogiem, today)),
    "legnica" -> Seq(helios(HeliosNuxt.Lubin), new KinoAurumClient(http, KinoAurum), new CyfroweKinoClient(http, KinoCyfroweKino), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/boleslawiecki-osrodek-kultury-miedzynarodowe-centrum-ceramiki-kino-forum-1586", KinoForumBoleslawiec), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/centrum-kultury-muza-w-lubinie-1375", KinoMuzaLubin), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/centrum-kultury-w-polkowicach-1689", KinoPCA)),
    "plock" -> Seq(new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-kutnowskiego-domu-kultury-1474", KinoKDK), new SystemBiletowyClient(http, "https://ckp.systembiletowy.pl", KinoKalejdoskop), new MsiClient(http, "https://kinoodeon.eurobilet.pl", KinoODEON, today)),
    "nowy-sacz" -> Seq(new EkobiletClient(http, "kino-jaworzyna", KinoJaworzyna, today), new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/limanowski-dom-kultury-1368", KinoKlaps)),
    "slupsk" -> Seq(new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/leborskie-centrum-kultury-kino-fregata-1683", KinoFregata)),
    "jelenia-gora" -> Seq(new Bilety24OrganizerClient(http, "https://www.bilety24.pl/kino/organizator/kino-wawel-w-lubaniu-1489", KinoWawel)),
    "przemysl" -> Seq(new FilmwebShowtimesClient(http, 1786, KinoCentrum3DPrzemysl, today = today), new MsiClient(http, "https://kinoikar.mok-jar.pl", KinoIkar, today), new MsiClient(http, "https://jaroslaw.kinonabiegunach.pl", KinoNaBiegunach, today), new KinoSDKSanokClient(http, KinoSDK, today)),
  )

  // ── United Kingdom (Flicks) ──────────────────────────────────────────────
  private def flicks(slug: String, cinema: Cinema): FlicksClient =
    new FlicksClient(http, slug, cinema, today = today)
  private val londonScrapers: Seq[CinemaScraper] = Seq(
    flicks("act-one-acton", ActOneActon),
    flicks("arthouse-crouch-end", ArthouseCrouchEnd),
    flicks("barbican-london-cinema-1", BarbicanLondonCinema1),
    flicks("bfi-london-imax", BfiLondonImax),
    flicks("bfi-london-southbank", BfiLondonSouthbank),
    flicks("castle-cinema-hackney", CastleCinemaHackney),
    flicks("sidcup-storyteller", SidcupStoryteller),
    flicks("chiswick-cinema", ChiswickCinema),
    flicks("cineworld-greenwich", CineworldGreenwich),
    flicks("cineworld-bexleyheath", CineworldBexleyheath),
    flicks("cineworld-enfield", CineworldEnfield),
    flicks("cineworld-feltham", CineworldFeltham),
    flicks("cineworld-ilford", CineworldIlford),
    flicks("cineworld-leicester-square", CineworldLeicesterSquare),
    flicks("cineworld-london-hounslow", CineworldLondonHounslow),
    flicks("cineworld-south-ruislip", CineworldSouthRuislip),
    flicks("cineworld-wandsworth", CineworldWandsworth),
    flicks("cineworld-wembley", CineworldWembley),
    flicks("cineworld-west-india-quay", CineworldWestIndiaQuay),
    flicks("cineworld-wood-green", CineworldWoodGreen),
    flicks("cine-lumiere-london", CineLumiereLondon),
    flicks("close-up-film-centre-shoreditch", CloseUpFilmCentreShoreditch),
    flicks("crouch-end-picturehouse", CrouchEndPicturehouse),
    flicks("curzon-cinema-aldgate", CurzonCinemaAldgate),
    flicks("curzon-cinema-bloomsbury", CurzonCinemaBloomsbury),
    flicks("curzon-cinema-camden", CurzonCinemaCamden),
    flicks("curzon-cinema-hoxton", CurzonCinemaHoxton),
    flicks("curzon-cinema-kingston", CurzonCinemaKingston),
    flicks("curzon-cinema-mayfair", CurzonCinemaMayfair),
    flicks("curzon-cinema-richmond", CurzonCinemaRichmond),
    flicks("curzon-cinema-sea-containers-mondrian", CurzonCinemaSeaContainersMondrian),
    flicks("curzon-cinema-victoria", CurzonCinemaVictoria),
    flicks("curzon-soho", CurzonSoho),
    flicks("curzon-wimbledon", CurzonWimbledon),
    flicks("david-lean-cinema-croydon", DavidLeanCinemaCroydon),
    flicks("electric-cinema-london", ElectricCinemaLondon),
    flicks("electric-cinema-white-city", ElectricCinemaWhiteCity),
    flicks("everyman-at-the-whiteley-london", EverymanAtTheWhiteleyLondon),
    flicks("everyman-brentford", EverymanBrentford),
    flicks("everyman-cinema-baker-street", EverymanCinemaBakerStreet),
    flicks("everyman-cinema-barnet", EverymanCinemaBarnet),
    flicks("everyman-cinema-belsize-park-hampstead", EverymanCinemaBelsizeParkHampstead),
    flicks("everyman-cinema-borough-yards", EverymanCinemaBoroughYards),
    flicks("everyman-cinema-broadgate", EverymanCinemaBroadgate),
    flicks("everyman-cinema-canary-wharf", EverymanCinemaCanaryWharf),
    flicks("everyman-cinema-chelsea", EverymanCinemaChelsea),
    flicks("everyman-cinema-crystal-palace", EverymanCinemaCrystalPalace),
    flicks("everyman-cinema-egham", EverymanCinemaEgham),
    flicks("everyman-cinema-esher", EverymanCinemaEsher),
    flicks("everyman-cinema-hampstead", EverymanCinemaHampstead),
    flicks("everyman-cinema-king-s-cross", EverymanCinemaKingSCross),
    flicks("everyman-cinema-maida-vale", EverymanCinemaMaidaVale),
    flicks("everyman-cinema-muswell-hill", EverymanCinemaMuswellHill),
    flicks("everyman-cinema-stratford-international", EverymanCinemaStratfordInternational),
    flicks("everyman-cinema-walton-on-thames", EverymanCinemaWaltonOnThames),
    flicks("everyman-cinema-islington", EverymanCinemaIslington),
    flicks("finsbury-park-picturehouse", FinsburyParkPicturehouse),
    flicks("forest-cinemas-walthamstow", ForestCinemasWalthamstow),
    flicks("genesis-tower-hamlets", GenesisTowerHamlets),
    flicks("institute-of-contemporary-arts", InstituteOfContemporaryArts),
    flicks("jw3-hampstead", Jw3Hampstead),
    flicks("kiln-kilburn", KilnKilburn),
    flicks("leatherhead-theatre-cinema-leatherhead", LeatherheadTheatreCinemaLeatherhead),
    flicks("lexi-kensal-rise", LexiKensalRise),
    flicks("lumiere-romford", LumiereRomford),
    flicks("nova-cinema-ambassadors-woking", NovaCinemaWoking),
    flicks("odeon-cinema-acton", OdeonCinemaActon),
    flicks("odeon-cinema-beckenham", OdeonCinemaBeckenham),
    flicks("odeon-cinema-epsom", OdeonCinemaEpsom),
    flicks("odeon-cinema-greenwich", OdeonCinemaGreenwich),
    flicks("odeon-cinema-holloway", OdeonCinemaHolloway),
    flicks("odeon-cinema-kingston", OdeonCinemaKingston),
    flicks("odeon-cinema-orpington", OdeonCinemaOrpington),
    flicks("odeon-cinema-richmond", OdeonCinemaRichmond),
    flicks("odeon-cinema-south-woodford", OdeonCinemaSouthWoodford),
    flicks("odeon-cinema-streatham", OdeonCinemaStreatham),
    flicks("odeon-cinema-tottenham-court-road", OdeonCinemaTottenhamCourtRoad),
    flicks("odeon-cinema-uxbridge", OdeonCinemaUxbridge),
    flicks("odeon-cinema-wimbledon", OdeonCinemaWimbledon),
    flicks("odeon-cinema-luxe-haymarket", OdeonCinemaLuxeHaymarket),
    flicks("odeon-luxe-islington", OdeonLuxeIslington),
    flicks("odeon-luxe-lee-valley", OdeonLuxeLeeValley),
    flicks("odeon-cinema-luxe-leicester-square", OdeonCinemaLuxeLeicesterSquare),
    flicks("odeon-cinema-luxe-putney", OdeonCinemaLuxePutney),
    flicks("odeon-luxe-swiss-cottage", OdeonLuxeSwissCottage),
    flicks("odeon-luxe-west-end", OdeonLuxeWestEnd),
    flicks("olympic-cinema-barnes", OlympicCinemaBarnes),
    flicks("empire-cinema-sutton", EmpireCinemaSutton),
    flicks("peckhamplex", Peckhamplex),
    flicks("phoenix-cinema-east-finchley", PhoenixCinemaEastFinchley),
    flicks("picturehouse-central-london", PicturehouseCentralLondon),
    flicks("picturehouse-clapham", PicturehouseClapham),
    flicks("picturehouse-ealing-filmworks", PicturehouseEalingFilmworks),
    flicks("picturehouse-east-dulwich", PicturehouseEastDulwich),
    flicks("picturehouse-epsom-square", PicturehouseEpsomSquare),
    flicks("picturehouse-greenwich", PicturehouseGreenwich),
    flicks("picturehouse-hackney", PicturehouseHackney),
    flicks("picturehouse-west-norwood", PicturehouseWestNorwood),
    flicks("prince-charles-london", PrinceCharlesLondon),
    flicks("regent-street-cinema-london", RegentStreetCinemaLondon),
    flicks("rich-mix-bethnal-green", RichMixBethnalGreen),
    flicks("rio-dalston", RioDalston),
    flicks("riverside-studios-hammersmith", RiversideStudiosHammersmith),
    flicks("rooftop-film-club-peckham-bussey-building", RooftopFilmClubPeckhamBusseyBuilding),
    flicks("rooftop-film-club-stratford-roof-east", RooftopFilmClubStratfordRoofEast),
    flicks("science-museum-london-imax", ScienceMuseumLondonImax),
    flicks("archlight-cinemas", ArchlightCinemas),
    flicks("the-arzner", TheArzner),
    flicks("the-cinema-at-selfridges", TheCinemaAtSelfridges),
    flicks("the-cinema-in-the-power-station", TheCinemaInThePowerStation),
    flicks("the-garden-cinema", TheGardenCinema),
    flicks("the-gate-picturehouse-london", TheGatePicturehouseLondon),
    flicks("the-light-cinemas-addlestone", TheLightCinemasAddlestone),
    flicks("the-nickel-london", TheNickelLondon),
    flicks("the-ritzy-picturehouse-brixton", TheRitzyPicturehouseBrixton),
    flicks("vue-cinemas-bromley", VueCinemasBromley),
    flicks("vue-cinemas-dagenham", VueCinemasDagenham),
    flicks("vue-cinemas-eltham", VueCinemasEltham),
    flicks("vue-cinemas-finchley-road-swiss-cottage", VueCinemasFinchleyRoadSwissCottage),
    flicks("vue-cinemas-fulham", VueCinemasFulham),
    flicks("vue-cinemas-harrow", VueCinemasHarrow),
    flicks("vue-cinemas-islington", VueCinemasIslington),
    flicks("vue-cinemas-finchley", VueCinemasFinchley),
    flicks("vue-cinemas-piccadilly-circus", VueCinemasPiccadillyCircus),
    flicks("vue-cinemas-purley-way-croydon", VueCinemasPurleyWayCroydon),
    flicks("vue-cinemas-romford", VueCinemasRomford),
    flicks("vue-cinemas-staines-upon-thames", VueCinemasStainesUponThames),
    flicks("vue-cinemas-stratford", VueCinemasStratford),
    flicks("vue-cinemas-west-end", VueCinemasWestEnd),
    flicks("vue-cinemas-westfield-shepherd-s-bush", VueCinemasWestfieldShepherdSBush),
    flicks("vue-cinemas-wood-green", VueCinemasWoodGreen),
    flicks("watermans-art-centre-brentford", WatermansArtCentreBrentford),
    flicks("wyllyotts-theatre-potters-bar", WyllyottsTheatrePottersBar),
  )
  private val manchesterScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-ashton-under-lyne", CineworldAshtonUnderLyne),
    flicks("cineworld-manchester", CineworldManchester),
    flicks("cultplex-manchester", CultplexManchester),
    flicks("everyman-manchester-st-johns", EverymanManchesterStJohns),
    flicks("flix-treehouse-manchester", FlixTreehouseManchester),
    flicks("home-manchester", HomeManchester),
    flicks("leigh-film-factory", LeighFilmFactory),
    flicks("northern-light-sale", NorthernLightSale),
    flicks("odeon-cinema-manchester-great-northern", OdeonCinemaManchesterGreatNorthern),
    flicks("odeon-cinema-manchester-trafford-centre", OdeonCinemaManchesterTraffordCentre),
    flicks("odeon-cinema-oldham", OdeonCinemaOldham),
    flicks("empire-cinema-wigan", EmpireCinemaWigan),
    flicks("plaza-stockport", PlazaStockport),
    flicks("reel-cinema-rochdale", ReelCinemaRochdale),
    flicks("regent-marple", RegentMarple),
    flicks("savoy-heaton-moor", SavoyHeatonMoor),
    flicks("the-light-cinemas-stockport", TheLightCinemasStockport),
    flicks("vue-cinemas-manchester-printworks", VueCinemasManchesterPrintworks),
    flicks("vue-cinemas-manchester-quayside", VueCinemasManchesterQuayside),
  )
  private val norwichScrapers: Seq[CinemaScraper] = Seq(
    flicks("arc-cinema-at-the-royalty-great-yarmouth", ArcCinemaGreatYarmouth),
    flicks("central-fakenham", CentralCinemaFakenham),
    flicks("corn-exchange-cinema-king-s-lynn", CornExchangeCinemaKingSLynn),
    flicks("east-coast-cinema-lowestoft", EastCoastCinemaLowestoft),
    flicks("little-theatre-sheringham", LittleTheatreSheringham),
    flicks("majestic-king-s-lynn", MajesticKingSLynn),
    flicks("marina-theatre-lowestoft", MarinaTheatreLowestoft),
    flicks("odeon-cinema-norwich", OdeonNorwich),
    flicks("orion-dereham", OrionDereham),
    flicks("palace-cinema-gorleston-on-sea", PalaceCinemaGorlestonOnSea),
    flicks("cinema-city-picturehouse-norwich", CinemaCityPicturehouseNorwich),
    flicks("regal-movieplex-cromer", RegalMovieplexCromer),
    flicks("the-light-cinemas-thetford", TheLightThetford),
    flicks("vue-cinemas-norwich", VueCinemasNorwich),
  )
  private val aberdeenshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("arc-peterhead", ArcCinemaPeterhead),
    flicks("belmont-filmhouse-aberdeen", BelmontFilmhouse),
    flicks("cineworld-queens-link-aberdeen", CineworldQueensLinkAberdeen),
    flicks("cineworld-union-square-aberdeen", CineworldUnionSquareAberdeen),
    flicks("moray-playhouse-elgin", MorayPlayhouse),
    flicks("number-30-huntly", Number30Huntly),
    flicks("the-barn-banchory", TheBarnBanchory),
    flicks("victoria-hall-ellon", VictoriaHallEllon),
  )
  private val antrimScrapers: Seq[CinemaScraper] = Seq(
    flicks("imc-cinema-ballymena", IMCCinemaBallymena),
    flicks("movie-house-glengormley", MovieHouseGlengormley),
    flicks("omniplex-antrim", OmniplexAntrim),
    flicks("omniplex-carrickfergus", OmniplexCarrickfergus),
    flicks("omniplex-larne", OmniplexLarne),
  )
  private val armaghScrapers: Seq[CinemaScraper] = Seq(
    flicks("omniplex-craigavon", OmniplexCraigavon),
  )
  private val ayrshireAndArranScrapers: Seq[CinemaScraper] = Seq(
    flicks("astoria-cinema-ayr", AstoriaCinemaAyr),
    flicks("premier-saltcoats", CinemaSaltcoatsPremierLeisure),
    flicks("odeon-cinema-kilmarnock", OdeonCinemaKilmarnock),
  )
  private val bedfordshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-luton", CineworldLuton),
    flicks("vue-cinemas-bedford", VueCinemasBedford),
  )
  private val belfastScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-belfast", CineworldBelfast),
    flicks("movie-house-city-side-belfast", MovieHouseCitySideBelfast),
    flicks("odeon-cinema-belfast", OdeonCinemaBelfast),
    flicks("omniplex-belfast", OmniplexBelfast),
    flicks("omniplex-lisburn", OmniplexLisburn),
    flicks("queen-s-film-theatre-belfast", QueenSFilmTheatreBelfast),
    flicks("strand-arts-centre-belfast", StrandArtsCentreBelfast),
    flicks("the-avenue-cinema-belfast", TheAvenueCinemaBelfast),
  )
  private val berkshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-bracknell", CineworldBracknell),
    flicks("screen-one-newbury", CornExchangeNewburyScreenOne),
    flicks("everyman-cinema-wokingham", EverymanCinemaWokingham),
    flicks("odeon-cinema-luxe-maidenhead", OdeonLuxeMaidenhead),
    flicks("reading-biscuit-factory", ReadingBiscuitFactory),
    flicks("showcase-cinema-de-lux-reading", ShowcaseDeLuxReading),
    flicks("south-hill-park-arts-centre-bracknell", SouthHillParkArtsCentreBracknell),
    flicks("the-assembly-at-heckfield-place", TheAssemblyAtHeckfieldPlace),
    flicks("the-screen-cinema-windsor", TheOldCourtWindsor),
    flicks("vue-cinemas-newbury", VueCinemasNewbury),
    flicks("vue-cinemas-reading", VueCinemasReading),
  )
  private val birminghamScrapers: Seq[CinemaScraper] = Seq(
    flicks("artrix-bromsgrove", ArtrixBromsgrove),
    flicks("cineworld-broad-street-birmingham", CineworldBroadStreetBirmingham),
    flicks("cineworld-nec-birmingham", CineworldNECBirmingham),
    flicks("cineworld-solihull", CineworldSolihull),
    flicks("everyman-cinema-birmingham", EverymanCinemaBirmingham),
    flicks("midlands-arts-centre-birmingham", MidlandsArtsCentreBirmingham),
    flicks("mockingbird-cinema-kitchen-birmingham", MockingbirdCinemaKitchenBirmingham),
    flicks("odeon-cinema-birmingham-new-street", OdeonBirminghamNewStreet),
    flicks("odeon-luxe-birmingham", OdeonLuxeBirminghamBroadwayPlaza),
    flicks("empire-cinema-birmingham", OmniplexBirmingham),
    flicks("reel-cinema-quinton", ReelCinemaQuinton),
    flicks("royal-cinema-sutton-coldfield", RoyalCinemasSuttonColdfield),
    flicks("vue-cinemas-birmingham", VueCinemasBirmingham),
  )
  private val bristolScrapers: Seq[CinemaScraper] = Seq(
    flicks("cube-bristol", CubeCinemaBristol),
    flicks("everyman-cinema-bristol", EverymanCinemaBristol),
    flicks("odeon-cabot-circus", OdeonCabotCircus),
    flicks("orpheus-bristol", ScottCinemasBristolWestburyPark),
    flicks("showcase-cinema-bristol", ShowcaseBristolAvonmeads),
    flicks("vue-cinemas-cribbs-causeway-bristol", VueCinemasBristolCribbsCauseway),
    flicks("vue-cinemas-longwell-green-bristol", VueCinemasBristolLongwellGreen),
    flicks("watershed-bristol", WatershedBristol),
  )
  private val buckinghamshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-high", CineworldHighWycombe),
    flicks("cineworld-milton-keynes", CineworldMiltonKeynes),
    flicks("everyman-cinema-gerrards-cross", EverymanCinemaGerrardsCross),
    flicks("everyman-cinema-marlow", EverymanCinemaMarlow),
    flicks("odeon-cinema-aylesbury", OdeonCinemaAylesbury),
    flicks("odeon-cinema-milton-keynes", OdeonCinemaMiltonKeynes),
    flicks("empire-cinema-high-wycombe", OmniplexHighWycombeFormerlyEmpire),
    flicks("village-picture-house-cuddington", VillagePictureHouseCuddington),
  )
  private val cambridgeshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("arts-cinema-john-clare-theatre-peterborough", ArtsCinemaJohnClareTheatrePeterborough),
    flicks("arts-picturehouse-cambridge", ArtsPicturehouseCambridge),
    flicks("cineworld-ely", CineworldEly),
    flicks("cineworld-huntingdon", CineworldHuntingdon),
    flicks("cineworld-st-neots", CineworldStNeots),
    flicks("ely-community-cinema", ElyCommunityCinema),
    flicks("everyman-cinema-cambridge", EverymanCinemaCambridge),
    flicks("key-theatre-peterborough", KeyTheatrePeterborough),
    flicks("luxe-wisbech", LuxeWisbech),
    flicks("odeon-luxe-peterborough", OdeonLuxePeterborough),
    flicks("cinema-de-lux-peterborough", ShowcaseDeLuxPeterborough),
    flicks("the-light-cinemas-cambridge", TheLightCambridge),
    flicks("the-light-cinemas-wisbech", TheLightWisbech),
  )
  private val cardiffScrapers: Seq[CinemaScraper] = Seq(
    flicks("chapter-cardiff", ChapterCardiff),
    flicks("cineworld-cardiff", CineworldCardiff),
    flicks("everyman-cinema-cardiff", EverymanCinemaCardiff),
    flicks("odeon-cinema-cardiff", OdeonCinemaCardiff),
    flicks("showcase-cardiff", ShowcaseCinemaCardiff),
  )
  private val centralScotlandScrapers: Seq[CinemaScraper] = Seq(
    flicks("chalmers-alloa-cinema", ChalmersAlloaCinema),
    flicks("cineworld-falkirk", CineworldFalkirk),
    flicks("hippodrome-bo-ness", HippodromeBoNess),
    flicks("macrobert-art-centre-stirling", MacrobertArtCentreStirling),
    flicks("vue-cinemas-stirling", VueCinemasStirling),
  )
  private val cheshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("buxton-cinema", BuxtonCinemaPavilionArtsCentre),
    flicks("cinemac-macclesfield", CinemacMacclesfield),
    flicks("cineworld-warrington", CineworldWarrington),
    flicks("curzon-cinema-knutsford", CurzonCinemaKnutsford),
    flicks("everyman-cinema-altrincham", EverymanCinemaAltrincham),
    flicks("odeon-cinema-crewe", OdeonCinemaCrewe),
    flicks("odeon-cinema-northwich", OdeonCinemaNorthwichBaronsQuay),
    flicks("odeon-luxe-warrington", OdeonLuxeWarrington),
    flicks("picturehouse-chester", PicturehouseChester),
    flicks("reel-cinema-widnes", ReelCinemaWidnes),
    flicks("rex-wilmslow", RexWilmslow),
    flicks("storyhouse-chester", StoryhouseChester),
    flicks("vue-cinemas-altrincham", VueCinemasAltrincham),
    flicks("vue-cinemas-ellesmere-port", VueCinemasCheshireOaks),
  )
  private val clwydScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-llandudno", CineworldLlandudno),
    flicks("scala-prestatyn", MerlinScalaPrestatyn),
    flicks("vue-cinemas-rhyl", StrandCinemaRhyl),
    flicks("theatr-colwyn-colwyn-bay", TheatrColwyn),
  )
  private val cornwallScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-plymouth", CineworldPlymouth),
    flicks("filmhouse-newlyn", FilmhouseNewlyn),
    flicks("flora-helston", FloraCinemaHelston),
    flicks("capitol-bodmin", MerlinCapitolBodmin),
    flicks("regal-redruth", MerlinRegalCinemaRedruth),
    flicks("savoy-penzance", MerlinSavoyPenzance),
    flicks("phoenix-falmouth", PhoenixCinemaFalmouth),
    flicks("plymouth-arts-cinema-plymouth", PlymouthArtsCinema),
    flicks("rebel-bude", RebelCinema),
    flicks("royal-st-ives", RoyalStIvesCinema),
    flicks("the-astra-cinema-st-mawgan", TheAstraCinemaStMawgan),
    flicks("the-poly-falmouth", ThePolyFalmouth),
    flicks("vue-cinemas-plymouth", VueCinemasPlymouth),
    flicks("lighthouse-newquay", WTWLighthouseNewquay),
    flicks("plaza-truro", WTWPlazaTruro),
    flicks("regal-wadebridge", WTWRegalWadebridge),
    flicks("white-river-st-austell", WTWWhiteRiverCinema),
  )
  private val countyDurhamScrapers: Seq[CinemaScraper] = Seq(
    flicks("arc-stockton-on-tees", ARCStocktonOnTees),
    flicks("cineworld-murton-county", CineworldDaltonParkMurtonCounty),
    flicks("empire-cinema-consett-county", EmpireTheatreConsett),
    flicks("everyman-cinema-durham", EverymanCinemaDurham),
    flicks("fuse-community-cinema-prudhoe", FuseCommunityCinemaPrudhoe),
    flicks("gala-theatre-and-cinema-durham-county", GalaCinemaDurham),
    flicks("odeon-luxe-durham-county", OdeonLuxeDurham),
    flicks("showcase-cinema-de-lux-stockton-on-tees", ShowcaseCinemaDeLuxTeesside),
    flicks("vue-cinemas-darlington", VueCinemasDarlington),
    flicks("vue-cinemas-hartlepool-county", VueCinemasHartlepool),
  )
  private val cumbriaScrapers: Seq[CinemaScraper] = Seq(
    flicks("brewery-arts-centre-kendal", BreweryArtsCentreKendal),
    flicks("gaiety-cinema-whitehaven", GaietyCinemaWhitehaven),
    flicks("alhambra-keswick", KeswickAlhambra),
    flicks("lonsdale-alhambra-penrith", LonsdaleAlhambraPenrith),
    flicks("plaza-workington", ParkwayWorkington),
    flicks("reel-cinema-morecambe", ReelCinemaMorecambe),
    flicks("roxy-ulverston", RoxyUlverston),
    flicks("royalty-bowness-on-windemere", RoyaltyBownessOnWindemere),
    flicks("the-ritz-cinema-workington", TheRitzCinemaWorkington),
    flicks("vue-cinemas-barrow", VueCinemasBarrow),
    flicks("vue-cinemas-carlisle", VueCinemasCarlisle),
    flicks("zeffirellis-by-the-park-ambleside", ZeffirellisCinemaAmbleside),
  )
  private val derbyshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-chesterfield", CineworldChesterfield),
    flicks("elite-cinema-and-theatre-ashbourne", EliteCinemaAndTheatreAshbourne),
    flicks("northern-light-wirksworth", NorthernLightWirksworth),
    flicks("odeon-cinema-swadlincote", OdeonCinemaSwadlincote),
    flicks("odeon-luxe-derby", OdeonLuxeDerby),
    flicks("quad-derby", QuadDerby),
    flicks("ritz-belper", RitzBelper),
    flicks("showcase-cinema-de-lux-derby", ShowcaseCinemaDeLuxDerby),
  )
  private val devonScrapers: Seq[CinemaScraper] = Seq(
    flicks("alexandra-newton-abbot", AlexandraNewtonAbbot),
    flicks("dartington-art-centre-totnes", BarnCinemaDartingtonArtCentre),
    flicks("central-barnstaple", CentralCinemaBarnstaple),
    flicks("embassy-ilfracombe", EmbassyCinemaIlfracombe),
    flicks("everyman-cinema-plymouth", EverymanCinemaPlymouth),
    flicks("kings-kingsbridge", KingsCinemaKingsbridge),
    flicks("lynton-cinema-lynton-lynmouth", LyntonCinema),
    flicks("new-carlton-okehampton", NewCarltonOkehampton),
    flicks("new-central-cinema-torquay", NewCentralCinemaTorquay),
    flicks("odeon-cinema-exeter", OdeonCinemaExeter),
    flicks("pavilions-teignmouth", PavilionsTeignmouth),
    flicks("picturehouse-exeter", PicturehouseExeter),
    flicks("plough-arts-centre-torrington", PloughArtsCentreTorrington),
    flicks("radway-sidmouth", RadwaySidmouth),
    flicks("savoy-exmouth", SavoyScottCinemasExmouth),
    flicks("the-beehive-honiton", TheBeehiveHoniton),
    flicks("flavel-dartmouth", TheFlavel),
    flicks("the-watermark-ivybridge", TheWatermarkIvybridge),
    flicks("tivoli-tiverton", TivoliTiverton),
    flicks("totnes-cinema", TotnesCinema),
    flicks("vue-cinemas-exeter", VueCinemasExeter),
    flicks("vue-cinemas-paignton", VueCinemasTorbayPaignton),
  )
  private val dorsetScrapers: Seq[CinemaScraper] = Seq(
    flicks("colosseum-bournemouth", ColosseumBournemouth),
    flicks("electric-palace-bridport", ElectricPalaceBridport),
    flicks("hilltop-cinema-shaftesbury-arts-centre", HilltopCinemaShaftesburyArtsCentre),
    flicks("lighthouse-poole", LighthousePoole),
    flicks("mowlem-swanage", MowlemTheatre),
    flicks("odeon-cinema-bournemouth", OdeonCinemaBournemouthBH2),
    flicks("odeon-cinema-dorchester", OdeonCinemaDorchester),
    flicks("plaza-cinema-dorchester", PlazaCinemaDorchester),
    flicks("regent-christchurch", RegentChristchurch),
    flicks("the-new-vic-tisbury-village-hall", TheNewVicTisburyVillageHall),
    flicks("rex-wareham", TheRexCinemaWareham),
    flicks("tivoli-wimborne-minster", TivoliTheatreWimborne),
    flicks("vue-poole", VueCinemasPoole),
  )
  private val downScrapers: Seq[CinemaScraper] = Seq(
    flicks("movieland-newtownards", IMCNewtownardsMovieland),
    flicks("iveagh-movie-studio-banbridge", IveaghMovieStudioIMCBanbridge),
    flicks("omniplex-banbridge", OmniplexBanbridge),
    flicks("omniplex-d-luxx-bangor", OmniplexBangor),
    flicks("omniplex-downpatrick", OmniplexDownpatrick),
    flicks("omniplex-dundonald", OmniplexDundonald),
    flicks("omniplex-newry", OmniplexNewry),
  )
  private val dudleyScrapers: Seq[CinemaScraper] = Seq(
    flicks("odeon-cinema-dudley", OdeonCinemaDudley),
    flicks("showcase-cinema-dudley", ShowcaseCinemaDudley),
  )
  private val dumfriesAndGallowayScrapers: Seq[CinemaScraper] = Seq(
    flicks("lonsdale-city-annan", LonsdaleCityCinemaAnnan),
    flicks("robert-burns-centre-film-theatre-dumfries", RobertBurnsCentreFilmTheatre),
    flicks("the-cinema-newton-stewart", TheCinemaNewtonStewart),
    flicks("fullarton-castle-douglas", TheFullartonCastleDouglas),
  )
  private val dunbartonshireArgyllButeScrapers: Seq[CinemaScraper] = Seq(
    flicks("campbeltown-picture-house", CampbeltownPictureHouse),
    flicks("discovery-theatre-rothesay", DiscoveryCentreCinemaRothesay),
    flicks("empire-cinema-clydebank", OmniplexClydebankFormerlyEmpire),
    flicks("studio-dunoon", StudioCinemaDunoon),
  )
  private val dyfedScrapers: Seq[CinemaScraper] = Seq(
    flicks("aberystwyth-arts-centre", AberystwythArtsCentre),
    flicks("commodore-aberystwyth", CommodoreCinemaAberystwyth),
    flicks("public-hall-cinema-cross-hands", CrossHandsHallCinema),
    flicks("libanus-1877-borth", Libanus1877),
    flicks("miners-welfare-and-community-hall-ystradgynlais", MinersWelfareAndCommunityHallYstradgynlais),
    flicks("odeon-cinema-llanelli", OdeonCinemaLlanelli),
    flicks("palace-haverfordwest", PalaceCinemaHaverfordwest),
    flicks("public-hall-brynamman", PublicHallBrynamman),
    flicks("theatr-gwaun-fishguard", TheatrGwaunFishguard),
    flicks("theatr-mwldan-cardigan", TheatrMwldanCardigan),
    flicks("torch-theatre-milford-haven", TorchTheatreMilfordHaven),
    flicks("vue-cinemas-carmarthen", VueCinemasCarmarthen),
  )
  private val eastSussexScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-brighton", CineworldBrighton),
    flicks("cineworld-eastbourne", CineworldEastbourne),
    flicks("depot-lewes", DepotLewes),
    flicks("duke-of-york-s-picturehouse", DukeOfYorkSPicturehouseBrighton),
    flicks("duke-s-at-komedia-picturehouse", DukeSAtKomediaPicturehouse),
    flicks("electric-palace-hastings", ElectricPalaceHastings),
    flicks("kino-rye", KinoRye),
    flicks("kino-teatr-st-leonards-on-sea", KinoTeatr),
    flicks("odeon-cinema-brighton", OdeonCinemaBrighton),
    flicks("odeon-cinema-hastings", OdeonCinemaHastings),
    flicks("pavilion-hailsham", PavilionHailsham),
    flicks("picturehouse-uckfield", PictureHouseUckfield),
    flicks("towner-eastbourne-cinema", TownerEastbourneCinema),
  )
  private val eastYorkshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-hull", CineworldHull),
    flicks("forum-bridlington", ForumBridlington),
    flicks("odeon-luxe-hull", OdeonLuxeHull),
    flicks("palace-cinema-malton", PalaceCinemaMalton),
    flicks("parkway-beverley", ParkwayBeverley),
    flicks("reel-cinema-hull", ReelCinemaHull),
    flicks("vue-cinemas-hull", VueCinemasHull),
  )
  private val edinburghAndLothiansScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-edinburgh", CineworldEdinburgh),
    flicks("dominion-edinburgh", DominionCinemaEdinburgh),
    flicks("everyman-cinema-edinburgh", EverymanCinemaEdinburgh),
    flicks("filmhouse-edinburgh", FilmhouseEdinburgh),
    flicks("odeon-cinema-edinburgh-fort-kinnaird", OdeonEdinburghFortKinnaird),
    flicks("odeon-cinema-edinburgh-lothian-road", OdeonEdinburghLothianRoad),
    flicks("odeon-luxe-edinburgh-edinburgh-west", OdeonLuxeEdinburghEdinburghWest),
    flicks("scotsman-picturehouse-edinburgh", ScotsmanPicturehouseEdinburgh),
    flicks("the-cameo-picturehouse", TheCameoPicturehouse),
    flicks("the-fraser-centre-tranent", TheFraserCentreTranent),
    flicks("vue-cinemas-livingston", VueCinemasLivingston),
    flicks("vue-ocean-terminal-edinburgh", VueEdinburghOceanTerminal),
    flicks("vue-omni-centre-edinburgh", VueEdinburghOmniCentre),
  )
  private val essexScrapers: Seq[CinemaScraper] = Seq(
    flicks("century-clacton", CenturyCinemaClacton),
    flicks("cineworld-basildon", CineworldBasildon),
    flicks("cineworld-braintree", CineworldBraintree),
    flicks("cineworld-harlow-harvey-centre", CineworldHarlowHarveyCentre),
    flicks("cineworld-harlow-queensgate", CineworldHarlowQueensgate),
    flicks("curzon-cinema-colchester", CurzonCinemaColchester),
    flicks("electric-palace-harwich", ElectricPalaceHarwich),
    flicks("empire-theatre-halstead-park", EmpireTheatreHalstead),
    flicks("everyman-cinema-chelmsford", EverymanCinemaChelmsford),
    flicks("movie-starr-canvey-island", MovieStarrCanveyIsland),
    flicks("odeon-cinema-chelmsford", OdeonCinemaChelmsford),
    flicks("odeon-cinema-colchester", OdeonCinemaColchester),
    flicks("odeon-cinema-southend-on-sea", OdeonCinemaSouthendOnSea),
    flicks("rio-burnham-on-crouch", RioBurnhamOnCrouch),
    flicks("empire-cinema-bishops", RoxyMoviesBishopSStortford),
    flicks("saffron-screen", SaffronScreen),
    flicks("vue-cinemas-basildon", VueCinemasBasildon),
    flicks("vue-cinemas-colchester", VueCinemasColchester),
    flicks("vue-cinemas-west-thurrock", VueCinemasWestThurrock),
  )
  private val fermanaghScrapers: Seq[CinemaScraper] = Seq(
    flicks("imc-cinema-enniskillen", IMCCinemaEnniskillen),
  )
  private val fifeScrapers: Seq[CinemaScraper] = Seq(
    flicks("adam-smith-theatre-kirkcaldy", AdamSmithTheatreKirkcaldy),
    flicks("kino-glenrothes", KinoGlenrothes),
    flicks("odeon-cinema-dunfermline", OdeonCinemaDunfermline),
  )
  private val glamorganScrapers: Seq[CinemaScraper] = Seq(
    flicks("coliseum-theatre-aberdare", ColiseumTheatreAberdare),
    flicks("gwyn-hall-neath", GwynHallNeath),
    flicks("odeon-cinema-bridgend", OdeonCinemaBridgend),
    flicks("odeon-cinema-swansea", OdeonCinemaSwansea),
    flicks("pontardawe-arts-centre", PontardaweArtsCentre),
    flicks("reel-port-talbot", ReelCinemaPortTalbot),
    flicks("taliesin-arts-centre-swansea", TaliesinArtsCentreSwansea),
    flicks("vue-cinemas-merthyr-tydfil", VueCinemasMerthyrTydfil),
    flicks("vue-cinemas-swansea", VueCinemasSwansea),
  )
  private val glasgowScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-silverburn-glasgow", CineworldSilverburnGlasgow),
    flicks("everyman-cinema-glasgow", EverymanCinemaGlasgow),
    flicks("glasgow-film-theatre-glasgow", GlasgowFilmTheatre),
    flicks("grosvenor-cinema-glasgow", GrosvenorCinemaGlasgow),
    flicks("imax-glasgow", IMAXAtGlasgowScienceCentre),
    flicks("cumbernauld-theatre-at-lanternhouse", LanternhouseCinema),
    flicks("odeon-luxe-glasgow", OdeonLuxeGlasgow),
    flicks("vue-cinemas-glasgow", VueCinemasGlasgowFort),
    flicks("vue-cinemas-glasgow-st-enoch", VueCinemasGlasgowStEnoch),
  )
  private val gloucestershireScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-cheltenham", CineworldCheltenham),
    flicks("cineworld-gloucester", CineworldGloucesterQuays),
    flicks("electric-picture-house-wotton-under-edge", ElectricPictureHouseWottonUnderEdge),
    flicks("tivoli-cheltenham", EverymanCheltenham),
    flicks("guildhall-cinema-gloucester", GuildhallCinemaGloucester),
    flicks("studio-coleford", MerlinStudioColeford),
    flicks("palace-cinema-cinderford", PalaceCinemaCinderford),
    flicks("roses-theatre-tewksbury", RosesTheatreTewkesbury),
    flicks("sherborne-gloucester", SherborneCinemaGloucester),
    flicks("vue-cinemas-stroud", VueCinemasStroud),
  )
  private val guernseyScrapers: Seq[CinemaScraper] = Seq(
    flicks("beau-sejour-cinema-st-peter-port-guernsey", BeauSejourLeisureCentreGuernsey),
    flicks("the-mallard-cinema-guernsey", TheMallardCinemaGuernsey),
  )
  private val gwentScrapers: Seq[CinemaScraper] = Seq(
    flicks("baker-street-cinema-abergavenny", BakerStreetCinemaAbergavenny),
    flicks("cineworld-spytty-park-newport", CineworldSpyttyParkNewport),
    flicks("market-hall-cinema-brynmawr", MarketHallCinemaBrynmawr),
    flicks("maxime-blackwood", MaximeCinemaBlackwood),
    flicks("riverfront-newport", RiverfrontNewport),
    flicks("savoy-monmouth", SavoyTheatreMonmouth),
    flicks("vue-cinemas-cwmbran", VueCinemasCwmbran),
  )
  private val gwyneddScrapers: Seq[CinemaScraper] = Seq(
    flicks("cellb-blaenau-ffestiniog", CellBBlaenauFfestiniog),
    flicks("empire-cinema-holyhead", EmpireCinemaHolyhead),
    flicks("galeri-caenarfon", GaleriCaenarfon),
    flicks("magic-lantern-llansern-hud-tywyn", MagicLanternTywyn),
    flicks("neuadd-dwyfor-pwllheli", NeuaddDwyforPwllheli),
    flicks("pontio-arts-innovation-centre-bangor", PontioArtsInnovationCentreBangor),
    flicks("theatr-derek-williams", TheatrDerekWilliams),
  )
  private val hampshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("chichester-cinema-at-new-park", ChichesterCinemaAtNewPark),
    flicks("cineworld-chichester", CineworldChichester),
    flicks("cineworld-fareham", CineworldWhiteley),
    flicks("everyman-cinema-winchester", EverymanCinemaWinchester),
    flicks("harbour-lights-picturehouse-southampton", HarbourLightsPicturehouse),
    flicks("hythe-moviola-cinema", HytheMoviolaCinema),
    flicks("no-6-cinema-portsmouth", No6CinemaPortsmouth),
    flicks("odeon-cinema-basingstoke", OdeonCinemaBasingstoke),
    flicks("odeon-cinema-portsmouth", OdeonCinemaPortSolent),
    flicks("reel-cinema-fareham", ReelCinemaFareham),
    flicks("showcase-cinema-de-lux-southampton", ShowcaseDeLuxSouthampton),
    flicks("southsea-cinema-and-arts-centre", SouthseaCinemaArtsCentre),
    flicks("the-living-room-cinema", TheLivingRoomCinemaLiphook),
    flicks("the-malt-lymington", TheMaltLymington),
    flicks("vue-cinemas-basingstoke", VueCinemasBasingstoke),
    flicks("vue-cinemas-eastleigh", VueCinemasEastleigh),
    flicks("vue-cinemas-portsmouth", VueCinemasPortsmouth),
  )
  private val herefordshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("courtyard-hereford", CourtyardHereford),
    flicks("gateway-ross-on-wye", GatewayCinemaRossOnWye),
    flicks("odeon-cinema-hereford", OdeonCinemaHereford),
    flicks("richard-booth-s-cinema-hay-on-wye", RichardBoothSBookshopHayOnWye),
  )
  private val hertfordshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("baldock-arts-and-heritage-centre", BaldockArtsHeritageCentre),
    flicks("hertford-theatre", BEAMHertfordTheatre),
    flicks("broadway-letchworth", BroadwayLetchworth),
    flicks("cineworld-hemel-hempstead", CineworldHemelHempstead),
    flicks("cineworld-stevenage", CineworldStevenage),
    flicks("cineworld-watford", CineworldWatford),
    flicks("odeon-cinema-hatfield", OdeonCinemaHatfield),
    flicks("reel-cinema-borehamwood", ReelCinemaBorehamwood),
    flicks("garden-city-cinema-welwyn", TheCinemaCampusWest),
    flicks("odyssey-st-albans", TheOdysseyStAlbans),
    flicks("rex-berkhamsted", TheRexCinemaBerkhamsted),
    flicks("vue-cinemas-watford", VueCinemasWatford),
    flicks("watersmeet", Watersmeet),
  )
  private val highlandsAndIslandsScrapers: Seq[CinemaScraper] = Seq(
    flicks("an-lanntair-arts-centre-stornoway-isle-of-lewis", AnLanntairArtsCentreStornowayIsleOfLewis),
    flicks("cromarty-cinema-cromarty", CromartyCinema),
    flicks("eden-court-theatre-inverness", EdenCourtTheatreInverness),
    flicks("highland-cinema-fort-william", HighlandCinemaFortWilliam),
    flicks("aros-portree", LASPortRighArosCinemaPortree),
    flicks("mareel-lerwick-shetland-islands", MareelLerwickShetlandIslands),
    flicks("merlin-thurso", MerlinCinemaThurso),
    flicks("phoenix-kirkwall-okney-islands", PhoenixKirkwallOkneyIslands),
    flicks("spey-valley-aviemore", SpeyValleyCinemaAviemore),
    flicks("vue-cinemas-inverness", VueCinemasInverness),
    flicks("west-side-cinema-stromness", WestSideCinemaStromness),
  )
  private val isleOfManScrapers: Seq[CinemaScraper] = Seq(
    flicks("broadway-cinema-villa-marina", BroadwayCinemaVillaMarina),
    flicks("palace-cinemas-isle-of-man", PalaceCinemasIsleOfMan),
  )
  private val isleOfWightScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-newport-isle-of-wight", CineworldNewportIsleOfWight),
    flicks("commodore-ryde-isle-of-wight", CommodoreRydeIsleOfWight),
  )
  private val jerseyScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-st-helier-jersey", CineworldStHelierJersey),
  )
  private val kentScrapers: Seq[CinemaScraper] = Seq(
    flicks("carlton-westgate-on-sea", CarltonCinemaWestgateOnSea),
    flicks("cinemarsh-the-marsh-academy", CinemarshTheMarshAcademy),
    flicks("cineworld-ashford", CineworldAshford),
    flicks("cineworld-dover", CineworldDover),
    flicks("cineworld-rochester", CineworldRochester),
    flicks("curzon-canterbury-riverside", CurzonCanterburyRiverside),
    flicks("empire-cinema-sandwich", EmpireCinemaSandwich),
    flicks("gulbenkian-theatre-canterbury", GulbenkianTheatre),
    flicks("kavanagh-herne-bay", KavanaghCinemaHerneBay),
    flicks("kino-hawkhurst", KinoHawkhurst),
    flicks("odeon-cinema-chatham", OdeonCinemaChatham),
    flicks("odeon-cinema-maidstone", OdeonCinemaMaidstone),
    flicks("odeon-cinema-tunbridge-wells", OdeonCinemaTunbridgeWells),
    flicks("palace-cinema-broadstairs", PalaceCinemaKent),
    flicks("royal-faversham", RoyalCinemaFaversham),
    flicks("showcase-cinema-de-lux-dartford", ShowcaseDeLuxBluewater),
    flicks("silver-screen-folkestone", SilverScreenFolkestone),
    flicks("stag-sevenoaks", StagSevenoaks),
    flicks("picturehouse-ashford", TheAshfordCinemaFormerlyPicturehouse),
    flicks("the-light-cinemas-sittingbourne", TheLightSittingbourne),
    flicks("the-woodville-gravesend", TheWoodvilleGravesend),
    flicks("vue-cinemas-westwood-cross-thanet", VueCinemasThanetWestwoodCross),
  )
  private val lanarkshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("odeon-luxe-east-kilbride", OdeonLuxeEastKilbride),
    flicks("showcase-de-lux-coatbridge", ShowcaseGlasgowCoatbridge),
    flicks("vue-cinemas-hamilton", VueCinemasHamilton),
  )
  private val lancashireScrapers: Seq[CinemaScraper] = Seq(
    flicks("the-backlot-cinema-and-diner", ArcCinemaBlackpool),
    flicks("arc-cinema-preston", ArcCinemaPreston),
    flicks("cineworld-bolton", CineworldBolton),
    flicks("cineworld-broughton", CineworldBroughton),
    flicks("everyman-cinema-clitheroe", EverymanCinemaClitheroe),
    flicks("flower-bowl-entertainment-centre-preston", FlowerBowlEntertainmentCentrePreston),
    flicks("lowther-pavilion-lytham", LowtherPavilionLytham),
    flicks("odeon-cinema-preston", OdeonCinemaPreston),
    flicks("odeon-cinema-rochdale", OdeonCinemaRochdale),
    flicks("reel-cinema-blackburn", ReelCinemaBlackburn),
    flicks("reel-cinema-chorley", ReelCinemaChorley),
    flicks("reel-cinema-burnley", ReelCinemasBurnley),
    flicks("regent-blackpool", RegentBlackpool),
    flicks("the-dukes-lancaster", TheDukesLancaster),
    flicks("island-cinemas-lytham-st-annes", TheIslandLythamStAnnes),
    flicks("the-light-cinemas-bolton", TheLightBolton),
    flicks("vue-cinemas-accrington", VueCinemasAccrington),
    flicks("vue-cinemas-blackburn", VueCinemasBlackburn),
    flicks("vue-cinemas-bolton", VueCinemasBolton),
    flicks("vue-cinemas-bury", VueCinemasBury),
    flicks("vue-cinemas-cleveleys", VueCinemasCleveleys),
    flicks("vue-cinemas-lancaster", VueCinemasLancaster),
    flicks("vue-cinemas-preston", VueCinemasPreston),
  )
  private val leicestershireScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-hinckley", CineworldHinckley),
    flicks("flix-student-run-cinema-loughborough", FlixStudentRunCinemaLoughborough),
    flicks("odeon-cinema-loughborough", OdeonCinemaLoughborough),
    flicks("odeon-luxe-leicester", OdeonLuxeLeicester),
    flicks("phoenix-square-leicester", PhoenixCinemaAndArtCentreLeicester),
    flicks("piccadilly-leicester", PiccadillyCinemaLeicester),
    flicks("regal-melton-mowbray", RegalMeltonMowbray),
    flicks("showcase-cinema-de-lux-leicester", ShowcaseDeLuxLeicester),
    flicks("vue-cinemas-leicester", VueCinemasLeicester),
  )
  private val lincolnshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("arts-centre-stamford", ArtsCentreStamford),
    flicks("everyman-cinema-lincoln", EverymanCinemaLincoln),
    flicks("junction-goole", JunctionGoole),
    flicks("kinema-in-the-woods-woodhall-spa", KinemaInTheWoods),
    flicks("loewen-mablethorpe", LoewenCinema),
    flicks("odeon-cinema-lincoln", OdeonCinemaLincoln),
    flicks("playhouse-louth", ParkwayCinemaLouth),
    flicks("parkway-cleethorpes", ParkwayCleethorpes),
    flicks("savoy-boston", SavoyBoston),
    flicks("savoy-grantham", SavoyGrantham),
    flicks("sleaford-playhouse", SleafordPlayhouse),
    flicks("tower-skegness", TowerCinemaSkegness),
    flicks("vue-cinemas-scunthorpe", VueCinemasScunthorpe),
  )
  private val londonderryScrapers: Seq[CinemaScraper] = Seq(
    flicks("brunswick-moviebowl-londonderry", BrunswickMoviebowlLondonderry),
    flicks("movie-house-coleraine", MovieHouseColeraine),
    flicks("movie-house-maghera", MovieHouseMaghera),
    flicks("nerve-centre-londonderry", NerveCentreLondonderry),
    flicks("omniplex-londonderry", OmniplexLondonderry),
  )
  private val liverpoolScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-speke", CineworldSpeke),
    flicks("cineworld-st-helens", CineworldStHelens),
    flicks("everyman-cinema-liverpool", EverymanCinemaLiverpool),
    flicks("odeon-cinema-liverpool-one", OdeonLiverpoolONE),
    flicks("odeon-luxe-liverpool", OdeonLiverpoolSwitchIsland),
    flicks("odeon-luxe-bromborough", OdeonLuxeBromborough),
    flicks("picturehouse-at-fact-liverpool", PicturehouseAtFACTLiverpool),
    flicks("plaza-crosby", PlazaCommunityCinemaLiverpool),
    flicks("showcase-de-lux-liverpool", ShowcaseDeLuxLiverpool),
    flicks("bijou-southport", SouthportBijouCinema),
    flicks("the-light-cinemas-new-brighton", TheLightNewBrighton),
    flicks("vue-cinemas-birkenhead", VueCinemasBirkenhead),
    flicks("vue-cinemas-southport", VueCinemasSouthport),
    flicks("woolton-picture-house", WooltonPictureHouse),
  )
  private val northYorkshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-york", CineworldYork),
    flicks("city-screen-picturehouse-york", CityScreenPicturehouseYork),
    flicks("everyman-cinema-harrogate", EverymanCinemaHarrogate),
    flicks("everyman-cinema-northallerton", EverymanCinemaNorthallerton),
    flicks("everyman-cinema-york", EverymanCinemaYork),
    flicks("hollywood-plaza-scarborough", HollywoodPlazaScarborough),
    flicks("odeon-cinema-harrogate", OdeonCinemaHarrogate),
    flicks("odeon-middlesbrough", OdeonMiddlesbrough),
    flicks("pavilion-whitby", PavilionCinemaWhitby),
    flicks("pocklington-arts-centre", PocklingtonArtsCentre),
    flicks("regent-redcar", RegentRedcar),
    flicks("ritz-thirsk", RitzCinemaThirsk),
    flicks("roxy-movies-middlesbrough", RoxyMoviesMiddlesbrough),
    flicks("savoy-cinema-catterick-garrison", SavoyCinemaCatterickGarrison),
    flicks("station-richmond", StationCinemaRichmond),
    flicks("stephen-joseph-theatre-scarborough", StephenJosephTheatreScarborough),
    flicks("the-forum-northallerton", TheForumNorthallerton),
    flicks("vue-cinemas-york", VueCinemasYork),
  )
  private val northamptonshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("arc-cinema-daventry", ArcCinemaDaventry),
    flicks("cineworld-rushden", CineworldRushdenLakes),
    flicks("forum-northampton", ForumCinemaNorthampton),
    flicks("northampton-filmhouse-northampton", NorthamptonFilmhouse),
    flicks("odeon-northampton", OdeonNorthampton),
    flicks("savoy-corby", SavoyCinemaCorby),
    flicks("vue-cinemas-northampton", VueCinemasNorthampton),
  )
  private val northumberlandScrapers: Seq[CinemaScraper] = Seq(
    flicks("forum-hexham", ForumCinemaHexham),
    flicks("market-pavillion-cinema-blyth", MarketPavilionCinemaBlyth),
    flicks("phoenix-cinema-blyth", PhoenixCinemaBlyth),
    flicks("the-maltings-berwick-upon-tweed", TheMaltingsBerwickUponTweed),
    flicks("vue-cinemas-cramlington", VueCinemasCramlington),
  )
  private val nottinghamshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("arc-cinema-at-the-byron-hucknall", ArcCinemaAtTheByronHucknall),
    flicks("arc-cinema-beeston", ArcCinemaBeeston),
    flicks("broadway-nottingham", BroadwayCinemaNottingham),
    flicks("odeon-cinema-mansfield", OdeonCinemaMansfield),
    flicks("odeon-cinema-newark", OdeonCinemaNewark),
    flicks("scala-ilkeston", ReelCinemaScalaIlkeston),
    flicks("savoy-nottingham", SavoyCinemaNottingham),
    flicks("savoy-worksop", SavoyWorksop),
    flicks("showcase-cinema-nottingham", ShowcaseDeLuxNottingham),
    flicks("cineworld-nottingham", VueCinemasNottingham),
  )
  private val oxfordshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("abbey-cinema-abingdon", AbbeyCinemaAbingdon),
    flicks("cineworld-didcot", CineworldDidcot),
    flicks("cineworld-witney", CineworldWitney),
    flicks("corn-exchange-cinema-wallingford", CornExchangeCinemaWallingford),
    flicks("curzon-cinema-oxford", CurzonCinemaOxford),
    flicks("phoenix-picturehouse-oxford", PhoenixPicturehouseOxford),
    flicks("regal-picturehouse-henley", RegalPicturehouseHenley),
    flicks("the-light-cinemas-banbury", TheLightBanbury),
    flicks("the-living-room-cinema-chipping-norton", TheLivingRoomCinemaChippingNorton),
    flicks("cinema-oxford", TheOxfordCinemaCafe),
    flicks("ultimate-picture-palace-oxford", UltimatePicturePalaceOxford),
    flicks("vue-cinemas-bicester", VueCinemasBicester),
    flicks("vue-cinemas-oxford", VueCinemasOxford),
  )
  private val powysScrapers: Seq[CinemaScraper] = Seq(
    flicks("coliseum-brecon", ColiseumCinemaBrecon),
    flicks("odeon-cinema-wrexham", OdeonCinemaWrexham),
    flicks("wyeside-arts-centre-builth-wells", WyesideArtsCentreBuilthWells),
  )
  private val renfrewshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("odeon-cinema-braehead", OdeonCinemaBraehead),
    flicks("showcase-de-lux-paisley", ShowcaseDeLuxPaisley),
    flicks("the-tower-digital-arts-center-scottish-submarine-centre", TheTowerDigitalArtsCenterHelensburgh),
    flicks("waterfront-greenock", WaterfrontGreenock),
  )
  private val roxburghEttrickAndLauderdaleScrapers: Seq[CinemaScraper] = Seq(
    flicks("pavilion-galashiels", PavilionCinemaGalashiels),
    flicks("tower-mill-cinema-hawick", TowerMillHeartOfHawick),
  )
  private val sandwellScrapers: Seq[CinemaScraper] = Seq(
    flicks("odeon-cinema-west-bromwich", OdeonCinemaWestBromwich),
  )
  private val shropshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("assembly-rooms-ludlow", AssemblyRoomsLudlow),
    flicks("cineworld-shrewsbury", CineworldShrewsbury),
    flicks("cineworld-telford", CineworldTelford),
    flicks("festival-drayton-centre-market-drayton", FestivalDraytonCentre),
    flicks("maona-cinema-oswestry", MaonaCinemaOswestry),
    flicks("odeon-luxe-telford", OdeonLuxeTelford),
    flicks("old-market-hall-shrewsbury", OldMarketHallShrewsbury),
    flicks("majestic-bridgnorth", ReelCinemaBridgnorthMajestic),
    flicks("wellington-orbit-wellington", WellingtonOrbit),
  )
  private val somersetScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-weston-super-mare", CineworldWestonSuperMare),
    flicks("cineworld-yeovil", CineworldYeovil),
    flicks("curzon-cinema-clevedon", CurzonCinemaClevedon),
    flicks("tivoli-bath", EverymanBath),
    flicks("little-theatre-picturehouse", LittleTheatrePicturehouse),
    flicks("wellesley-wellington", MerlinWellesleyWellington),
    flicks("odeon-cinema-bath", OdeonCinemaBath),
    flicks("odeon-cinema-taunton", OdeonCinemaTaunton),
    flicks("plaza-cinema-weston-super-mare", PlazaCinemaWestonSuperMare),
    flicks("ritz-burnham-on-sea", RitzBurnhamOnSea),
    flicks("scott-bridgwater", ScottCinemasBridgwater),
    flicks("taunton-brewhouse", TauntonBrewhouse),
    flicks("the-avenue-cinema-minehead", TheAvenueCinemaMinehead),
    flicks("the-wells-film-centre", TheWellsFilmCentre),
    flicks("westway-frome", WestwayCinemaFrome),
  )
  private val southYorkshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("arc-cinema-rotherham", ArcCinemaRotherham),
    flicks("cineworld-barnsley", CineworldBarnsley),
    flicks("parkway-barnsley", ParkwayBarnsley),
    flicks("savoy-doncaster", SavoyDoncaster),
    flicks("vue-cinemas-doncaster", VueCinemasDoncaster),
  )
  private val staffordshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("electric-palace-picture-house-cannock", CannockCinema),
    flicks("cinebowl-uttoxeter", CinebowlUttoxeter),
    flicks("cineworld-burton-on-trent", CineworldBurtonOnTrent),
    flicks("cineworld-stoke-on-trent", CineworldStokeOnTrent),
    flicks("cineworld-wolverhampton", CineworldWolverhampton),
    flicks("film-theatre-stoke-on-trent", FilmTheatreStokeOnTrent),
    flicks("lichfield-garrick-theatre-and-studio", LichfieldGarrickTheatreStudio),
    flicks("lockworks-cinema", LockworksCinemaWolverhampton),
    flicks("odeon-cinema-stoke-on-trent", OdeonCinemaStokeOnTrent),
    flicks("odeon-cinema-tamworth", OdeonCinemaTamworth),
    flicks("odeon-luxe-stafford", OdeonLuxeStafford),
    flicks("red-carpet-barton-under-needwood", RedCarpetBartonMarina),
    flicks("the-light-cinemas-walsall", TheLightWalsall),
    flicks("vue-cinemas-newcastle-under-lyme", VueCinemasNewcastleUnderLyme),
  )
  private val suffolkScrapers: Seq[CinemaScraper] = Seq(
    flicks("abbeygate-bury-st-edmunds", AbbeygateBuryStEdmunds),
    flicks("aldeburgh-cinema", AldeburghCinema),
    flicks("cineworld-bury-st-edmunds", CineworldBuryStEdmunds),
    flicks("cineworld-haverhill", CineworldHaverhill),
    flicks("cineworld-ipswich", CineworldIpswich),
    flicks("electric-picture-palace-southwold", ElectricPicturePalaceSouthwold),
    flicks("everyman-bury-st-edmunds", EverymanBuryStEdmunds),
    flicks("film-theatre-leiston", FilmTheatreLeiston),
    flicks("haverhill-arts-centre-haverhill", HaverhillArtsCentre),
    flicks("ipswich-film-theatre-ipswich", KingStreetCinema),
    flicks("kings-cinema-newmarket", KingsCinemaNewmarket),
    flicks("empire-cinema-ipswich", OmniplexIpswichFormerlyEmpire),
    flicks("palace-cinema-felixstowe", PalaceCinemaFelixstowe),
    flicks("regal-stowmarket", RegalStowmarket),
    flicks("riverside-theatre-woodbridge", RiversideTheatreWoodbridge),
  )
  private val surreyScrapers: Seq[CinemaScraper] = Seq(
    flicks("chiddingfold-village-hall-cinema", ChiddingfoldVillageHallCinema),
    flicks("cineworld-aldershot", CineworldAldershot),
    flicks("everyman-cinema-oxted", EverymanCinemaOxted),
    flicks("everyman-cinema-reigate", EverymanCinemaReigate),
    flicks("haslemere-hall-cinema", HaslemereHallCinema),
    flicks("odeon-cinema-guildford", OdeonCinemaGuildford),
    flicks("reel-cinemas-farnham", ReelCinemasFarnham),
    flicks("the-light-redhill", TheLightRedhill),
    flicks("vue-cinemas-camberley", VueCinemasCamberley),
    flicks("vue-cinemas-farnborough", VueCinemasFarnborough),
  )
  private val taysideScrapers: Seq[CinemaScraper] = Seq(
    flicks("birks-aberfeldy", BirksAberfeldy),
    flicks("chalmers-arbroath-cinema", ChalmersFilmhouseArbroath),
    flicks("cineworld-dundee", CineworldDundee),
    flicks("dundee-contemporary-arts-dca-dundee", DundeeContemporaryArtsDCA),
    flicks("new-picture-house-st-andrews", NewPictureHouseStAndrews),
    flicks("odeon-luxe-dundee", OdeonLuxeDundee),
    flicks("playhouse-perth", PlayhouseCinemaPerth),
    flicks("the-montrose-playhouse", TheMontrosePlayhouse),
  )
  private val tyneAndWearScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-boldon-tyne-wear", CineworldBoldonTyneWear),
    flicks("cineworld-newcastle", CineworldNewcastle),
    flicks("customs-house-south-shields", CustomsHouseCinemaSouthShields),
    flicks("everyman-cinema-newcastle", EverymanCinemaNewcastle),
    flicks("jam-jar-cinema", JamJarCinema),
    flicks("odeon-cinema-tyne-wear-gateshead", OdeonCinemaMetrocentre),
    flicks("odeon-cinema-tyne-wear-wallsend", OdeonCinemaSilverlink),
    flicks("empire-cinema-sunderland-tyne-wear", OmniplexSunderlandFormerlyEmpire),
    flicks("star-and-shadow-cinema-newcastle", StarAndShadowCinemaNewcastle),
    flicks("tyneside-newcastle", TynesideNewcastle),
    flicks("vue-cinemas-gateshead-tyne-wear", VueCinemasGateshead),
  )
  private val tyroneScrapers: Seq[CinemaScraper] = Seq(
    flicks("omniplex-dungannon", OmniplexDungannon),
    flicks("omniplex-omagh", OmniplexOmagh),
    flicks("ritz-multiplex-cookstown", RitzMultiplexCookstown),
  )
  private val warwickshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-rugby", CineworldRugby),
    flicks("everyman-cinema-stratford-upon-avon", EverymanCinemaStratfordUponAvon),
    flicks("odeon-cinema-coventry", OdeonCinemaCoventry),
    flicks("odeon-luxe-nuneaton", OdeonLuxeNuneaton),
    flicks("royal-spa-centre-cinema-leamington", RoyalSpaCentre),
    flicks("cinema-de-lux-coventry", ShowcaseDeLuxCoventry),
    flicks("vue-cinemas-leamington-spa", VueCinemasLeamingtonSpa),
    flicks("warwick-arts-centre-coventry", WarwickArtsCentreCoventry),
  )
  private val westSussexScrapers: Seq[CinemaScraper] = Seq(
    flicks("atrium-east-grinstead", AtriumEastGrinstead),
    flicks("capitol-horsham", CapitolHorsham),
    flicks("cineworld-crawley", CineworldCrawley),
    flicks("connaught-theatre-studio-worthing", ConnaughtTheatreStudioWorthing),
    flicks("dome-worthing", DomeWorthing),
    flicks("everyman-cinema-horsham", EverymanCinemaHorsham),
    flicks("orion-burgess-hill", OrionBurgessHill),
    flicks("picturedrome-bognor-regis", PicturedromeBognorRegis),
    flicks("windmill-littlehampton", WindmillLittlehampton),
  )
  private val westYorkshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-bradford", CineworldBradford),
    flicks("cineworld-leeds", CineworldLeeds),
    flicks("cineworld-wakefield", CineworldWakefield),
    flicks("cottage-road-cinema-leeds", CottageRoadCinemaLeeds),
    flicks("everyman-cinema-leeds", EverymanCinemaLeeds),
    flicks("heart-centre-headingley", HeartCentreHeadingley),
    flicks("picturehouse-hebden-bridge", HebdenBridgePicturehouse),
    flicks("hyde-park-picture-house-leeds", HydeParkPictureHouse),
    flicks("ilkley-cinema-ilkley", IlkleyCinema),
    flicks("odeon-cinema-huddersfield", OdeonCinemaHuddersfield),
    flicks("odeon-luxe-leeds", OdeonLuxeLeedsThorpePark),
    flicks("odeon-luxe-bradford", OdeonLuxeLeedsBradford),
    flicks("picturehouse-keighley", PictureHouseKeighley),
    flicks("national-science-and-media-museum-bradford", PicturevilleScienceAndMediaMuseumBradford),
    flicks("plaza-skipton", PlazaSkipton),
    flicks("reel-cinema-wakefield", ReelCinemaWakefield),
    flicks("rex-elland", RexElland),
    flicks("showcase-cinema-leeds", ShowcaseDeLuxLeeds),
    flicks("the-light-cinemas-bradford", TheLightBradford),
    flicks("cineworld-castleford", VueCinemasCastleford),
    flicks("vue-cinemas-halifax", VueCinemasHalifax),
    flicks("vue-cinemas-leeds-kirkstall-road", VueCinemasLeedsKirkstallRoad),
    flicks("vue-cinemas-leeds-the-light", VueCinemasLeedsTheLight),
    flicks("wetherby-film-theatre-wetherby", WetherbyFilmTheatre),
  )
  private val wiltshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-shaw-ridge-swindon", CineworldShawRidgeSwindon),
    flicks("everyman-cinema-salisbury", EverymanCinemaSalisbury),
    flicks("odeon-cinema-andover", OdeonCinemaAndover),
    flicks("odeon-cinema-salisbury", OdeonCinemaSalisbury),
    flicks("odeon-cinema-trowbridge", OdeonCinemaTrowbridge),
    flicks("palace-cinema-devizes", PalaceCinemaDevizes),
    flicks("astoria-chippenham", ReelCinemaChippenhamAstoria),
    flicks("regal-fordingbridge", RegalCinemaFordingbridge),
    flicks("the-parade-cinema-marlborough", TheParadeCinemaMarlborough),
    flicks("vue-cinemas-swindon", VueCinemasSwindon),
  )
  private val worcestershireScrapers: Seq[CinemaScraper] = Seq(
    flicks("castlemorton-cinema-morton-majestic", CastlemortonCinemaMortonMajestic),
    flicks("lume-cinema-and-cafe", FuturistCinema),
    flicks("cinema-malvern", MalvernTheatres),
    flicks("number-8-pershore", Number8Pershore),
    flicks("odeon-cinema-worcester", OdeonCinemaWorcester),
    flicks("regal-evesham", RegalCinemaEvesham),
    flicks("regal-tenbury-wells", RegalCinemaTenburyWells),
    flicks("vue-cinemas-redditch", VueCinemasRedditch),
    flicks("vue-cinemas-worcester", VueCinemasWorcester),
  )
  private val yorkshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("cineworld-sheffield", CineworldSheffield),
    flicks("odeon-luxe-sheffield", OdeonLuxeSheffield),
    flicks("paramount-penistone", ParamountPenistone),
    flicks("showroom-sheffield", ShowroomSheffield),
    flicks("the-light-cinemas-sheffield", TheLightSheffield),
    flicks("vue-cinemas-sheffield", VueCinemasSheffield),
  )

  // ── Germany (AlloCiné/Filmstarts website-JSON) ───────────────────────────
  private def filmstarts(theaterId: String, cinema: Cinema): WebediaShowtimesClient =
    new WebediaShowtimesClient(http, "www.filmstarts.de", theaterId, cinema, today = today)
  private val berlinScrapers: Seq[CinemaScraper] = Seq(
    filmstarts("A0275", CinemaxxPotsdamerPlatz),
    filmstarts("A0332", CineStarCubixAlexanderplatz),
    filmstarts("A0597", HackescheHoefeKino),
    filmstarts("A0625", KinoInternationalBerlin),
    filmstarts("A0185", KinoCentralBerlin),
  )
  private val munichScrapers: Seq[CinemaScraper] = Seq(
    filmstarts("A0025", MathaeserFilmpalast),
    filmstarts("A0943", CinemaxxMuenchen),
    filmstarts("A1610", RoyalFilmpalast),
    filmstarts("A1570", MuseumLichtspiele),
  )
  private val wurzburgScrapers: Seq[CinemaScraper] = Seq(filmstarts("A0263", CinemaxxWuerzburg))

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
    // United Kingdom (Flicks)
    "london" -> londonScrapers,
    "manchester" -> manchesterScrapers,
    "norwich" -> norwichScrapers,
    "aberdeenshire" -> aberdeenshireScrapers,
    "antrim" -> antrimScrapers,
    "armagh" -> armaghScrapers,
    "ayrshire-and-arran" -> ayrshireAndArranScrapers,
    "bedfordshire" -> bedfordshireScrapers,
    "belfast" -> belfastScrapers,
    "berkshire" -> berkshireScrapers,
    "birmingham" -> birminghamScrapers,
    "bristol" -> bristolScrapers,
    "buckinghamshire" -> buckinghamshireScrapers,
    "cambridgeshire" -> cambridgeshireScrapers,
    "cardiff" -> cardiffScrapers,
    "central-scotland" -> centralScotlandScrapers,
    "cheshire" -> cheshireScrapers,
    "clwyd" -> clwydScrapers,
    "cornwall" -> cornwallScrapers,
    "county-durham" -> countyDurhamScrapers,
    "cumbria" -> cumbriaScrapers,
    "derbyshire" -> derbyshireScrapers,
    "devon" -> devonScrapers,
    "dorset" -> dorsetScrapers,
    "down" -> downScrapers,
    "dudley" -> dudleyScrapers,
    "dumfries-and-galloway" -> dumfriesAndGallowayScrapers,
    "dunbartonshire-argyll-bute" -> dunbartonshireArgyllButeScrapers,
    "dyfed" -> dyfedScrapers,
    "east-sussex" -> eastSussexScrapers,
    "east-yorkshire" -> eastYorkshireScrapers,
    "edinburgh-and-lothians" -> edinburghAndLothiansScrapers,
    "essex" -> essexScrapers,
    "fermanagh" -> fermanaghScrapers,
    "fife" -> fifeScrapers,
    "glamorgan" -> glamorganScrapers,
    "glasgow" -> glasgowScrapers,
    "gloucestershire" -> gloucestershireScrapers,
    "guernsey" -> guernseyScrapers,
    "gwent" -> gwentScrapers,
    "gwynedd" -> gwyneddScrapers,
    "hampshire" -> hampshireScrapers,
    "herefordshire" -> herefordshireScrapers,
    "hertfordshire" -> hertfordshireScrapers,
    "highlands-and-islands" -> highlandsAndIslandsScrapers,
    "isle-of-man" -> isleOfManScrapers,
    "isle-of-wight" -> isleOfWightScrapers,
    "jersey" -> jerseyScrapers,
    "kent" -> kentScrapers,
    "lanarkshire" -> lanarkshireScrapers,
    "lancashire" -> lancashireScrapers,
    "leicestershire" -> leicestershireScrapers,
    "lincolnshire" -> lincolnshireScrapers,
    "londonderry" -> londonderryScrapers,
    "liverpool" -> liverpoolScrapers,
    "north-yorkshire" -> northYorkshireScrapers,
    "northamptonshire" -> northamptonshireScrapers,
    "northumberland" -> northumberlandScrapers,
    "nottinghamshire" -> nottinghamshireScrapers,
    "oxfordshire" -> oxfordshireScrapers,
    "powys" -> powysScrapers,
    "renfrewshire" -> renfrewshireScrapers,
    "roxburgh-ettrick-and-lauderdale" -> roxburghEttrickAndLauderdaleScrapers,
    "sandwell" -> sandwellScrapers,
    "shropshire" -> shropshireScrapers,
    "somerset" -> somersetScrapers,
    "south-yorkshire" -> southYorkshireScrapers,
    "staffordshire" -> staffordshireScrapers,
    "suffolk" -> suffolkScrapers,
    "surrey" -> surreyScrapers,
    "tayside" -> taysideScrapers,
    "tyne-and-wear" -> tyneAndWearScrapers,
    "tyrone" -> tyroneScrapers,
    "warwickshire" -> warwickshireScrapers,
    "west-sussex" -> westSussexScrapers,
    "west-yorkshire" -> westYorkshireScrapers,
    "wiltshire" -> wiltshireScrapers,
    "worcestershire" -> worcestershireScrapers,
    "yorkshire" -> yorkshireScrapers,
    // Germany (Filmstarts)
    "berlin"     -> berlinScrapers,
    "munich"     -> munichScrapers,
    "wurzburg"   -> wurzburgScrapers,
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
