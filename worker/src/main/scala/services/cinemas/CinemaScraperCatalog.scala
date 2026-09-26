package services.cinemas

import models._
import tools.{CachingDetailFetch, HttpFetch}
import services.cinemas.common.{CinemaScraper, GatsbyBoxOfficeClient, MultiListingScraper, VueCinemasPlatformClient, WebediaMarket, WebediaShowtimesClient, ZyteFallback}
import services.cinemas.pl._
import services.cinemas.common.{FlicksClient, FlicksMarket, KinoprogrammClient}
import services.cinemas.uk.{CineworldClient, OdeonClient, TheOldCourtClient}
import services.cinemas.es.OcineClient
import services.cinemas.us.{AlamoDrafthouseClient, UsChainVenues}
import services.movies.TitleNormalizer

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
 *                  ctor defaults it to `ZyteFallback.fetchFor(http, …)`, and the
 *                  fixture wiring overrides it back to `http`.
 *   - `flicksFetch` — Decodo residential egress for www.flicks.co.uk, which
 *                  Cloudflare 403s from our Fly datacenter IP. Flicks is the
 *                  ONLY UK source, so this seam carries all ~843 UK venues; a
 *                  direct fetch blacks out the whole country (2026-07-26). The
 *                  diagnostic ctor + fixture wiring default it back to `http`.
 *   - `odeonFetch` — Decodo residential egress for vwc.odeon.co.uk. Odeon's
 *                  ocapi backend used to answer our datacenter IP directly, but
 *                  Cloudflare 403s it from the Hetzner egress the worker moved to
 *                  on 2026-08-29, which took all 102 Odeon venues red at once.
 *                  Same shape (and same remedy) as `flicksFetch`.
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
  // Builds the per-chain detail-page cache, taking the chain it is for. The worker
  // injects a Mongo-backed cache so chain detail is deduped across servers; the
  // diagnostic ctor + tests default to the in-process CachingDetailFetch. The chain name
  // is not decoration — each chain's cache carries its own TTL, so each needs its own
  // store (see `MongoCachingDetailFetch.collectionName`).
  chainDetailCache: (String, HttpFetch, FiniteDuration) => HttpFetch,
  // Zyte residential egress for venues whose firewall blocks both our Fly IP and
  // the Decodo proxy (see the ctor doc). No primary-ctor default — Scala can't
  // reference `http` here — so the secondary ctor and WorkerWiring supply it.
  zyteFetch: HttpFetch,
  // Residential-proxy egress for flicks.co.uk — every UK venue (see the ctor
  // doc). Same no-default reason as `zyteFetch`.
  flicksFetch: HttpFetch,
  // Host-sticky residential egress for Vue/CinemaxX: their films API is
  // Cloudflare-403'd from our Fly IP AND token-cookie-gated, so it needs a
  // residential IP that STAYS the same across the token POST + films GET. Own
  // param (not flicksFetch) because flicksFetch is per-venue sticky, which would
  // split the POST and GET onto different IPs. Defaults to `http` in the secondary
  // ctor/tests.
  vueFetch: HttpFetch,
  // Residential-proxy egress for Odeon's Vista ocapi backend (see the ctor doc).
  // Own param rather than reusing `flicksFetch` so an Odeon venue keeps its flicks
  // FALLBACK on the flicks seam while its own-site primary rides this one. Same
  // no-default reason as `zyteFetch`.
  odeonFetch: HttpFetch,
  // Supplies Odeon's short-lived Vista JWT (harvested via Zyte in prod, see
  // [[services.cinemas.uk.OdeonAuthHarvester]]). No primary-ctor default (Scala 3
  // forbids two overloaded ctors both carrying defaults); the diagnostic ctor +
  // tests pass `() => None` so Odeon clients simply throw (→ flicks fallback)
  // without needing Zyte, and WorkerWiring injects the live harvester.
  odeonAuthToken: () => Option[String],
  // The title rules the scrapers below clean their raw titles with. Country-scoped:
  // the " & " → " i " unification is Poland-only, and applying it to a German venue
  // re-keys "Minions & Monster" as `minionsimonster`. `WorkerWiring` passes the rule
  // set for the country it was booted for; the diagnostic ctor defaults to Poland's.
  titles: TitleNormalizer
) {

  /** Diagnostic ctor: the Zyte-routed fetches (Multikino's API, biletyna's venue
   *  pages) default to the path derived from `http` (a clean body-derived
   *  default, not the old `null`-parameter workaround — Scala can't reference `http`
   *  in a primary-constructor default, but a secondary constructor can).
   *  `WorkerWiring` uses the primary ctor to inject its (possibly
   *  fixture-overridden) `multikinoFetch` / `biletynaFetch`. Each Zyte chain gets a
   *  client of its own, built only if `configuration` carries ZYTE_API_KEY — a diagnostic is not worth
   *  a shared one. `configuration` is the caller's: a tool's `main` passes the process's, a spec none. */
  def this(http: HttpFetch, today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw")),
           titles: TitleNormalizer = TitleNormalizer.forCountry(Country.default),
           configuration: settings.ProcessConfiguration = new settings.ProcessConfiguration(tools.Env.of())) =
    this(http, MultikinoClient.fetchFor(http, ZyteFallback.newHttpClient(), configuration),
      ZyteFallback.fetchFor(http, ZyteFallback.newHttpClient(), configuration), today,
      (_, h, ttl) => new CachingDetailFetch(h, ttl),
      zyteFetch = ZyteFallback.fetchFor(http, ZyteFallback.newHttpClient(), configuration),
      // No residential proxy outside WorkerWiring — a diagnostic runs from a
      // developer's own (unblocked) IP, so plain `http` is the right default.
      flicksFetch = http, vueFetch = http, odeonFetch = http,
      // A diagnostic has no Zyte harvester wired, so Odeon venues throw → flicks fallback.
      odeonAuthToken = () => None,
      titles = titles)

  // Per-film detail bodies are static between passes and IDENTICAL across a
  // chain's locations, so each chain shares ONE CachingDetailFetch: a film's
  // detail (Helios `/api/movie/{id}`, Cinema City film page) is fetched once per
  // chain per TTL instead of once per location per pass. Live listing/screening
  // fetches stay on `http`.
  //
  // BOTH EXPIRE INSIDE THE `DetailEnrich` REFRESH WINDOW, and `CinemaScraperCatalogSpec`
  // holds them to it. Cinema City sat at 6h, EQUAL to that window, so whether a
  // scheduled refresh did any work was decided by which of the two elapsed first
  // — and on the losing side it re-parsed the bytes it already had, produced the
  // identical detail, and stamped it fresh. Helios earns its cache on a different
  // axis and keeps a shorter one still: it fetches movie and screen bodies from
  // `fetchRestData()`, INSIDE the scrape pass, once per id in the listing.
  val heliosDetailTtl:     FiniteDuration = 2.hours
  val cinemaCityDetailTtl: FiniteDuration = 2.hours
  private val heliosDetailHttp:     HttpFetch = chainDetailCache("helios", http, heliosDetailTtl)
  private val cinemaCityDetailHttp: HttpFetch = chainDetailCache("cinema-city", http, cinemaCityDetailTtl)
  private def helios(config: HeliosCinema): HeliosClient =
    new HeliosClient(http, config, today, Some(heliosDetailHttp), titles = titles)

  // The three venue clients this catalog builds MANY of — 36 Bilety24 organisers,
  // 5 Ekobilet venues, 3 NoveKino — bind their shared arguments here rather than
  // repeating them at every site, the way `multikino` and `helios` already do.
  private def bilety24(organizerUrl: String, cinema: Cinema): Bilety24OrganizerClient =
    new Bilety24OrganizerClient(http, organizerUrl, cinema, titles = titles)
  private def ekobilet(slug: String, cinema: Cinema): EkobiletClient =
    new EkobiletClient(http, slug, cinema, today)
  private def noveKino(slug: String, cinema: Cinema): NoveKinoClient =
    new NoveKinoClient(http, slug, cinema)
  private def filmweb(filmwebCinemaId: Int, cinema: Cinema): FilmwebShowtimesClient =
    new FilmwebShowtimesClient(http, filmwebCinemaId, cinema, today = today)

  // ── The four Polish ticketing platforms that host MANY venues ─────────────
  // One table per platform — each venue's page or portal URL (plus the odd
  // per-venue knob) beside its Cinema — and one factory binding the shared
  // arguments, the way Germany / Spain / the US build every scraper off their
  // rosters below. The city lists keep naming each venue in scrape order; only
  // what used to be a repeated constructor moved here.

  // MSI / VisualTicket portals (29). `fetch` is Zyte's residential egress for
  // the one venue whose origin firewall blocks both our Fly IP and the Decodo
  // proxy (see the ctor doc); `mvcPath` / `titlePrefix` / `titleSuffix` are the
  // per-install quirks `MsiClient` documents.
  private case class MsiVenue(baseUrl:     String,
                              mvcPath:     String         = MsiClient.DefaultMvcPath,
                              titlePrefix: Option[String] = None,
                              titleSuffix: Option[String] = None,
                              fetch:       HttpFetch      = http)
  private val msiVenues: Map[Cinema, MsiVenue] = Map(
    KinoKryterium                -> MsiVenue("https://bilety.ck105.koszalin.pl", fetch = zyteFetch),
    KinoMillenium                -> MsiVenue("https://bilety.csm.tarnow.pl", mvcPath = "/Kino/mvc/pl"),
    KinoKinomax                  -> MsiVenue("https://bilety.kinomax.info.pl"),
    KinoCentrumSkarzyskoKamienna -> MsiVenue("https://bilet-mck.skarzysko.pl"),
    KinoWRatuszu                 -> MsiVenue("https://bilety.mdk-zdunskawola.pl"),
    KinoKozienickiDomKultury     -> MsiVenue("http://bilety.dkkozienice.pl"),
    KinoSwitZwolen               -> MsiVenue("https://bilety.switzwolen.pl"),
    KinoMCK                      -> MsiVenue("https://bilety.kinolezajsk.pl"),
    KinoSniezka                  -> MsiVenue("https://bilety.mokdebica.pl"),
    KinoWarszawa                 -> MsiVenue("https://bilety-kino.przeworsk.um.gov.pl"),
    KinoCinemaLumiere            -> MsiVenue("https://bilety.kinoszczytno.pl"),
    KinoIgnacy                   -> MsiVenue("https://www.biletyignacy.pl"),
    KinoChemik                   -> MsiVenue("https://bilety.mok.com.pl", titlePrefix = Some("Chemik")),
    KinoTwierdza                 -> MsiVenue("https://bilety.mok.com.pl", titlePrefix = Some("TWIERDZA")),
    KinoPowisle                  -> MsiVenue("https://kinosztumbilety.pl"),
    KinoBajkaDarlowo             -> MsiVenue("https://darlowo.vectorsoft.pl"),
    KinoGOK                      -> MsiVenue("https://bilety.goktychowo.pl"),
    KinoGoplana                  -> MsiVenue("https://bilety.ckpolczyn.pl"),
    KinoWybrzeze                 -> MsiVenue("http://bilety.rck.kolobrzeg.pl", titleSuffix = Some("KINO WYBRZEŻE")),
    KinoMaxKino                  -> MsiVenue("https://repertuar.maxkino.eu"),
    KinoSDKSwiebodzin            -> MsiVenue("https://bilety.kino.swiebodzin.pl:4433"),
    KinoPlanetCinema             -> MsiVenue("https://oswiecim.planetcinema.pl"),
    KinoPlanetCinemaElk          -> MsiVenue("https://elk.planetcinema.pl"),
    KinoPlanetCinemaZabrze       -> MsiVenue("https://zabrze.planetcinema.pl"),
    KinoPlanetCinemaZawiercie    -> MsiVenue("https://zawiercie.planetcinema.pl"),
    KinoMOKNowaRuda              -> MsiVenue("https://bilety.nowaruda.pl"),
    KinoStopiak                  -> MsiVenue("https://bilety.stopiakcinema.pl"),
    KinoPlaneta                  -> MsiVenue("https://rezerwacja.planetabrzesko.pl", mvcPath = "/Rezerwacja/mvc/pl"),
    KinoJutrzenka                -> MsiVenue("https://kino.sierpc.pl"),
    KinoNoweKinoWarszawa         -> MsiVenue("http://bilety.mck-gostynin.pl"),
    KinoZaRogiem                 -> MsiVenue("https://bilety.pokis.pl"),
    KinoODEON                    -> MsiVenue("https://kinoodeon.eurobilet.pl"),
    KinoIkar                     -> MsiVenue("https://kinoikar.mok-jar.pl"),
    KinoNaBiegunach              -> MsiVenue("https://jaroslaw.kinonabiegunach.pl"),
    KinoMOKMiedzyrzecz           -> MsiVenue("https://bilety.mokmiedzyrzecz.pl"),
    // Kino Manhattan's TLS leaf cert doesn't validate from here (times out
    // over https, plain http serves the identical page) — same shape as the
    // three expired-cert venues MsiClient's baseUrl doc names.
    KinoManhattanJanikowo       -> MsiVenue("http://bilety.mgok.janikowo.com.pl"),
    KinoAleKinoLibiaz           -> MsiVenue("https://biletylck.libiaz.pl"),
    KinoWolnoscSzczecinek       -> MsiVenue("https://bilety.sapik.pl"),
  )
  private def msi(cinema: Cinema): MsiClient = {
    val venue = msiVenues(cinema)
    new MsiClient(venue.fetch, venue.baseUrl, cinema, today,
      mvcPath = venue.mvcPath, titlePrefix = venue.titlePrefix, titleSuffix = venue.titleSuffix)
  }

  // biletyna.pl venue pages (plus Końskie's two below). biletyna.pl 403s our datacenter IP (Cloudflare
  // waiting-room), so every one routes through `bnFetch` — Zyte's residential
  // egress in prod, the fixture fake in tests.
  private val biletynaPages: Map[Cinema, String] = Map(
    KinoMCKTkacz -> "https://biletyna.pl/Tomaszow-Mazowiecki/Miejskie-Centrum-Kultury-Filia-Tkacz",
    KinoMDKOpoczno -> "https://biletyna.pl/Opoczno/Miejski-Dom-Kultury-im-Tadeusza-Sygietynskiego",
    KinoDomSztukiUrsynow -> "https://biletyna.pl/Warszawa/Kino-Domu-Sztuki-Ursynow",
    KinoTerminalKultury -> "https://biletyna.pl/Warszawa/Terminal-Kultury",
    KinoBielanskiOsrodekKultury -> "https://biletyna.pl/Warszawa/Bielanski-Osrodek-Kultury",
    KinoWawerskaStrefaKultury -> "https://biletyna.pl/Warszawa/Wawerska-Strefa-Kultury-sala-widowiskowa",
    KinoBasn -> "https://biletyna.pl/Piastow/sala-widowiskowa",
    KinoKasynoOficerskie -> "https://biletyna.pl/Nowy-Dwor-Mazowiecki/Kasyno-Oficerskie-Sala-kinowa",
    KinoNowyPafawag -> "https://biletyna.pl/Wroclaw/Centrum-Kultury-Wroclaw-Zachod",
    TeatrBoto -> "https://biletyna.pl/Sopot/Teatr-Boto",
    TeatrAtelier -> "https://biletyna.pl/Sopot/Teatr-Atelier-Sopot",
    KinoCKGniewino -> "https://biletyna.pl/Gniewino/Centrum-Kultury-Sportu-Turystyki-i-Biblioteka-w-Gniewinie",
    KinoMOKGlogow -> "https://biletyna.pl/Glogow/Miejski-Osrodek-Kultury",
    TeatrGryphius -> "https://biletyna.pl/Glogow/Teatr-im-Andreasa-Gryphiusa",
    KinoCKSwiecie -> "https://biletyna.pl/Swiecie/Sala-Widowiskowa-Osrodek-Kultury-Sportu-i-Rekreacji",
    KinoMOKGlowno -> "https://biletyna.pl/Glowno/Miejski-Osrodek-Kultury",
    KinoPromienWiecbork -> "https://biletyna.pl/Wiecbork/Kino-Promien",
    KinoCKiSSepolno -> "https://biletyna.pl/Sepolno-Krajenskie/Centrum-Kultury-i-Sztuki",
    KinoGOKLipka -> "https://biletyna.pl/Lipka/Gminny-Osrodek-Kultury",
    KinoNOKNasielsk -> "https://biletyna.pl/Nasielsk/Nasielski-Osrodek-Kultury",
    KinoFeniks -> "https://biletyna.pl/Rydultowy/Rydultowskie-Centrum-Kultury-sala-teatralna",
    KinoCKPrzezmierowo -> "https://biletyna.pl/Przezmierowo/Dom-Kultury",
    KinoNOKNowyTomysl -> "https://biletyna.pl/Nowy-Tomysl/Nowotomyski-Osrodek-Kultury",
    KinoMewaMiedzychod -> "https://biletyna.pl/Miedzychod/Kino-Mewa",
    KinoPromyk -> "https://biletyna.pl/Bystra/Gminny-Osrodek-Kultury-Promyk",
    KinoKsiazka -> "https://biletyna.pl/Stara-Blotnica/Kino-KSIAZKA-w-GPB-w-St-Blotnicy",
    KinoChDKChoszczno -> "https://biletyna.pl/Choszczno/Choszczenski-Dom-Kultury",
    KinoDOKDrawno -> "https://biletyna.pl/Drawno/Drawienski-Osrodek-Kultury",
    KinoMGOKRecz -> "https://biletyna.pl/Recz/Miejsko-Gminny-Osrodek-Kultury",
    KinoOKJastrowie -> "https://biletyna.pl/Jastrowie/Osrodek-Kultury",
    AdaKinoStudyjne             -> "https://www.biletyna.pl/Warszawa/ADA-Kino-Studyjne",
    // Filmweb (320 / 3149) went empty for both on ~2026-09-22; biletyna sells
    // every ticket for the cinema and for the CKiS concert hall's film nights.
    KinoPolonez                 -> "https://biletyna.pl/Skierniewice/Kinoteatr-Polonez",
    KinoCKiSSkierniewice        -> "https://biletyna.pl/Skierniewice/Centrum-Kultury-i-Sztuki-Sala-koncertowa",
    // Filmweb had fallen out of step with the box office for these six by
    // 2026-09-26 (one or two stray slots against a full biletyna week).
    KinoGiewont                 -> "https://biletyna.pl/Zakopane/Kino-Giewont",
    KinoKulturaWolomin          -> "https://biletyna.pl/Wolomin/Kino-Kultura",
    KinoCentrumWadowice         -> "https://biletyna.pl/Wadowice/Wadowickie-Centrum-Kultury",
    KinoLen                     -> "https://biletyna.pl/Zyrardow/Kino-Len",
    KinoApolloDzialdowo         -> "https://biletyna.pl/Dzialdowo/Miejski-Dom-Kultury",
    KinoZaRogiemChmielno        -> "https://biletyna.pl/Chmielno/Kino-za-Rogiem-w-Chmielnie",
    KinoKameralne               -> "https://biletyna.pl/Gdansk/Kino-Kameralne-Cafe",
    KinoTur                     -> "https://biletyna.pl/Turek/Kino-Tur",
    KinoMok                     -> "https://biletyna.pl/Zagorow/Gminny-Osrodek-Kultury",
    KinoPDK                     -> "https://biletyna.pl/Pyrzyce/Pyrzycki-Dom-Kultury",
    KinoNaSzekspirowskim        -> "https://biletyna.pl/Gdansk/Kino-na-Szekspirowskim",
    KinoRondo                   -> "https://biletyna.pl/Chelmno/Kinoteatr-Rondo",
    KinoMiejskieCentrumKultury  -> "https://biletyna.pl/Aleksandrow-Kujawski/Miejskie-Centrum-Kultury",
    KinoZdroj                   -> "https://biletyna.pl/Ciechocinek/Kino-Zdroj",
    KinoSwitCzechowiceDziedzice -> "https://biletyna.pl/Czechowice-Dziedzice/Kino-Swit",
    KinoTeatrElektryczny        -> "https://biletyna.pl/Skoczow/Teatr-Elektryczny",
    KinoPegaz                   -> "https://biletyna.pl/Wodzislaw-Slaski/Wodzislawskie-Centrum-Kultury",
    KinoZulawskiOsrodekKultury  -> "https://biletyna.pl/Nowy-Dwor-Gdanski/Zulawski-Osrodek-Kultury",
    KinoDK                      -> "https://biletyna.pl/Slawno/Slawienski-Dom-Kultury",
    KinoGCK                     -> "https://biletyna.pl/Solec-Zdroj/Kino-Solec-Zdroj",
    KinoNawojka                 -> "https://biletyna.pl/Lipno/Kino-Nawojka",
    KinoSCKStrzegom             -> "https://biletyna.pl/Strzegom/Kino-SCK",
    KinoSztuka                  -> "https://biletyna.pl/Chrzanow/Miejski-Osrodek-Kultury-Sportu-i-Rekreacji",
    KinoGryfinskiDomKultury      -> "https://biletyna.pl/Gryfino/Sala-widowiskowa-Gryfinskiego-Domu-Kultury",
    KinoTucholskiOsrodekKultury  -> "https://biletyna.pl/Tuchola/Tucholski-Osrodek-Kultury",
  )
  private def biletyna(cinema: Cinema): BiletynaClient =
    new BiletynaClient(bnFetch, BiletynaPlacePage(biletynaPages(cinema)), cinema)

  // Końskie's culture centre lists each hall on a biletyna page of its own. The
  // stage hall is mostly cabaret and concerts but does screen the odd film
  // ("Popiełuszko - wolność jest w nas", 2026-10-20), so both halls are ONE
  // cinema, each showtime tagged with its hall.
  private val koneckieCentrumKultury: CinemaScraper = new MultiListingScraper(KinoKoneckieCentrumKultury, Seq(
    new BiletynaClient(bnFetch, BiletynaPlacePage("https://biletyna.pl/Konskie/Koneckie-Centrum-Kultury-sala-kinowa"),
      KinoKoneckieCentrumKultury, room = Some("sala kinowa")),
    new BiletynaClient(bnFetch, BiletynaPlacePage("https://biletyna.pl/Konskie/Koneckie-Centrum-Kultury-sala-widowiskowa"),
      KinoKoneckieCentrumKultury, room = Some("sala widowiskowa")),
  ))

  // systembiletowy.pl installs, by portal base URL.
  private val systemBiletowyPortals: Map[Cinema, String] = Map(
    KinoKawiarnia   -> "https://kgl.systembiletowy.pl",
    KinoKuznica     -> "https://shd.systembiletowy.pl",
    KinoPckulKino   -> "https://bilety.pckul.pl",
    KinoNaStarowce  -> "https://bilety.mok.zory.pl",
    KinoCentrum3D   -> "https://kck.systembiletowy.pl",
    KinoNaszeKino   -> "https://ock.systembiletowy.pl",
    KinoFarys       -> "https://kfb.systembiletowy.pl",
    KinoRegis       -> "https://bilety.kino.bochnia.pl",
    KinoKalejdoskop -> "https://ckp.systembiletowy.pl",
    KinoKadrStaszow -> "https://sta.systembiletowy.pl",
    KinoBieszczadzkiDK -> "https://bdk.systembiletowy.pl",
    Kino1410        -> "https://kht.systembiletowy.pl",
    KinoMikro       -> "https://bilety.kinomikro.pl",
    MikroBronowice  -> "https://bilety.kinomikro.pl",
    KinoCKiBNowaSarzyna -> "https://oks.systembiletowy.pl",
  )
  // Instances that also sell someone else's events, scoped to the venue's own
  // `location.institution_name` (see SystemBiletowyClient).
  private val systemBiletowyInstitutions: Map[Cinema, Institution] = Map(
    KinoNaszeKino  -> Institution("Nasze Kino"),   // Oświęcim's culture centre sells its concerts on the same instance
    KinoMikro      -> Institution("Kino Mikro"),   // the two Mikro screens share one instance
    MikroBronowice -> Institution("Mikro Bronowice"),
  )
  private def systemBiletowy(cinema: Cinema): SystemBiletowyClient =
    new SystemBiletowyClient(http, VisualSoftPortal(systemBiletowyPortals(cinema)), cinema, titles = titles,
      institution = systemBiletowyInstitutions.get(cinema))

  // Venues on their own `<venue>.bilety24.pl` subdomain (7) — as opposed to the
  // organiser listings on bilety24.pl itself that `bilety24` above builds.
  private val bilety24Subdomains: Map[Cinema, String] = Map(
    KinoLuna            -> "https://kinoluna.bilety24.pl",
    KinoteatrRialto     -> "https://kinoteatrrialto.bilety24.pl",
    KinoCkLublin        -> "https://ck-lublin.bilety24.pl",
    KinoApolloWalbrzych -> "https://kino-apollo.bilety24.pl",
    KinoPiast           -> "https://kino-piast.bilety24.pl",
    KinoLot             -> "https://kino-lot.bilety24.pl",
    KinoOskard          -> "https://ckis-konin.bilety24.pl",
  )
  private def bilety24Subdomain(cinema: Cinema): Bilety24Client =
    new Bilety24Client(http, bilety24Subdomains(cinema), cinema, titles = titles)

  // Shared per-source helper clients the scrapers below reuse.
  val cinemaCityClient: CinemaCityClient = new CinemaCityClient(http, Some(cinemaCityDetailHttp), titles = titles)
  // One per Cinema City venue.
  private def cinemaCity(cinemaId: String, cinema: Cinema): CinemaCityScraper =
    new CinemaCityScraper(cinemaCityClient, cinemaId, cinema)
  // One per Multikino venue — 31 of them, so the shared `mkFetch` + `titles` are
  // bound here rather than repeated at every site. Defaults mirror
  // `MultikinoClient`'s: Poznań's Stary Browar.
  private def multikino(cinemaId: String = MultikinoClient.PoznanStaryBrowarId,
                        cinema:   Cinema = Multikino): MultikinoClient =
    new MultikinoClient(mkFetch, cinemaId, cinema, titles)
  val kinoMuzaClient:   KinoMuzaClient   = new KinoMuzaClient(http, today, titles = titles)

  private val poznanScrapers: Seq[CinemaScraper] = Seq(
    multikino(),
    new CharlieMonroeClient(http),
    new KinoPalacoweClient(http, titles = titles),
    helios(HeliosNuxt.Poznan),
    cinemaCity("1078", CinemaCityPoznanPlaza),
    cinemaCity("1081", CinemaCityKinepolis),
    kinoMuzaClient,
    new KinoBulgarskaClient(http, today),
    new KinoApolloClient(http, titles = titles),
    new RialtoClient(http),
  )

  private val wroclawScrapers: Seq[CinemaScraper] = Seq(
    cinemaCity("1097", CinemaCityWroclavia),
    cinemaCity("1067", CinemaCityKorona),
    multikino("0010", MultikinoPasazGrunwaldzki),
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
    multikino("0013", MultikinoZloteTarasy),
    multikino("0040", MultikinoMlociny),
    multikino("0052", MultikinoReduta),
    multikino("0024", MultikinoTargowek),
    multikino("0025", MultikinoWolaPark),
    helios(HeliosNuxt.BlueCity),
    new MuranowClient(http, today),
    bilety24Subdomain(KinoLuna),
    bilety24("https://www.bilety24.pl/kino/organizator/kino-elektronik-631", KinoElektronik),
    new IluzjonClient(http, today),
    new KinoGramClient(http),
    new KinoKulturaClient(http),
    new AmondoClient(http),
    new BokClient(http, "kino-na-boku", KinoNaBoku, today, titles = titles),
    new BokClient(http, "kino-glebocka-66", KinoGlebocka66, today, titles = titles),
    new KinomuzeumClient(http, today),
    new SwitClient(http),
    new PromKepaClient(http),
    new FalenicaClient(http),
    new SdkClient(http),
    noveKino("atlantic", KinoAtlantic),
    new KinotekaClient(http, titles),
    new UjazdowskiClient(http, today),
    new CytadelaClient(http),
    noveKino("wisla", KinoWisla),
    biletyna(AdaKinoStudyjne),
    new AlternatywyClient(http, today, titles = titles),
    filmweb(2436, KinoWKadrze),
  )

  private val krakowScrapers: Seq[CinemaScraper] = Seq(
    cinemaCity("1090", CinemaCityBonarka),
    cinemaCity("1076", CinemaCityKazimierz),
    cinemaCity("1064", CinemaCityZakopianka),
    multikino("0005", MultikinoKrakow),
    systemBiletowy(KinoMikro),
    systemBiletowy(MikroBronowice),
    new KinoSfinksClient(http, KinoSfinks),
    new KinoPodBaranamiClient(http, KinoPodBaranami, today),
    new KinoKijowClient(http, KinoKijow, today, titles = titles),
    new KinoKikaClient(http, KinoKika),
    new KinoAgrafkaClient(http, KinoAgrafka),
    new KinoParadoxClient(http, KinoParadox),
  )

  private val lodzScrapers: Seq[CinemaScraper] = Seq(
    cinemaCity("1080", CinemaCityManufaktura),
    multikino("0023", MultikinoLodz),
    helios(HeliosNuxt.Lodz),
    new CharlieClient(http, KinoCharlie),
    new KinematografLodzClient(http, KinematografLodz, today, titles = titles),
    new NckfClient(http, Nckf, today),
    new KinoTatryClient(http, KinoTatry, today),
  )

  private val katowiceScrapers: Seq[CinemaScraper] = Seq(
    cinemaCity("1065", CinemaCityPunkt44),
    cinemaCity("1079", CinemaCitySilesia),
    multikino("0035", MultikinoKatowice),
    helios(HeliosNuxt.Katowice),
    // Silesia Film's art-house trio, all Bilety24-hosted: listing at `/repertuar/`
    // linking per-film `/wydarzenie/?id=N` pages, so they reuse the shared Bilety24Client.
    bilety24("https://www.bilety24.pl/kino/organizator/kino-kosmos-1501", KinoKosmos),
    bilety24("https://www.bilety24.pl/kino/organizator/kino-swiatowid-1503", KinoSwiatowid),
    bilety24Subdomain(KinoteatrRialto),
  )

  private val szczecinScrapers: Seq[CinemaScraper] = Seq(
    helios(HeliosNuxt.Szczecin),
    multikino("0007", MultikinoSzczecin),
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
    multikino("0004", MultikinoGdansk),
    helios(HeliosNuxt.Metropolia),
    helios(HeliosNuxt.Forum),
    helios(HeliosNuxt.Riviera),
    new KinoSpektrumClient(http, KinoSpektrum),
    // biletyna.pl 403s our datacenter IP, so route through `bnFetch` — Zyte's
    // residential egress in production, the fixture fake in tests. See
    // WorkerWiring.biletynaFetch / ZyteFallback.
    biletyna(KinoKameralne),
    new KinoIkmClient(http, KinoIkm, today),
    new KinoMuzeumGdanskClient(http, KinoMuzeumGdansk),
    new KinoZakClient(http, KinoZak, today),
    // KinoPort (GCSW, formerly CSW Łaźnia, Gdańsk) was read off Filmweb (id 1735)
    // after a 2026-06 site rebuild dropped its stable programme alias. Filmweb
    // then went silently empty for it — `[]` on every date while the venue was
    // screening five films a day — so we read GCSW's own repertoire post instead,
    // via the WP REST route that survives the post's rotating permalink.
    new KinoPortClient(http, KinoPort, today),
    // Migrated off the legacy MSI portal onto the "POSitive Cinema" Angular SPA
    // platform in 2026; the SPA renders no server HTML, so this reads the
    // platform's own JSON REST API directly (see Cinema1Client's scaladoc).
    new Cinema1Client(http, Cinema1Gdansk, cinemaId = "8d3b10d9-f892-4f57-bf74-9f86905ce3ea", today = today),
    new GdynskieCentrumFilmoweClient(http, GdynskieCentrumFilmowe),
  )

  private val bydgoszczScrapers: Seq[CinemaScraper] = Seq(
    cinemaCity("1086", CinemaCityBydgoszcz),
    multikino("0006", MultikinoBydgoszcz),
    helios(HeliosNuxt.Bydgoszcz),
    new KinoOrzelClient(http, KinoOrzel),
  )

  private val lublinScrapers: Seq[CinemaScraper] = Seq(
    cinemaCity("1094", CinemaCityLublinFelicity),
    cinemaCity("1084", CinemaCityLublinPlaza),
    multikino("0034", MultikinoLublin),
    new KinoBajkaClient(http, KinoBajka, titles = titles),
    bilety24Subdomain(KinoCkLublin),
    new KinoChatkaZakaClient(http, KinoChatkaZaka),
  )

  private val czestochowaScrapers: Seq[CinemaScraper] = Seq(
    cinemaCity("1089", CinemaCityCzestochowaJurajska),
    cinemaCity("1075", CinemaCityCzestochowaWolnosc),
    new OkfIluzjaClient(http, OkfIluzja, today),
  )

  private val radomScrapers: Seq[CinemaScraper] = Seq(
    helios(HeliosNuxt.Radom),
    multikino("0026", MultikinoRadom),
    new McswElektrowniaCinemaClient(http, McswElektrowniaCinema, today),
  )

  private val sosnowiecScrapers: Seq[CinemaScraper] = Seq(
    helios(HeliosNuxt.Sosnowiec),
    cinemaCity("1083", CinemaCitySosnowiec),
  )

  private val torunScrapers: Seq[CinemaScraper] = Seq(
    cinemaCity("1077", CinemaCityTorunCzerwonaDroga),
    cinemaCity("1093", CinemaCityTorunPlaza),
    new KinoCentrumCswClient(http, KinoCentrumCsw),
    // Kino 1410's own site (kino1410.pl) links kht.systembiletowy.pl for booking —
    // an existing platform, so it reuses SystemBiletowyClient rather than a new
    // client. Mostly "event cinema" broadcasts (André Rieu, Met Opera) alongside
    // the odd real film/classic-restoration screening; OnlyMovieEventsFilter
    // keeps the broadcasts (see NonMovieEventClassifier's broadcast veto) and
    // drops anything else event-shaped.
    systemBiletowy(Kino1410),
  )

  private val kielceScrapers: Seq[CinemaScraper] = Seq(
    helios(HeliosNuxt.Kielce),
    multikino("0029", MultikinoKielce),
    // iframe639.biletyna.pl 403s our Fly datacenter IP (Cloudflare) on the
    // per-film /artist/view/id detail pages, so route through the biletyna seam
    // (residential proxy → Zyte) like every other biletyna venue — else the
    // deferred detail enrichment fetches all 403 and the enrichment bar goes red.
    new KinoFenomenClient(bnFetch, KinoFenomen),
    new KinoMoskwaClient(http, KinoMoskwa, today),
  )

  private val rzeszowScrapers: Seq[CinemaScraper] = Seq(
    helios(HeliosNuxt.Rzeszow),
    multikino("0028", MultikinoRzeszow),
    new KinoZorzaClient(http, KinoZorza, today),
    new KinoZaRogiemCafeClient(http, KinoZaRogiemCafe, today),
  )

  private val gliwiceScrapers: Seq[CinemaScraper] = Seq(
    cinemaCity("1085", CinemaCityGliwice),
    new KinoAmokClient(http, KinoAmok, today),
  )

  private val zabrzeScrapers: Seq[CinemaScraper] = Seq(
    multikino("0003", MultikinoZabrze),
    new KinoRomaClient(http, KinoRoma, today),
    msi(KinoPlanetCinemaZabrze),
  )

  // ── New mid-size cities ─────────────────────────────────────────────────────
  // Chain ids verified against each chain's own cinema-list API. Each city's
  // independent screen, where one has a machine-readable source, is added via a
  // shared platform client: FilmwebShowtimesClient (Filmweb's seances JSON, by
  // internal cinema id — verified to return non-empty seances), Bilety24Client
  // (a bilety24.pl venue), or NoveKinoClient.
  private val olsztynScrapers      = Seq(helios(HeliosNuxt.Olsztyn), multikino("0036", MultikinoOlsztyn), new KinoAwangarda2Client(http, today))
  private val bielskoBialaScrapers = Seq(helios(HeliosNuxt.BielskoBiala), cinemaCity("1088", CinemaCityBielskoBiala), new KinoKreskaClient(http, KinoKreska, today))
  private val opoleScrapers        = Seq(helios(HeliosNuxt.OpoleKarolinka), helios(HeliosNuxt.OpoleSolaris), ekobilet("opolskielamy", KinoMeduza))
  private val rybnikScrapers       = Seq(multikino("0014", MultikinoRybnik), cinemaCity("1082", CinemaCityRybnik))
  private val gorzowScrapers       = Seq(helios(HeliosNuxt.Gorzow), multikino("0047", MultikinoGorzow), bilety24("https://www.bilety24.pl/kino/organizator/miejski-osrodek-sztuki-kino-60-krzesel-dkf-megaron-776", Kino60Krzesel))
  private val elblagScrapers       = Seq(multikino("0037", MultikinoElblag), cinemaCity("1099", CinemaCityElblag))
  private val koszalinScrapers     = Seq(helios(HeliosNuxt.Koszalin), multikino("0015", MultikinoKoszalin), msi(KinoKryterium))
  private val kaliszScrapers       = Seq(helios(HeliosNuxt.Kalisz), multikino("0042", MultikinoKalisz))
  private val zielonaGoraScrapers  = Seq(cinemaCity("1087", CinemaCityZielonaGora))
  private val tychyScrapers        = Seq(multikino("0053", MultikinoTychy))
  private val walbrzychScrapers    = Seq(cinemaCity("1091", CinemaCityWalbrzych), bilety24Subdomain(KinoApolloWalbrzych))
  private val tarnowScrapers       = Seq(multikino("0050", MultikinoTarnow), msi(KinoMillenium), new KinoMarzenieClient(http, KinoMarzenie, today))
  private val wloclawekScrapers    = Seq(multikino("0008", MultikinoWloclawek))
  private val legnicaScrapers      = Seq(helios(HeliosNuxt.Legnica), bilety24Subdomain(KinoPiast))
  private val plockScrapers        = Seq(helios(HeliosNuxt.Plock), noveKino("przedwiosnie", KinoPrzedwiosnie))
  // BCKino sells theatre/workshops/concerts through the same listing as its
  // films, tagged with a `data-group` per event ("BCKino" for films) —
  // `filmGroups` keeps only those and peels the resulting "BCKino – " title
  // prefix. Verified screening 2026-09-23.
  private val bytomScrapers        = Seq(
    cinemaCity("1092", CinemaCityBytom),
    new SystemBiletowyClient(http, VisualSoftPortal("https://bck.systembiletowy.pl"), KinoBCKBytom, titles = titles, filmGroups = Set(EventCategory("BCKino"))),
  )
  private val dabrowaGorniczaScrapers = Seq(helios(HeliosNuxt.DabrowaGornicza), new VisualTicketClient(http, "https://bilety.palac.art.pl", KinoKadr, locationId = 2))
  private val nowySaczScrapers     = Seq(helios(HeliosNuxt.NowySacz), bilety24("https://www.bilety24.pl/kino/organizator/malopolskie-centrum-kultury-sokol-w-nowym-saczu-1225", KinoSokol))
  private val slupskScrapers       = Seq(multikino("0030", MultikinoSlupsk), ekobilet("kinorejs", KinoRejs))
  private val jeleniaGoraScrapers  = Seq(helios(HeliosNuxt.JeleniaGora), bilety24Subdomain(KinoLot))
  private val przemyslScrapers     = Seq(helios(HeliosNuxt.Przemysl))
  // Konin + its catchment: Helios via the chain client, and each independent off
  // its own ticketing listing (bilety24 organiser / biletyna), Filmweb only where
  // it has none. Neighbouring bilety24 organiser ids are different towns: 1626 is
  // Konin's culture centre, 1621 Koło's MDK. Września's Kino Trójka (1698) is
  // intentionally not wired.
  private val koninScrapers        = Seq(
    helios(HeliosNuxt.Konin),
    bilety24Subdomain(KinoOskard),
    filmweb(2405, KinoZacheta),   // Kleczew
    bilety24("https://www.bilety24.pl/kino/organizator/koninskie-centrum-kultury-1626", KinoStudyjneCentrum),   // Konin
    bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-w-kole-1621", KinoNadWarta),   // Koło
    bilety24("https://www.bilety24.pl/kino/organizator/zajezdnia-kultury-w-pleszewie-1255", KinoHel),   // Pleszew
    bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-w-slupcy-1423", KinoSokolnia),   // Słupca
    biletyna(KinoTur),   // Turek
    biletyna(KinoMok),   // Zagórów
  )

  // ── Regional hubs (2026-09 sweep) ───────────────────────────────────────────
  // Smaller towns grouped around their largest one, every venue about an hour's
  // drive from it — the shape Konin already had for Turek, Koło and Słupca.
  private val piotrkowTrybunalskiScrapers = Seq(
    helios(HeliosNuxt.PiotrkowTrybunalski),   // Piotrków Trybunalski
    helios(HeliosNuxt.Belchatow),   // Bełchatów
    filmweb(1330, KinoKulturaBelchatow),   // Bełchatów
    helios(HeliosNuxt.TomaszowMazowiecki),   // Tomaszów Mazowiecki
  )
  private val siedlceScrapers = Seq(
    helios(HeliosNuxt.Siedlce),   // Siedlce
    filmweb(1400, NoveKinoSiedlce),   // Siedlce
    filmweb(2073, KinoSokolSokolowPodlaski),   // Sokołów Podlaski
    filmweb(1658, KinoSlawa),   // Międzyrzec Podlaski
    filmweb(1850, KinoZaRogiemMiedzyrzec),   // Międzyrzec Podlaski
    filmweb(1742, KinoKongres),   // Węgrów
    filmweb(1808, KinoLukow),   // Łuków
    cinemaCity("1100", CinemaCityBialaPodlaska),   // Biała Podlaska
    filmweb(1677, KinoWilga),   // Garwolin
  )
  private val pilaScrapers = Seq(
    helios(HeliosNuxt.Pila),   // Piła
    bilety24("https://www.bilety24.pl/kino/organizator/waleckie-centrum-kultury-1598", KinoWCKWalcz),   // Wałcz
    bilety24("https://www.bilety24.pl/kino/organizator/kino-osiedlowe-trzcianka-1559", KinoOsiedlowe),   // Trzcianka
    filmweb(2415, KinoRodlo),   // Złotów
    bilety24("https://www.bilety24.pl/kino/organizator/gminny-osrodek-kultury-685", KinoMewaBudzyn),   // Budzyń
    bilety24("https://www.bilety24.pl/kino/organizator/chodzieski-dom-kultury-591", KinoNotec),   // Chodzież
    bilety24("https://www.bilety24.pl/kino/organizator/nck-w-wieleniu-1344", KinoZaRogiemWielen),   // Wieleń
    bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-623", KinoMDKWagrowiec),   // Wągrowiec
    bilety24("https://www.bilety24.pl/kino/organizator/mck-kino-swiatowid-831", KinoSwiatowidCzarnkow),   // Czarnków
  )
  private val ostrowiecSwietokrzyskiScrapers = Seq(
    bilety24("https://www.bilety24.pl/kino/organizator/miejskie-centrum-kultury-w-ostrowcu-swietokrzyskim-1389", KinoEtiuda),   // Ostrowiec Świętokrzyski
    filmweb(3133, KinoOOK),   // Opatów
    filmweb(1601, KinoStarowka),   // Sandomierz
  )
  private val gnieznoScrapers = Seq(
    helios(HeliosNuxt.Gniezno),   // Gniezno
    bilety24("https://www.bilety24.pl/kino/organizator/mogilenski-dom-kultury-kino-wawrzyn-1644", KinoWawrzyn),   // Mogilno
    bilety24("https://www.bilety24.pl/kino/organizator/biblioteka-publiczna-im-czeslawa-chruszczewskiego-1294", KinoMiescisko),   // Mieścisko
  )
  private val suwalkiScrapers = Seq(
    filmweb(1441, KinoCinemaLumiereSuwalki),   // Suwałki
    filmweb(457, KinoIskra),   // Augustów
    bilety24("https://www.bilety24.pl/kino/organizator/dom-kultury-w-goldapi-1089", KinoKulturaGoldap),   // Gołdap
    filmweb(1533, KinoMazur),   // Olecko
    filmweb(1650, KinoECK),   // Ełk
    msi(KinoPlanetCinemaElk),   // Ełk
  )
  private val stalowaWolaScrapers = Seq(
    helios(HeliosNuxt.StalowaWola),   // Stalowa Wola
    bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-stalowa-wola-1419", KinoWrzos),   // Stalowa Wola
    filmweb(2407, KinoSokolNisko),   // Nisko
    filmweb(1903, KinoMetalowiecNowaDeba),   // Nowa Dęba
    filmweb(1489, KinoBCK),   // Biłgoraj
  )
  private val zamoscScrapers = Seq(
    filmweb(1670, KinoStylowy),   // Zamość
    bilety24("https://www.bilety24.pl/kino/organizator/chelmski-dom-kultury-1718", KinoZorzaChelm),   // Chełm
    filmweb(284, KinoMorskieOko),   // Krasnystaw
    filmweb(2330, KinoPlon),   // Hrubieszów
  )
  private val lesznoScrapers = Seq(
    multikino("0044", MultikinoLeszno),   // Leszno
    bilety24("https://www.bilety24.pl/kino/organizator/teatr-miejski-w-lesznie-1653", KinoTeatrMiejskiLeszno),   // Leszno
    filmweb(1656, KinoPromienRawicz),   // Rawicz
    bilety24("https://www.bilety24.pl/kino/organizator/gostynski-osrodek-kultury-hutnik-z-siedziba-w-gostyniu-679", KinoPodKopula),   // Gostyń
    bilety24("https://www.bilety24.pl/kino/organizator/biblioteka-i-centrum-kultury-rekreacji-im-jana-z-domachowa-bzdegi-w-krobi-1643", KinoKrobia),   // Krobia
  )
  private val lomzaScrapers = Seq(
    helios(HeliosNuxt.Lomza),   // Łomża
    bilety24("https://www.bilety24.pl/kino/organizator/ostroleckie-centrum-kultury-1068", KinoJantar),   // Ostrołęka
    filmweb(1644, KinoCKZambrow),   // Zambrów
    filmweb(1743, KinoOstrovia),   // Ostrów Mazowiecka
    filmweb(2348, KinoWars),   // Wysokie Mazowieckie
    filmweb(1532, KinoDomKulturyGrajewo),   // Grajewo
  )
  private val pulawyScrapers = Seq(
    filmweb(311, KinoSybilla),   // Puławy
    filmweb(2089, KinoRenesans),   // Ryki
  )
  private val skierniewiceScrapers = Seq(
    biletyna(KinoPolonez),   // Skierniewice
    biletyna(KinoCKiSSkierniewice),   // Skierniewice
    biletyna(KinoLen),   // Żyrardów
    filmweb(2332, KinoRomaRawa),   // Rawa Mazowiecka
    filmweb(1519, KinoFenix),   // Łowicz
  )
  private val starogardGdanskiScrapers = Seq(
    cinemaCity("1095", CinemaCityStarogard),   // Starogard Gdański
    filmweb(2317, KinoRemus),   // Kościerzyna
  )
  private val ciechanowScrapers = Seq(
    bilety24("https://www.bilety24.pl/kino/organizator/powiatowe-centrum-kultury-i-sztuki-im-marii-konopnickiej-1307", KinoLydynia),   // Ciechanów
    filmweb(1921, KinoKosmosMlawa),   // Mława
    bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-im-stanislawa-ostoi-kotkowskiego-w-przasnyszu-1482", KinoSwiatowidPrzasnysz),   // Przasnysz
    bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-w-makowie-mazowieckim-1485", KinoMDKMakow),   // Maków Mazowiecki
    biletyna(KinoApolloDzialdowo),   // Działdowo
  )
  private val wielunScrapers = Seq(
    filmweb(2034, KinoSyrena),   // Wieluń
    filmweb(2333, KinoTeatrSieradz),   // Sieradz
    filmweb(2334, KinoSlonce),   // Wieruszów
    filmweb(2358, KinoSokolniaKepno),   // Kępno
  )
  private val chojniceScrapers = Seq(
    filmweb(2307, KinoChDK),   // Chojnice
    filmweb(2349, KinoUciechaCzluchow),   // Człuchów
    bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-i-biblioteki-im-jana-karnowskiego-1410", KinoZaRogiemBrusy),   // Brusy
  )
  private val zgorzelecScrapers = Seq(
    multikino("0031", MultikinoZgorzelec),   // Zgorzelec
    bilety24("https://www.bilety24.pl/kino/organizator/bogatynski-osrodek-kultury-1659", KinoKadrBogatynia),   // Bogatynia
  )
  private val ilawaScrapers = Seq(
    filmweb(1857, KinoteatrPasja),   // Iława
    filmweb(1805, KinoPokoj),   // Lubawa
    bilety24("https://www.bilety24.pl/kino/organizator/miejskie-centrum-kultury-1291", KinoteatrHarmonia),   // Nowe Miasto Lubawskie
  )
  private val ketrzynScrapers = Seq(
    filmweb(1686, KinoGwiazdaKetrzyn),   // Kętrzyn
    filmweb(1701, KinoZodiak),   // Mrągowo
  )
  private val zakopaneScrapers = Seq(
    filmweb(1273, KinoSokolZakopane),   // Zakopane
    biletyna(KinoGiewont),   // Zakopane
    filmweb(2336, KinoMiejsce),   // Zakopane
    bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-sportu-i-promocji-w-rabce-zdroju-625", KinoSniezkaRabka),   // Rabka-Zdrój
    msi(KinoStopiak),   // Nowy Targ — own site links bilety.stopiakcinema.pl, a standard MSI portal
  )
  private val wyszkowScrapers = Seq(
    filmweb(2304, KinoDobrychFilmow),   // Wyszków
    filmweb(2337, KinoMiGOK),   // Łochów
  )
  // Two towns with one cinema each and nothing else within an hour's drive —
  // pages of their own rather than a stretch onto the nearest region.
  private val slubiceScrapers = Seq(bilety24("https://www.bilety24.pl/kino/organizator/slubicki-miejski-osrodek-kultury-1336", KinoSmok))
  private val wlodawaScrapers = Seq(filmweb(2312, KinoWDK))
  private val zlocieniecScrapers = Seq(
    bilety24("https://www.bilety24.pl/kino/organizator/zlocieniecki-osrodek-kultury-1710", KinoMewaZlocieniec),   // Złocieniec
    bilety24("https://www.bilety24.pl/kino/organizator/osrodek-kultury-w-drawsku-pomorskim-1680", KinoOKDrawsko),   // Drawsko Pomorskie
  )

  /** Raw scrapers grouped by city slug — same slugs `City.slug` uses, so a
   *  caller can scope by city without re-spelling the membership. */
  // Catchment cinemas (nearby towns) and a few in-city venues that came in via a
  // Filmweb sweep, each by Filmweb internal cinema id — except the handful that
  // have since moved to their own site (Kino Spójnia, Kino Praha), which fed
  // Filmweb too thinly. Merged into byCity below so every city's catchment is
  // scraped without touching its hand-written scraper group.
  private val filmwebExtra: Map[String, Seq[CinemaScraper]] = Map(
    "wroclaw" -> Seq(new Bilety24SubdomainClient(http, "https://kulturalne-oborniki.bilety24.pl/repertuar/", KinoAstra, today = today, titles = titles), filmweb(1645, KinoDyskusyjnyKlubFilmowyPolitechnika)),
    "warszawa" -> Seq(new PrahaClient(http, KinoMazowieckiTeatrMuzycznyImJanaKiepuryKinoPraha, today)),
    "lodz" -> Seq(new KinoSpojniaClient(http, KinoSpojnia), bilety24("https://www.bilety24.pl/kino/organizator/miejski-osrodek-kultury-stary-mlyn-w-zgierzu-1697", KinoStaryMlyn)),
    "katowice" -> Seq(cinemaCity("1062", CinemaCity), new KinoPatriaClient(http, KinoPatria, today)),
    "szczecin" -> Seq(systemBiletowy(KinoKawiarnia), biletyna(KinoPDK), new SckStargardClient(http, KinoSCK)),
    "bialystok" -> Seq(new KinoSokolSokolkaClient(http, KinoSokolSokolka)),
    "trojmiasto" -> Seq(biletyna(KinoNaSzekspirowskim), multikino("0027", MultikinoRumia)),
    "bydgoszcz" -> Seq(msi(KinoKinomax), biletyna(KinoRondo)),
    "lublin" -> Seq(bilety24("https://www.bilety24.pl/kino/organizator/lubartowski-osrodek-kultury-1382", KinoLewart), bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-i-promocji-w-krasniku-1529", KinoMetalowiec)),
    "czestochowa" -> Seq(new KinoDKFRumcajsClient(http, KinoDKFRumcajs, today = today), new KinoKarolinkaClient(http, KinoKarolinka), bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-w-radomsku-1546", KinoMDK), bilety24("https://www.bilety24.pl/kino/organizator/miejski-osrodek-kultury-centrum-im-adama-mickiewicza-w-zawierciu-1305", KinoMOKCentrum), new KinoZaciszeClient(http, KinoZacisze)),
    "radom" -> Seq(helios(HeliosNuxt.Starachowice), msi(KinoCentrumSkarzyskoKamienna), bilety24("https://www.bilety24.pl/kino/organizator/szydlowieckie-centrum-kultury-zamek-1320", KinoGornik), msi(KinoKozienickiDomKultury), systemBiletowy(KinoKuznica), msi(KinoSwitZwolen)),
    "torun" -> Seq(biletyna(KinoMiejskieCentrumKultury), biletyna(KinoZdroj)),
    "kielce" -> Seq(bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-w-jedrzejowie-1458", KinoCK), koneckieCentrumKultury),
    "rzeszow" -> Seq(helios(HeliosNuxt.Krosno), new ArtKinoKrosnoClient(http, KinoArtKino, today), new KinoJednoscClient(http, KinoJednosc), msi(KinoMCK), msi(KinoSniezka), new KinoSokolBrzozowClient(http, KinoSokolBrzozow), msi(KinoWarszawa)),
    "gliwice" -> Seq(new KinoScenaKulturaClient(http, KinoScenaKultura)),
    "olsztyn" -> Seq(msi(KinoCinemaLumiere), msi(KinoIgnacy), bilety24("https://www.bilety24.pl/kino/organizator/moraski-dom-kultury-1682", KinoNarie)),
    "bielsko-biala" -> Seq(bilety24("https://www.bilety24.pl/kino/organizator/kino-janosik-1500", KinoJanosik), systemBiletowy(KinoPckulKino), biletyna(KinoSwitCzechowiceDziedzice), biletyna(KinoTeatrElektryczny), bilety24("https://www.bilety24.pl/kino/organizator/osrodek-kultury-w-brzeszczach-1539", KinoWislaBrzeszcze), multikino("0033", MultikinoCzechowiceDziedzice)),
    "opole" -> Seq(helios(HeliosNuxt.KedzierzynKozle), bilety24("https://www.bilety24.pl/kino/organizator/centrum-aktywnosci-lokalnej-w-kluczborku-kino-bajka-1467", KinoBajkaKluczbork), msi(KinoChemik), new KinoDianaClient(http, KinoDiana), new KdkKrapkowiceClient(http, KinoKrapkowice), new KinoStudioClient(http, KinoStudio, today), msi(KinoTwierdza)),
    "rybnik" -> Seq(helios(HeliosNuxt.Zory), bilety24("https://www.bilety24.pl/kino/organizator/kino-baltyk-1499", KinoBaltyk), ekobilet("kino-centrum-jastrzebiezdrj", KinoCentrum), systemBiletowy(KinoNaStarowce), biletyna(KinoPegaz), new TeatrZiemiRybnickiejClient(http)),
    "elblag" -> Seq(helios(HeliosNuxt.Tczew), filmweb(2352, KinoBaszta), msi(KinoPowisle), biletyna(KinoZulawskiOsrodekKultury), new KinoSwiatowidElblagClient(http, KinoSwiatowidElblag, today)),
    "koszalin" -> Seq(msi(KinoBajkaDarlowo), bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-i-spotkan-europejskich-w-bialogardzie-1685", KinoCentrumBialogard), biletyna(KinoDK), msi(KinoGOK), msi(KinoGoplana), msi(KinoWybrzeze)),
    "kalisz" -> Seq(helios(HeliosNuxt.OstrowWielkopolski), systemBiletowy(KinoCentrum3D), bilety24("https://www.bilety24.pl/kino/organizator/kino-echo-1159", KinoEcho), bilety24("https://www.bilety24.pl/kino/organizator/ostrzeszowskie-centrum-kultury-601", KinoPiastOstrzeszow), bilety24("https://www.bilety24.pl/kino/organizator/krotoszynski-osrodek-kultury-1668", KinoPrzedwiosnieKrotoszyn)),
    "zielona-gora" -> Seq(bilety24("https://www.bilety24.pl/kino/organizator/nowosolski-dom-kultury-1679", KinoEuropa), msi(KinoMaxKino), bilety24("https://www.bilety24.pl/kino/organizator/kino-pionier-1492", KinoPionierZary), msi(KinoSDKSwiebodzin)),
    "tychy" -> Seq(systemBiletowy(KinoNaszeKino), msi(KinoPlanetCinema)),
    "walbrzych" -> Seq(msi(KinoMOKNowaRuda), ekobilet("mokis-bielawa", KinoMOKiS), new KinoSlezaClient(http, KinoSleza), new KinoZbyszekClient(http, KinoZbyszek), multikino("0041", MultikinoKlodzko), multikino("0043", MultikinoSwidnica)),
    "tarnow" -> Seq(systemBiletowy(KinoFarys), biletyna(KinoGCK), bilety24("https://www.bilety24.pl/kino/organizator/gorlickie-centrum-kultury-1581", KinoKolory), msi(KinoPlaneta), new KinoPromienClient(http, KinoPromien, today), systemBiletowy(KinoRegis), bilety24("https://www.bilety24.pl/kino/organizator/dabrowski-dom-kultury-1303", KinoSokolDabrowaTarnowska)),
    "wloclawek" -> Seq(msi(KinoJutrzenka), biletyna(KinoNawojka), msi(KinoNoweKinoWarszawa)),
    "legnica" -> Seq(helios(HeliosNuxt.Lubin), new KinoAurumClient(http, KinoAurum), new CyfroweKinoClient(http, KinoCyfroweKino, titles = titles), bilety24("https://www.bilety24.pl/kino/organizator/boleslawiecki-osrodek-kultury-miedzynarodowe-centrum-ceramiki-w-boleslawcu-1586", KinoForumBoleslawiec), bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-muza-w-lubinie-dolny-slask-1375", KinoMuzaLubin), bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-w-polkowicach-1689", KinoPCA)),
    "plock" -> Seq(msi(KinoZaRogiem), bilety24("https://www.bilety24.pl/kino/organizator/kutnowski-dom-kultury-1474", KinoKDK), systemBiletowy(KinoKalejdoskop), msi(KinoODEON)),
    "nowy-sacz" -> Seq(ekobilet("kino-jaworzyna", KinoJaworzyna), bilety24("https://www.bilety24.pl/kino/organizator/limanowski-dom-kultury-1368", KinoKlaps)),
    "slupsk" -> Seq(bilety24("https://www.bilety24.pl/kino/organizator/leborskie-centrum-kultury-fregata-1683", KinoFregata)),
    "jelenia-gora" -> Seq(bilety24("https://www.bilety24.pl/kino/organizator/kino-wawel-1489", KinoWawel)),
    "przemysl" -> Seq(new KinoCentrum3DPrzemyslClient(http, KinoCentrum3DPrzemysl), msi(KinoIkar), msi(KinoNaBiegunach), new KinoSDKSanokClient(http, KinoSDK, today)),
  )

  // ── United Kingdom (Flicks) ──────────────────────────────────────────────
  // Through `flicksFetch`, NOT `http`: Cloudflare 403s our Fly egress IP on
  // flicks.co.uk, and Flicks is the only UK source, so a direct fetch takes out
  // every UK venue at once (it did, 2026-07-26).
  private def flicks(slug: String, cinema: Cinema): FlicksClient =
    flicksIn(FlicksMarket.UnitedKingdom, slug, cinema)

  // The US runs on the same Flicks platform as the UK, so it reuses the same
  // client and the same residential egress — only the market (and so the host)
  // differs. It is a SEPARATE hostname, which is what keeps the two markets'
  // pace gates and 429 back-offs independent; see `FlicksMarket`.
  private def flicksUs(slug: String, cinema: Cinema): FlicksClient =
    flicksIn(FlicksMarket.UnitedStates, slug, cinema)

  private def flicksIn(market: FlicksMarket, slug: String, cinema: Cinema): FlicksClient =
    new FlicksClient(flicksFetch, slug, cinema, market, today = Some(today))

  // UK chain own-site clients — the catalogue PRIMARY for their venues, with
  // flicks.co.uk kept as the aggregator fallback (see [[ChainFlicksFallback]] +
  // `WorkerWiring.recordingScraper`). Cineworld + Vue are Cloudflare-403'd from our
  // Fly datacenter IP (verified in prod 2026-07-27), so they egress residential:
  // Cineworld via `flicksFetch` (GET-only, per-venue sticky is fine), Vue via
  // `vueFetch` (host-sticky, for its token cookie), Odeon via `odeonFetch`.
  // Showcase/Everyman still reach the origin directly, so they use `http`. Any
  // proxy failure rolls to the flicks fallback.
  private def cineworld(slug: String, cinema: Cinema): CineworldClient =
    new CineworldClient(flicksFetch, slug, cinema, today = today)
  private def vueUk(id: String, cinema: Cinema): VueCinemasPlatformClient =
    new VueCinemasPlatformClient(vueFetch, VueCinemasPlatformClient.MyVueBaseUrl, id, cinema)
  private def showcase(id: String, cinema: Cinema): GatsbyBoxOfficeClient =
    new GatsbyBoxOfficeClient(http, GatsbyBoxOfficeClient.ShowcaseBaseUrl, id, cinema)
  private def everyman(id: String, cinema: Cinema): GatsbyBoxOfficeClient =
    new GatsbyBoxOfficeClient(http, GatsbyBoxOfficeClient.EverymanBaseUrl, id, cinema)
  // Odeon pulls Vista `ocapi` over the injected JWT (harvested via Zyte); only the
  // token harvest needs a browser, the data fetch needs only the bearer. The ocapi
  // host DOES sit behind Cloudflare though — it answered our Fly IP but 403s the
  // Hetzner egress the worker moved to on 2026-08-29 — so the data fetch goes over
  // the residential `odeonFetch`, not direct `http`. A missing token, or a proxy
  // that can't get through either, throws → flicks fallback.
  private def odeon(id: String, cinema: Cinema): OdeonClient =
    new OdeonClient(odeonFetch, id, cinema, odeonAuthToken, today = today)
  private val londonScrapers: Seq[CinemaScraper] = Seq(
    flicks("act-one-acton", ActOneActon),
    flicks("arthouse-crouch-end", ArthouseCrouchEnd),
    flicks("barbican-london-cinema-1", BarbicanLondonCinema1),
    flicks("bfi-london-imax", BfiLondonImax),
    flicks("bfi-london-southbank", BfiLondonSouthbank),
    flicks("castle-cinema-hackney", CastleCinemaHackney),
    flicks("sidcup-storyteller", SidcupStoryteller),
    flicks("chiswick-cinema", ChiswickCinema),
    cineworld("x087g-cineworld-cinema-london-the-o2-greenwich", CineworldGreenwich),
    cineworld("x06x0-cineworld-cinema-london-bexleyheath", CineworldBexleyheath),
    cineworld("x078z-cineworld-cinema-london-enfield", CineworldEnfield),
    cineworld("x06xb-cineworld-cinema-london-feltham", CineworldFeltham),
    cineworld("x06xd-cineworld-cinema-london-ilford", CineworldIlford),
    cineworld("x06v1-cineworld-cinema-london-leicester-square", CineworldLeicesterSquare),
    cineworld("g01a4-cineworld-cinema-london-hounslow", CineworldLondonHounslow),
    cineworld("x0y86-cineworld-cinema-london-south-ruislip", CineworldSouthRuislip),
    cineworld("x076w-cineworld-cinema-london-wandsworth", CineworldWandsworth),
    cineworld("x0sra-cineworld-cinema-london-wembley", CineworldWembley),
    cineworld("x0795-cineworld-cinema-london-west-india-quay", CineworldWestIndiaQuay),
    cineworld("x06xs-cineworld-cinema-london-wood-green", CineworldWoodGreen),
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
    everyman("G05D7", EverymanAtTheWhiteleyLondon),
    everyman("G049A", EverymanBrentford),
    everyman("X0712", EverymanCinemaBakerStreet),
    everyman("X06SI", EverymanCinemaBarnet),
    everyman("X077P", EverymanCinemaBelsizeParkHampstead),
    everyman("G011I", EverymanCinemaBoroughYards),
    everyman("X11NT", EverymanCinemaBroadgate),
    everyman("X0VPB", EverymanCinemaCanaryWharf),
    everyman("X078X", EverymanCinemaChelsea),
    everyman("X11DR", EverymanCinemaCrystalPalace),
    everyman("G01HI", EverymanCinemaEgham),
    everyman("X06UF", EverymanCinemaEsher),
    everyman("X06ZW", EverymanCinemaHampstead),
    everyman("X0X5P", EverymanCinemaKingSCross),
    everyman("X0LWI", EverymanCinemaMaidaVale),
    everyman("X06SN", EverymanCinemaMuswellHill),
    everyman("G029X", EverymanCinemaStratfordInternational),
    everyman("X0710", EverymanCinemaWaltonOnThames),
    everyman("X077O", EverymanCinemaIslington),
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
    odeon("995", OdeonCinemaActon),
    odeon("695", OdeonCinemaBeckenham),
    odeon("570", OdeonCinemaEpsom),
    odeon("963", OdeonCinemaGreenwich),
    odeon("125", OdeonCinemaHolloway),
    odeon("536", OdeonCinemaKingston),
    odeon("852", OdeonCinemaOrpington),
    odeon("888", OdeonCinemaRichmond),
    odeon("090", OdeonCinemaSouthWoodford),
    odeon("694", OdeonCinemaStreatham),
    odeon("200", OdeonCinemaTottenhamCourtRoad),
    odeon("593", OdeonCinemaUxbridge),
    odeon("555", OdeonCinemaWimbledon),
    odeon("158", OdeonCinemaLuxeHaymarket),
    odeon("858", OdeonLuxeIslington),
    odeon("949", OdeonLuxeLeeValley),
    odeon("153", OdeonCinemaLuxeLeicesterSquare),
    odeon("486", OdeonCinemaLuxePutney),
    odeon("838", OdeonLuxeSwissCottage),
    odeon("155", OdeonLuxeWestEnd),
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
    vueUk("10093", VueCinemasBromley),
    vueUk("10044", VueCinemasDagenham),
    vueUk("10096", VueCinemasEltham),
    vueUk("10031", VueCinemasFinchleyRoadSwissCottage),
    vueUk("10046", VueCinemasFulham),
    vueUk("10006", VueCinemasHarrow),
    vueUk("10032", VueCinemasIslington),
    vueUk("10022", VueCinemasFinchley),
    vueUk("10080", VueCinemasPiccadillyCircus),
    vueUk("10025", VueCinemasPurleyWayCroydon),
    vueUk("10013", VueCinemasRomford),
    vueUk("10024", VueCinemasStainesUponThames),
    vueUk("10074", VueCinemasStratford),
    vueUk("10030", VueCinemasWestEnd),
    vueUk("10072", VueCinemasWestfieldShepherdSBush),
    vueUk("10071", VueCinemasWoodGreen),
    flicks("watermans-art-centre-brentford", WatermansArtCentreBrentford),
    flicks("wyllyotts-theatre-potters-bar", WyllyottsTheatrePottersBar),
  )
  private val manchesterScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x06wz-cineworld-cinema-ashton-under-lyne", CineworldAshtonUnderLyne),
    cineworld("x079r-cineworld-cinema-didsbury", CineworldManchester),
    flicks("cultplex-manchester", CultplexManchester),
    everyman("X11NP", EverymanManchesterStJohns),
    flicks("flix-treehouse-manchester", FlixTreehouseManchester),
    flicks("home-manchester", HomeManchester),
    flicks("leigh-film-factory", LeighFilmFactory),
    flicks("northern-light-sale", NorthernLightSale),
    odeon("856", OdeonCinemaManchesterGreatNorthern),
    odeon("955", OdeonCinemaManchesterTraffordCentre),
    odeon("851", OdeonCinemaOldham),
    flicks("empire-cinema-wigan", EmpireCinemaWigan),
    flicks("plaza-stockport", PlazaStockport),
    flicks("reel-cinema-rochdale", ReelCinemaRochdale),
    flicks("regent-marple", RegentMarple),
    flicks("savoy-heaton-moor", SavoyHeatonMoor),
    flicks("the-light-cinemas-stockport", TheLightCinemasStockport),
    vueUk("10091", VueCinemasManchesterPrintworks),
    vueUk("10057", VueCinemasManchesterQuayside),
  )
  private val norwichScrapers: Seq[CinemaScraper] = Seq(
    flicks("arc-cinema-at-the-royalty-great-yarmouth", ArcCinemaGreatYarmouth),
    flicks("central-fakenham", CentralCinemaFakenham),
    flicks("corn-exchange-cinema-king-s-lynn", CornExchangeCinemaKingSLynn),
    flicks("east-coast-cinema-lowestoft", EastCoastCinemaLowestoft),
    flicks("little-theatre-sheringham", LittleTheatreSheringham),
    flicks("majestic-king-s-lynn", MajesticKingSLynn),
    flicks("marina-theatre-lowestoft", MarinaTheatreLowestoft),
    odeon("957", OdeonNorwich),
    flicks("orion-dereham", OrionDereham),
    flicks("palace-cinema-gorleston-on-sea", PalaceCinemaGorlestonOnSea),
    flicks("cinema-city-picturehouse-norwich", CinemaCityPicturehouseNorwich),
    flicks("regal-movieplex-cromer", RegalMovieplexCromer),
    flicks("the-light-cinemas-thetford", TheLightThetford),
    vueUk("10014", VueCinemasNorwich),
  )
  private val aberdeenshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("arc-peterhead", ArcCinemaPeterhead),
    flicks("belmont-filmhouse-aberdeen", BelmontFilmhouse),
    cineworld("x079z-cineworld-cinema-aberdeen-queens-links", CineworldQueensLinkAberdeen),
    cineworld("x0fr5-cineworld-cinema-aberdeen-union-square", CineworldUnionSquareAberdeen),
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
    odeon("019", OdeonCinemaKilmarnock),
  )
  private val bedfordshireScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x06xf-cineworld-cinema-luton", CineworldLuton),
    vueUk("10092", VueCinemasBedford),
  )
  private val belfastScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x0787-cineworld-cinema-belfast", CineworldBelfast),
    flicks("movie-house-city-side-belfast", MovieHouseCitySideBelfast),
    odeon("600", OdeonCinemaBelfast),
    flicks("omniplex-belfast", OmniplexBelfast),
    flicks("omniplex-lisburn", OmniplexLisburn),
    flicks("queen-s-film-theatre-belfast", QueenSFilmTheatreBelfast),
    flicks("strand-arts-centre-belfast", StrandArtsCentreBelfast),
    flicks("the-avenue-cinema-belfast", TheAvenueCinemaBelfast),
  )
  private val berkshireScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x104v-cineworld-cinema-bracknell", CineworldBracknell),
    flicks("screen-one-newbury", CornExchangeNewburyScreenOne),
    everyman("X11NV", EverymanCinemaWokingham),
    odeon("962", OdeonLuxeMaidenhead),
    flicks("reading-biscuit-factory", ReadingBiscuitFactory),
    showcase("X06KD", ShowcaseDeLuxReading),
    flicks("south-hill-park-arts-centre-bracknell", SouthHillParkArtsCentreBracknell),
    flicks("the-assembly-at-heckfield-place", TheAssemblyAtHeckfieldPlace),
    // The Old Court is NOT on Flicks — it is absent from the whole
    // `sitemap-cinemas.xml`, and `the-screen-cinema-windsor` (the only Windsor
    // slug Flicks has) is a different venue, so this venue was reading someone
    // else's page and scraping to zero. Its own site carries the programme.
    new TheOldCourtClient(http, TheOldCourtWindsor, today),
    vueUk("10070", VueCinemasNewbury),
    vueUk("10020", VueCinemasReading),
  )
  private val birminghamScrapers: Seq[CinemaScraper] = Seq(
    flicks("artrix-bromsgrove", ArtrixBromsgrove),
    cineworld("x079j-cineworld-cinema-birmingham-broad-street", CineworldBroadStreetBirmingham),
    cineworld("x0vho-cineworld-cinema-birmingham-nec", CineworldNECBirmingham),
    cineworld("x06xl-cineworld-cinema-solihull", CineworldSolihull),
    everyman("X0VHE", EverymanCinemaBirmingham),
    flicks("midlands-arts-centre-birmingham", MidlandsArtsCentreBirmingham),
    flicks("mockingbird-cinema-kitchen-birmingham", MockingbirdCinemaKitchenBirmingham),
    odeon("017", OdeonBirminghamNewStreet),
    odeon("846", OdeonLuxeBirminghamBroadwayPlaza),
    flicks("empire-cinema-birmingham", OmniplexBirmingham),
    flicks("reel-cinema-quinton", ReelCinemaQuinton),
    flicks("royal-cinema-sutton-coldfield", RoyalCinemasSuttonColdfield),
    vueUk("10015", VueCinemasBirmingham),
  )
  private val bristolScrapers: Seq[CinemaScraper] = Seq(
    flicks("cube-bristol", CubeCinemaBristol),
    everyman("X0X3Q", EverymanCinemaBristol),
    odeon("315", OdeonCabotCircus),
    flicks("orpheus-bristol", ScottCinemasBristolWestburyPark),
    showcase("X06JH", ShowcaseBristolAvonmeads),
    vueUk("10018", VueCinemasBristolCribbsCauseway),
    vueUk("10019", VueCinemasBristolLongwellGreen),
    flicks("watershed-bristol", WatershedBristol),
  )
  private val buckinghamshireScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x08c8-cineworld-cinema-high-wycombe", CineworldHighWycombe),
    cineworld("x06xg-cineworld-cinema-milton-keynes", CineworldMiltonKeynes),
    everyman("X06SK", EverymanCinemaGerrardsCross),
    everyman("G01RL", EverymanCinemaMarlow),
    odeon("845", OdeonCinemaAylesbury),
    odeon("849", OdeonCinemaMiltonKeynes),
    flicks("empire-cinema-high-wycombe", OmniplexHighWycombeFormerlyEmpire),
    flicks("village-picture-house-cuddington", VillagePictureHouseCuddington),
  )
  private val cambridgeshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("arts-cinema-john-clare-theatre-peterborough", ArtsCinemaJohnClareTheatrePeterborough),
    flicks("arts-picturehouse-cambridge", ArtsPicturehouseCambridge),
    cineworld("x0y4c-cineworld-cinema-ely", CineworldEly),
    cineworld("x06xc-cineworld-cinema-huntingdon", CineworldHuntingdon),
    cineworld("x0tvc-cineworld-cinema-st-neots", CineworldStNeots),
    flicks("ely-community-cinema", ElyCommunityCinema),
    everyman("G02AM", EverymanCinemaCambridge),
    flicks("key-theatre-peterborough", KeyTheatrePeterborough),
    flicks("luxe-wisbech", LuxeWisbech),
    odeon("998", OdeonLuxePeterborough),
    showcase("X06JO", ShowcaseDeLuxPeterborough),
    flicks("the-light-cinemas-cambridge", TheLightCambridge),
    flicks("the-light-cinemas-wisbech", TheLightWisbech),
  )
  private val cardiffScrapers: Seq[CinemaScraper] = Seq(
    flicks("chapter-cardiff", ChapterCardiff),
    cineworld("x078m-cineworld-cinema-cardiff", CineworldCardiff),
    everyman("X11NU", EverymanCinemaCardiff),
    odeon("954", OdeonCinemaCardiff),
    showcase("X06JS", ShowcaseCinemaCardiff),
  )
  private val centralScotlandScrapers: Seq[CinemaScraper] = Seq(
    flicks("chalmers-alloa-cinema", ChalmersAlloaCinema),
    cineworld("x06xa-cineworld-cinema-falkirk", CineworldFalkirk),
    flicks("hippodrome-bo-ness", HippodromeBoNess),
    flicks("macrobert-art-centre-stirling", MacrobertArtCentreStirling),
    vueUk("10064", VueCinemasStirling),
  )
  private val cheshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("buxton-cinema", BuxtonCinemaPavilionArtsCentre),
    flicks("cinemac-macclesfield", CinemacMacclesfield),
    cineworld("x12iv-cineworld-cinema-warrington", CineworldWarrington),
    flicks("curzon-cinema-knutsford", CurzonCinemaKnutsford),
    everyman("X11DP", EverymanCinemaAltrincham),
    odeon("756", OdeonCinemaCrewe),
    odeon("853", OdeonCinemaNorthwichBaronsQuay),
    odeon("912", OdeonLuxeWarrington),
    flicks("picturehouse-chester", PicturehouseChester),
    flicks("reel-cinema-widnes", ReelCinemaWidnes),
    flicks("rex-wilmslow", RexWilmslow),
    flicks("storyhouse-chester", StoryhouseChester),
    vueUk("10077", VueCinemasAltrincham),
    vueUk("10034", VueCinemasCheshireOaks),
  )
  private val clwydScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x06xe-cineworld-cinema-llandudno", CineworldLlandudno),
    flicks("scala-prestatyn", MerlinScalaPrestatyn),
    flicks("vue-cinemas-rhyl", StrandCinemaRhyl),
    flicks("theatr-colwyn-colwyn-bay", TheatrColwyn),
  )
  private val cornwallScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x12iu-cineworld-cinema-plymouth", CineworldPlymouth),
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
    vueUk("10027", VueCinemasPlymouth),
    flicks("lighthouse-newquay", WTWLighthouseNewquay),
    flicks("plaza-truro", WTWPlazaTruro),
    flicks("regal-wadebridge", WTWRegalWadebridge),
    flicks("white-river-st-austell", WTWWhiteRiverCinema),
  )
  private val countyDurhamScrapers: Seq[CinemaScraper] = Seq(
    flicks("arc-stockton-on-tees", ARCStocktonOnTees),
    cineworld("x0x9f-cineworld-cinema-dalton-park", CineworldDaltonParkMurtonCounty),
    flicks("empire-cinema-consett-county", EmpireTheatreConsett),
    everyman("G01IW", EverymanCinemaDurham),
    flicks("fuse-community-cinema-prudhoe", FuseCommunityCinemaPrudhoe),
    flicks("gala-theatre-and-cinema-durham-county", GalaCinemaDurham),
    odeon("859", OdeonLuxeDurham),
    showcase("X06JP", ShowcaseCinemaDeLuxTeesside),
    vueUk("10090", VueCinemasDarlington),
    vueUk("10035", VueCinemasHartlepool),
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
    vueUk("10078", VueCinemasBarrow),
    vueUk("10056", VueCinemasCarlisle),
    flicks("zeffirellis-by-the-park-ambleside", ZeffirellisCinemaAmbleside),
  )
  private val derbyshireScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x06x8-cineworld-cinema-chesterfield", CineworldChesterfield),
    flicks("elite-cinema-and-theatre-ashbourne", EliteCinemaAndTheatreAshbourne),
    flicks("northern-light-wirksworth", NorthernLightWirksworth),
    odeon("759", OdeonCinemaSwadlincote),
    odeon("917", OdeonLuxeDerby),
    flicks("quad-derby", QuadDerby),
    flicks("ritz-belper", RitzBelper),
    showcase("X08EM", ShowcaseCinemaDeLuxDerby),
  )
  private val devonScrapers: Seq[CinemaScraper] = Seq(
    flicks("alexandra-newton-abbot", AlexandraNewtonAbbot),
    flicks("dartington-art-centre-totnes", BarnCinemaDartingtonArtCentre),
    flicks("central-barnstaple", CentralCinemaBarnstaple),
    flicks("embassy-ilfracombe", EmbassyCinemaIlfracombe),
    everyman("G01RJ", EverymanCinemaPlymouth),
    flicks("kings-kingsbridge", KingsCinemaKingsbridge),
    flicks("lynton-cinema-lynton-lynmouth", LyntonCinema),
    flicks("new-carlton-okehampton", NewCarltonOkehampton),
    flicks("new-central-cinema-torquay", NewCentralCinemaTorquay),
    odeon("086", OdeonCinemaExeter),
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
    vueUk("10059", VueCinemasExeter),
    vueUk("10084", VueCinemasTorbayPaignton),
  )
  private val dorsetScrapers: Seq[CinemaScraper] = Seq(
    flicks("colosseum-bournemouth", ColosseumBournemouth),
    flicks("electric-palace-bridport", ElectricPalaceBridport),
    flicks("hilltop-cinema-shaftesbury-arts-centre", HilltopCinemaShaftesburyArtsCentre),
    flicks("lighthouse-poole", LighthousePoole),
    flicks("mowlem-swanage", MowlemTheatre),
    odeon("854", OdeonCinemaBournemouthBH2),
    odeon("985", OdeonCinemaDorchester),
    flicks("plaza-cinema-dorchester", PlazaCinemaDorchester),
    flicks("regent-christchurch", RegentChristchurch),
    flicks("the-new-vic-tisbury-village-hall", TheNewVicTisburyVillageHall),
    flicks("rex-wareham", TheRexCinemaWareham),
    flicks("tivoli-wimborne-minster", TivoliTheatreWimborne),
    vueUk("20000", VueCinemasPoole),
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
    odeon("915", OdeonCinemaDudley),
    showcase("X06JJ", ShowcaseCinemaDudley),
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
    odeon("760", OdeonCinemaLlanelli),
    flicks("palace-haverfordwest", PalaceCinemaHaverfordwest),
    flicks("public-hall-brynamman", PublicHallBrynamman),
    flicks("theatr-gwaun-fishguard", TheatrGwaunFishguard),
    flicks("theatr-mwldan-cardigan", TheatrMwldanCardigan),
    flicks("torch-theatre-milford-haven", TorchTheatreMilfordHaven),
    vueUk("10075", VueCinemasCarmarthen),
  )
  private val eastSussexScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x079b-cineworld-cinema-brighton", CineworldBrighton),
    cineworld("x129j-cineworld-cinema-eastbourne-at-the-beacon", CineworldEastbourne),
    flicks("depot-lewes", DepotLewes),
    flicks("duke-of-york-s-picturehouse", DukeOfYorkSPicturehouseBrighton),
    flicks("duke-s-at-komedia-picturehouse", DukeSAtKomediaPicturehouse),
    flicks("electric-palace-hastings", ElectricPalaceHastings),
    flicks("kino-rye", KinoRye),
    flicks("kino-teatr-st-leonards-on-sea", KinoTeatr),
    odeon("824", OdeonCinemaBrighton),
    odeon("124", OdeonCinemaHastings),
    flicks("pavilion-hailsham", PavilionHailsham),
    flicks("picturehouse-uckfield", PictureHouseUckfield),
    flicks("towner-eastbourne-cinema", TownerEastbourneCinema),
  )
  private val eastYorkshireScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x079w-cineworld-cinema-hull", CineworldHull),
    flicks("forum-bridlington", ForumBridlington),
    odeon("440", OdeonLuxeHull),
    flicks("palace-cinema-malton", PalaceCinemaMalton),
    flicks("parkway-beverley", ParkwayBeverley),
    flicks("reel-cinema-hull", ReelCinemaHull),
    vueUk("10065", VueCinemasHull),
  )
  private val edinburghAndLothiansScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x07a1-cineworld-cinema-edinburgh", CineworldEdinburgh),
    flicks("dominion-edinburgh", DominionCinemaEdinburgh),
    everyman("G018L", EverymanCinemaEdinburgh),
    flicks("filmhouse-edinburgh", FilmhouseEdinburgh),
    odeon("850", OdeonEdinburghFortKinnaird),
    odeon("750", OdeonEdinburghLothianRoad),
    odeon("841", OdeonLuxeEdinburghEdinburghWest),
    flicks("scotsman-picturehouse-edinburgh", ScotsmanPicturehouseEdinburgh),
    flicks("the-cameo-picturehouse", TheCameoPicturehouse),
    flicks("the-fraser-centre-tranent", TheFraserCentreTranent),
    vueUk("10001", VueCinemasLivingston),
    vueUk("10010", VueEdinburghOceanTerminal),
    vueUk("10038", VueEdinburghOmniCentre),
  )
  private val essexScrapers: Seq[CinemaScraper] = Seq(
    flicks("century-clacton", CenturyCinemaClacton),
    cineworld("x06ur-cineworld-cinema-basildon", CineworldBasildon),
    cineworld("x06x3-cineworld-cinema-braintree", CineworldBraintree),
    cineworld("x0x9g-cineworld-cinema-harlow-harvey-centre", CineworldHarlowHarveyCentre),
    cineworld("x079n-cineworld-cinema-harlow-queensgate", CineworldHarlowQueensgate),
    flicks("curzon-cinema-colchester", CurzonCinemaColchester),
    flicks("electric-palace-harwich", ElectricPalaceHarwich),
    flicks("empire-theatre-halstead-park", EmpireTheatreHalstead),
    everyman("X0XMY", EverymanCinemaChelmsford),
    flicks("movie-starr-canvey-island", MovieStarrCanveyIsland),
    odeon("498", OdeonCinemaChelmsford),
    odeon("515", OdeonCinemaColchester),
    odeon("500", OdeonCinemaSouthendOnSea),
    flicks("rio-burnham-on-crouch", RioBurnhamOnCrouch),
    flicks("empire-cinema-bishops", RoxyMoviesBishopSStortford),
    flicks("saffron-screen", SaffronScreen),
    vueUk("10099", VueCinemasBasildon),
    vueUk("20004", VueCinemasColchester),
    vueUk("10045", VueCinemasWestThurrock),
  )
  private val fermanaghScrapers: Seq[CinemaScraper] = Seq(
    flicks("imc-cinema-enniskillen", IMCCinemaEnniskillen),
  )
  private val fifeScrapers: Seq[CinemaScraper] = Seq(
    flicks("adam-smith-theatre-kirkcaldy", AdamSmithTheatreKirkcaldy),
    flicks("kino-glenrothes", KinoGlenrothes),
    odeon("508", OdeonCinemaDunfermline),
  )
  private val glamorganScrapers: Seq[CinemaScraper] = Seq(
    flicks("coliseum-theatre-aberdare", ColiseumTheatreAberdare),
    flicks("gwyn-hall-neath", GwynHallNeath),
    odeon("546", OdeonCinemaBridgend),
    odeon("920", OdeonCinemaSwansea),
    flicks("pontardawe-arts-centre", PontardaweArtsCentre),
    flicks("reel-port-talbot", ReelCinemaPortTalbot),
    flicks("taliesin-arts-centre-swansea", TaliesinArtsCentreSwansea),
    vueUk("10066", VueCinemasMerthyrTydfil),
    vueUk("10058", VueCinemasSwansea),
  )
  private val glasgowScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x0vq1-cineworld-cinema-glasgow-silverburn", CineworldSilverburnGlasgow),
    everyman("X11DQ", EverymanCinemaGlasgow),
    flicks("glasgow-film-theatre-glasgow", GlasgowFilmTheatre),
    flicks("grosvenor-cinema-glasgow", GrosvenorCinemaGlasgow),
    flicks("imax-glasgow", IMAXAtGlasgowScienceCentre),
    flicks("cumbernauld-theatre-at-lanternhouse", LanternhouseCinema),
    odeon("530", OdeonLuxeGlasgow),
    vueUk("10086", VueCinemasGlasgowFort),
    vueUk("10097", VueCinemasGlasgowStEnoch),
  )
  private val gloucestershireScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x07p5-cineworld-cinema-cheltenham", CineworldCheltenham),
    cineworld("x079m-cineworld-cinema-gloucester-quays", CineworldGloucesterQuays),
    flicks("electric-picture-house-wotton-under-edge", ElectricPictureHouseWottonUnderEdge),
    everyman("G01VU", EverymanCheltenham),
    flicks("guildhall-cinema-gloucester", GuildhallCinemaGloucester),
    flicks("studio-coleford", MerlinStudioColeford),
    flicks("palace-cinema-cinderford", PalaceCinemaCinderford),
    flicks("roses-theatre-tewksbury", RosesTheatreTewkesbury),
    flicks("sherborne-gloucester", SherborneCinemaGloucester),
    vueUk("10083", VueCinemasStroud),
  )
  private val guernseyScrapers: Seq[CinemaScraper] = Seq(
    flicks("beau-sejour-cinema-st-peter-port-guernsey", BeauSejourLeisureCentreGuernsey),
    flicks("the-mallard-cinema-guernsey", TheMallardCinemaGuernsey),
  )
  private val gwentScrapers: Seq[CinemaScraper] = Seq(
    flicks("baker-street-cinema-abergavenny", BakerStreetCinemaAbergavenny),
    cineworld("x078t-cineworld-cinema-newport-wales-spytty-park", CineworldSpyttyParkNewport),
    flicks("market-hall-cinema-brynmawr", MarketHallCinemaBrynmawr),
    flicks("maxime-blackwood", MaximeCinemaBlackwood),
    flicks("riverfront-newport", RiverfrontNewport),
    flicks("savoy-monmouth", SavoyTheatreMonmouth),
    vueUk("10067", VueCinemasCwmbran),
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
    cineworld("x06x9-cineworld-cinema-chichester-gate", CineworldChichester),
    cineworld("x0wee-cineworld-cinema-whiteley", CineworldWhiteley),
    everyman("X0711", EverymanCinemaWinchester),
    flicks("harbour-lights-picturehouse-southampton", HarbourLightsPicturehouse),
    flicks("hythe-moviola-cinema", HytheMoviolaCinema),
    flicks("no-6-cinema-portsmouth", No6CinemaPortsmouth),
    odeon("948", OdeonCinemaPortSolent),
    flicks("reel-cinema-fareham", ReelCinemaFareham),
    showcase("X0XV2", ShowcaseDeLuxSouthampton),
    flicks("southsea-cinema-and-arts-centre", SouthseaCinemaArtsCentre),
    flicks("the-living-room-cinema", TheLivingRoomCinemaLiphook),
    flicks("the-malt-lymington", TheMaltLymington),
    vueUk("10029", VueCinemasBasingstoke),
    vueUk("10069", VueCinemasEastleigh),
    vueUk("10021", VueCinemasPortsmouth),
  )
  private val herefordshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("courtyard-hereford", CourtyardHereford),
    flicks("gateway-ross-on-wye", GatewayCinemaRossOnWye),
    odeon("848", OdeonCinemaHereford),
    flicks("richard-booth-s-cinema-hay-on-wye", RichardBoothSBookshopHayOnWye),
  )
  private val hertfordshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("baldock-arts-and-heritage-centre", BaldockArtsHeritageCentre),
    flicks("hertford-theatre", BEAMHertfordTheatre),
    flicks("broadway-letchworth", BroadwayLetchworth),
    cineworld("x06sl-cineworld-cinema-hemel-hempstead", CineworldHemelHempstead),
    cineworld("x06xn-cineworld-cinema-stevenage", CineworldStevenage),
    cineworld("x11db-cineworld-cinema-watford", CineworldWatford),
    odeon("929", OdeonCinemaHatfield),
    flicks("reel-cinema-borehamwood", ReelCinemaBorehamwood),
    flicks("garden-city-cinema-welwyn", TheCinemaCampusWest),
    flicks("odyssey-st-albans", TheOdysseyStAlbans),
    flicks("rex-berkhamsted", TheRexCinemaBerkhamsted),
    vueUk("10023", VueCinemasWatford),
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
    vueUk("10049", VueCinemasInverness),
    flicks("west-side-cinema-stromness", WestSideCinemaStromness),
  )
  private val isleOfManScrapers: Seq[CinemaScraper] = Seq(
    flicks("broadway-cinema-villa-marina", BroadwayCinemaVillaMarina),
    flicks("palace-cinemas-isle-of-man", PalaceCinemasIsleOfMan),
  )
  private val isleOfWightScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x07ad-cineworld-cinema-isle-of-wight", CineworldNewportIsleOfWight),
    flicks("commodore-ryde-isle-of-wight", CommodoreRydeIsleOfWight),
  )
  private val jerseyScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x06xu-cineworld-cinema-jersey", CineworldStHelierJersey),
  )
  private val kentScrapers: Seq[CinemaScraper] = Seq(
    flicks("carlton-westgate-on-sea", CarltonCinemaWestgateOnSea),
    flicks("cinemarsh-the-marsh-academy", CinemarshTheMarshAcademy),
    cineworld("x06wy-cineworld-cinema-ashford", CineworldAshford),
    cineworld("x10r9-cineworld-cinema-dover", CineworldDover),
    cineworld("x079e-cineworld-cinema-rochester", CineworldRochester),
    flicks("curzon-canterbury-riverside", CurzonCanterburyRiverside),
    flicks("empire-cinema-sandwich", EmpireCinemaSandwich),
    flicks("gulbenkian-theatre-canterbury", GulbenkianTheatre),
    flicks("kavanagh-herne-bay", KavanaghCinemaHerneBay),
    flicks("kino-hawkhurst", KinoHawkhurst),
    odeon("050", OdeonCinemaChatham),
    odeon("159", OdeonCinemaMaidstone),
    odeon("512", OdeonCinemaTunbridgeWells),
    flicks("palace-cinema-broadstairs", PalaceCinemaKent),
    flicks("royal-faversham", RoyalCinemaFaversham),
    showcase("X06JR", ShowcaseDeLuxBluewater),
    flicks("silver-screen-folkestone", SilverScreenFolkestone),
    flicks("stag-sevenoaks", StagSevenoaks),
    flicks("picturehouse-ashford", TheAshfordCinemaFormerlyPicturehouse),
    flicks("the-light-cinemas-sittingbourne", TheLightSittingbourne),
    flicks("the-woodville-gravesend", TheWoodvilleGravesend),
    vueUk("10063", VueCinemasThanetWestwoodCross),
  )
  private val lanarkshireScrapers: Seq[CinemaScraper] = Seq(
    odeon("923", OdeonLuxeEastKilbride),
    showcase("X06KE", ShowcaseGlasgowCoatbridge),
    vueUk("10003", VueCinemasHamilton),
  )
  private val lancashireScrapers: Seq[CinemaScraper] = Seq(
    flicks("the-backlot-cinema-and-diner", ArcCinemaBlackpool),
    flicks("arc-cinema-preston", ArcCinemaPreston),
    cineworld("x079p-cineworld-cinema-bolton", CineworldBolton),
    cineworld("x0vhp-cineworld-cinema-broughton", CineworldBroughton),
    everyman("X11NR", EverymanCinemaClitheroe),
    flicks("flower-bowl-entertainment-centre-preston", FlowerBowlEntertainmentCentrePreston),
    flicks("lowther-pavilion-lytham", LowtherPavilionLytham),
    odeon("925", OdeonCinemaPreston),
    odeon("843", OdeonCinemaRochdale),
    flicks("reel-cinema-blackburn", ReelCinemaBlackburn),
    flicks("reel-cinema-chorley", ReelCinemaChorley),
    flicks("reel-cinema-burnley", ReelCinemasBurnley),
    flicks("regent-blackpool", RegentBlackpool),
    flicks("the-dukes-lancaster", TheDukesLancaster),
    flicks("island-cinemas-lytham-st-annes", TheIslandLythamStAnnes),
    flicks("the-light-cinemas-bolton", TheLightBolton),
    vueUk("10051", VueCinemasAccrington),
    vueUk("10007", VueCinemasBlackburn),
    vueUk("10040", VueCinemasBolton),
    vueUk("10073", VueCinemasBury),
    vueUk("10060", VueCinemasCleveleys),
    vueUk("10061", VueCinemasLancaster),
    vueUk("10043", VueCinemasPreston),
  )
  private val leicestershireScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x0wef-cineworld-cinema-hinckley", CineworldHinckley),
    flicks("flix-student-run-cinema-loughborough", FlixStudentRunCinemaLoughborough),
    odeon("758", OdeonCinemaLoughborough),
    odeon("505", OdeonLuxeLeicester),
    flicks("phoenix-square-leicester", PhoenixCinemaAndArtCentreLeicester),
    flicks("piccadilly-leicester", PiccadillyCinemaLeicester),
    flicks("regal-melton-mowbray", RegalMeltonMowbray),
    showcase("X08NG", ShowcaseDeLuxLeicester),
    vueUk("10026", VueCinemasLeicester),
  )
  private val lincolnshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("arts-centre-stamford", ArtsCentreStamford),
    everyman("G004G", EverymanCinemaLincoln),
    flicks("junction-goole", JunctionGoole),
    flicks("kinema-in-the-woods-woodhall-spa", KinemaInTheWoods),
    flicks("loewen-mablethorpe", LoewenCinema),
    odeon("568", OdeonCinemaLincoln),
    flicks("playhouse-louth", ParkwayCinemaLouth),
    flicks("parkway-cleethorpes", ParkwayCleethorpes),
    flicks("savoy-boston", SavoyBoston),
    flicks("savoy-grantham", SavoyGrantham),
    flicks("sleaford-playhouse", SleafordPlayhouse),
    flicks("tower-skegness", TowerCinemaSkegness),
    vueUk("10050", VueCinemasScunthorpe),
  )
  private val londonderryScrapers: Seq[CinemaScraper] = Seq(
    flicks("brunswick-moviebowl-londonderry", BrunswickMoviebowlLondonderry),
    flicks("movie-house-coleraine", MovieHouseColeraine),
    flicks("movie-house-maghera", MovieHouseMaghera),
    flicks("nerve-centre-londonderry", NerveCentreLondonderry),
    flicks("omniplex-londonderry", OmniplexLondonderry),
  )
  private val liverpoolScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x11bh-cineworld-cinema-speke", CineworldSpeke),
    cineworld("x06xm-cineworld-cinema-st-helens", CineworldStHelens),
    everyman("X11I7", EverymanCinemaLiverpool),
    odeon("560", OdeonLiverpoolONE),
    odeon("522", OdeonLiverpoolSwitchIsland),
    odeon("475", OdeonLuxeBromborough),
    flicks("picturehouse-at-fact-liverpool", PicturehouseAtFACTLiverpool),
    flicks("plaza-crosby", PlazaCommunityCinemaLiverpool),
    showcase("X06JU", ShowcaseDeLuxLiverpool),
    flicks("bijou-southport", SouthportBijouCinema),
    flicks("the-light-cinemas-new-brighton", TheLightNewBrighton),
    vueUk("10055", VueCinemasBirkenhead),
    vueUk("10004", VueCinemasSouthport),
    flicks("woolton-picture-house", WooltonPictureHouse),
  )
  private val northYorkshireScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x12iw-cineworld-cinema-york", CineworldYork),
    flicks("city-screen-picturehouse-york", CityScreenPicturehouseYork),
    everyman("X0X9K", EverymanCinemaHarrogate),
    everyman("G01OJ", EverymanCinemaNorthallerton),
    everyman("X06TJ", EverymanCinemaYork),
    flicks("hollywood-plaza-scarborough", HollywoodPlazaScarborough),
    odeon("806", OdeonCinemaHarrogate),
    odeon("395", OdeonMiddlesbrough),
    flicks("pavilion-whitby", PavilionCinemaWhitby),
    flicks("pocklington-arts-centre", PocklingtonArtsCentre),
    flicks("regent-redcar", RegentRedcar),
    flicks("ritz-thirsk", RitzCinemaThirsk),
    flicks("roxy-movies-middlesbrough", RoxyMoviesMiddlesbrough),
    flicks("savoy-cinema-catterick-garrison", SavoyCinemaCatterickGarrison),
    flicks("station-richmond", StationCinemaRichmond),
    flicks("stephen-joseph-theatre-scarborough", StephenJosephTheatreScarborough),
    flicks("the-forum-northallerton", TheForumNorthallerton),
    vueUk("10048", VueCinemasYork),
  )
  private val northamptonshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("arc-cinema-daventry", ArcCinemaDaventry),
    cineworld("x125z-cineworld-cinema-rushden-lakes", CineworldRushdenLakes),
    flicks("forum-northampton", ForumCinemaNorthampton),
    flicks("northampton-filmhouse-northampton", NorthamptonFilmhouse),
    odeon("380", OdeonNorthampton),
    flicks("savoy-corby", SavoyCinemaCorby),
    vueUk("10005", VueCinemasNorthampton),
  )
  private val northumberlandScrapers: Seq[CinemaScraper] = Seq(
    flicks("forum-hexham", ForumCinemaHexham),
    flicks("market-pavillion-cinema-blyth", MarketPavilionCinemaBlyth),
    flicks("phoenix-cinema-blyth", PhoenixCinemaBlyth),
    flicks("the-maltings-berwick-upon-tweed", TheMaltingsBerwickUponTweed),
    vueUk("10085", VueCinemasCramlington),
  )
  private val nottinghamshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("arc-cinema-at-the-byron-hucknall", ArcCinemaAtTheByronHucknall),
    flicks("arc-cinema-beeston", ArcCinemaBeeston),
    flicks("broadway-nottingham", BroadwayCinemaNottingham),
    odeon("842", OdeonCinemaMansfield),
    odeon("757", OdeonCinemaNewark),
    flicks("scala-ilkeston", ReelCinemaScalaIlkeston),
    flicks("savoy-nottingham", SavoyCinemaNottingham),
    flicks("savoy-worksop", SavoyWorksop),
    showcase("X06JN", ShowcaseDeLuxNottingham),
    vueUk("20003", VueCinemasNottingham),
  )
  private val oxfordshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("abbey-cinema-abingdon", AbbeyCinemaAbingdon),
    cineworld("x083o-cineworld-cinema-didcot", CineworldDidcot),
    cineworld("x0fr6-cineworld-cinema-witney", CineworldWitney),
    flicks("corn-exchange-cinema-wallingford", CornExchangeCinemaWallingford),
    flicks("curzon-cinema-oxford", CurzonCinemaOxford),
    flicks("phoenix-picturehouse-oxford", PhoenixPicturehouseOxford),
    flicks("regal-picturehouse-henley", RegalPicturehouseHenley),
    flicks("the-light-cinemas-banbury", TheLightBanbury),
    flicks("the-living-room-cinema-chipping-norton", TheLivingRoomCinemaChippingNorton),
    flicks("cinema-oxford", TheOxfordCinemaCafe),
    flicks("ultimate-picture-palace-oxford", UltimatePicturePalaceOxford),
    vueUk("10087", VueCinemasBicester),
    vueUk("10008", VueCinemasOxford),
  )
  private val powysScrapers: Seq[CinemaScraper] = Seq(
    flicks("coliseum-brecon", ColiseumCinemaBrecon),
    odeon("598", OdeonCinemaWrexham),
    flicks("wyeside-arts-centre-builth-wells", WyesideArtsCentreBuilthWells),
  )
  private val renfrewshireScrapers: Seq[CinemaScraper] = Seq(
    odeon("615", OdeonCinemaBraehead),
    showcase("X06KC", ShowcaseDeLuxPaisley),
    flicks("the-tower-digital-arts-center-scottish-submarine-centre", TheTowerDigitalArtsCenterHelensburgh),
    flicks("waterfront-greenock", WaterfrontGreenock),
  )
  private val roxburghEttrickAndLauderdaleScrapers: Seq[CinemaScraper] = Seq(
    flicks("pavilion-galashiels", PavilionCinemaGalashiels),
    flicks("tower-mill-cinema-hawick", TowerMillHeartOfHawick),
  )
  private val sandwellScrapers: Seq[CinemaScraper] = Seq(
    odeon("761", OdeonCinemaWestBromwich),
  )
  private val shropshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("assembly-rooms-ludlow", AssemblyRoomsLudlow),
    cineworld("x06xk-cineworld-cinema-shrewsbury", CineworldShrewsbury),
    cineworld("x0u5o-cineworld-cinema-telford", CineworldTelford),
    flicks("festival-drayton-centre-market-drayton", FestivalDraytonCentre),
    flicks("maona-cinema-oswestry", MaonaCinemaOswestry),
    odeon("916", OdeonLuxeTelford),
    flicks("old-market-hall-shrewsbury", OldMarketHallShrewsbury),
    flicks("majestic-bridgnorth", ReelCinemaBridgnorthMajestic),
    flicks("wellington-orbit-wellington", WellingtonOrbit),
  )
  private val somersetScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x10kq-cineworld-cinema-weston-super-mare", CineworldWestonSuperMare),
    cineworld("x06xt-cineworld-cinema-yeovil", CineworldYeovil),
    flicks("curzon-cinema-clevedon", CurzonCinemaClevedon),
    everyman("G01VT", EverymanBath),
    flicks("little-theatre-picturehouse", LittleTheatrePicturehouse),
    flicks("wellesley-wellington", MerlinWellesleyWellington),
    odeon("355", OdeonCinemaBath),
    odeon("453", OdeonCinemaTaunton),
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
    cineworld("g01hn-cineworld-cinema-barnsley", CineworldBarnsley),
    flicks("parkway-barnsley", ParkwayBarnsley),
    flicks("savoy-doncaster", SavoyDoncaster),
    vueUk("10041", VueCinemasDoncaster),
  )
  private val staffordshireScrapers: Seq[CinemaScraper] = Seq(
    flicks("electric-palace-picture-house-cannock", CannockCinema),
    flicks("cinebowl-uttoxeter", CinebowlUttoxeter),
    cineworld("x06x5-cineworld-cinema-burton-upon-trent", CineworldBurtonOnTrent),
    cineworld("x0weg-cineworld-cinema-stoke-on-trent", CineworldStokeOnTrent),
    cineworld("x06xr-cineworld-cinema-wolverhampton", CineworldWolverhampton),
    flicks("film-theatre-stoke-on-trent", FilmTheatreStokeOnTrent),
    flicks("lichfield-garrick-theatre-and-studio", LichfieldGarrickTheatreStudio),
    flicks("lockworks-cinema", LockworksCinemaWolverhampton),
    odeon("430", OdeonCinemaStokeOnTrent),
    odeon("930", OdeonCinemaTamworth),
    odeon("857", OdeonLuxeStafford),
    flicks("red-carpet-barton-under-needwood", RedCarpetBartonMarina),
    flicks("the-light-cinemas-walsall", TheLightWalsall),
    vueUk("10053", VueCinemasNewcastleUnderLyme),
  )
  private val suffolkScrapers: Seq[CinemaScraper] = Seq(
    flicks("abbeygate-bury-st-edmunds", AbbeygateBuryStEdmunds),
    flicks("aldeburgh-cinema", AldeburghCinema),
    cineworld("x07mh-cineworld-cinema-bury-st-edmunds", CineworldBuryStEdmunds),
    cineworld("x08wo-cineworld-cinema-haverhill", CineworldHaverhill),
    cineworld("x078j-cineworld-cinema-ipswich", CineworldIpswich),
    flicks("electric-picture-palace-southwold", ElectricPicturePalaceSouthwold),
    everyman("G0210", EverymanBuryStEdmunds),
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
    cineworld("x0njx-cineworld-cinema-aldershot", CineworldAldershot),
    everyman("X070Y", EverymanCinemaOxted),
    everyman("X070X", EverymanCinemaReigate),
    flicks("haslemere-hall-cinema", HaslemereHallCinema),
    odeon("510", OdeonCinemaGuildford),
    flicks("reel-cinemas-farnham", ReelCinemasFarnham),
    flicks("the-light-redhill", TheLightRedhill),
    vueUk("10068", VueCinemasCamberley),
    vueUk("10089", VueCinemasFarnborough),
  )
  private val taysideScrapers: Seq[CinemaScraper] = Seq(
    flicks("birks-aberfeldy", BirksAberfeldy),
    flicks("chalmers-arbroath-cinema", ChalmersFilmhouseArbroath),
    cineworld("x07a0-cineworld-cinema-dundee", CineworldDundee),
    flicks("dundee-contemporary-arts-dca-dundee", DundeeContemporaryArtsDCA),
    flicks("new-picture-house-st-andrews", NewPictureHouseStAndrews),
    odeon("044", OdeonLuxeDundee),
    flicks("playhouse-perth", PlayhouseCinemaPerth),
    flicks("the-montrose-playhouse", TheMontrosePlayhouse),
  )
  private val tyneAndWearScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x079v-cineworld-cinema-boldon-tyne-and-wear", CineworldBoldonTyneWear),
    cineworld("x06th-cineworld-cinema-newcastle-upon-tyne", CineworldNewcastle),
    flicks("customs-house-south-shields", CustomsHouseCinemaSouthShields),
    everyman("X11KB", EverymanCinemaNewcastle),
    flicks("jam-jar-cinema", JamJarCinema),
    odeon("980", OdeonCinemaMetrocentre),
    odeon("961", OdeonCinemaSilverlink),
    flicks("empire-cinema-sunderland-tyne-wear", OmniplexSunderlandFormerlyEmpire),
    flicks("star-and-shadow-cinema-newcastle", StarAndShadowCinemaNewcastle),
    flicks("tyneside-newcastle", TynesideNewcastle),
    vueUk("10088", VueCinemasGateshead),
  )
  private val tyroneScrapers: Seq[CinemaScraper] = Seq(
    flicks("omniplex-dungannon", OmniplexDungannon),
    flicks("omniplex-omagh", OmniplexOmagh),
    flicks("ritz-multiplex-cookstown", RitzMultiplexCookstown),
  )
  private val warwickshireScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x06xi-cineworld-cinema-rugby", CineworldRugby),
    everyman("X0YZU", EverymanCinemaStratfordUponAvon),
    odeon("040", OdeonCinemaCoventry),
    odeon("244", OdeonLuxeNuneaton),
    flicks("royal-spa-centre-cinema-leamington", RoyalSpaCentre),
    showcase("X06JI", ShowcaseDeLuxCoventry),
    vueUk("10079", VueCinemasLeamingtonSpa),
    flicks("warwick-arts-centre-coventry", WarwickArtsCentreCoventry),
  )
  private val westSussexScrapers: Seq[CinemaScraper] = Seq(
    flicks("atrium-east-grinstead", AtriumEastGrinstead),
    flicks("capitol-horsham", CapitolHorsham),
    cineworld("x079c-cineworld-cinema-crawley", CineworldCrawley),
    flicks("connaught-theatre-studio-worthing", ConnaughtTheatreStudioWorthing),
    flicks("dome-worthing", DomeWorthing),
    everyman("X11LP", EverymanCinemaHorsham),
    flicks("orion-burgess-hill", OrionBurgessHill),
    flicks("picturedrome-bognor-regis", PicturedromeBognorRegis),
    flicks("windmill-littlehampton", WindmillLittlehampton),
  )
  private val westYorkshireScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x06x2-cineworld-cinema-bradford", CineworldBradford),
    cineworld("x106z-cineworld-cinema-leeds-white-rose", CineworldLeeds),
    cineworld("x06xp-cineworld-cinema-wakefield", CineworldWakefield),
    flicks("cottage-road-cinema-leeds", CottageRoadCinemaLeeds),
    everyman("X0QM5", EverymanCinemaLeeds),
    flicks("heart-centre-headingley", HeartCentreHeadingley),
    flicks("picturehouse-hebden-bridge", HebdenBridgePicturehouse),
    flicks("hyde-park-picture-house-leeds", HydeParkPictureHouse),
    flicks("ilkley-cinema-ilkley", IlkleyCinema),
    odeon("953", OdeonCinemaHuddersfield),
    odeon("860", OdeonLuxeLeedsThorpePark),
    odeon("484", OdeonLuxeLeedsBradford),
    flicks("picturehouse-keighley", PictureHouseKeighley),
    flicks("national-science-and-media-museum-bradford", PicturevilleScienceAndMediaMuseumBradford),
    flicks("plaza-skipton", PlazaSkipton),
    flicks("reel-cinema-wakefield", ReelCinemaWakefield),
    flicks("rex-elland", RexElland),
    showcase("X06JK", ShowcaseDeLuxLeeds),
    flicks("the-light-cinemas-bradford", TheLightBradford),
    vueUk("20002", VueCinemasCastleford),
    vueUk("10076", VueCinemasHalifax),
    vueUk("10037", VueCinemasLeedsKirkstallRoad),
    vueUk("10012", VueCinemasLeedsTheLight),
    flicks("wetherby-film-theatre-wetherby", WetherbyFilmTheatre),
  )
  private val wiltshireScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x079h-cineworld-cinema-swindon-shaw-ridge", CineworldShawRidgeSwindon),
    everyman("G01QM", EverymanCinemaSalisbury),
    odeon("755", OdeonCinemaAndover),
    odeon("220", OdeonCinemaSalisbury),
    odeon("847", OdeonCinemaTrowbridge),
    flicks("palace-cinema-devizes", PalaceCinemaDevizes),
    flicks("astoria-chippenham", ReelCinemaChippenhamAstoria),
    flicks("regal-fordingbridge", RegalCinemaFordingbridge),
    flicks("the-parade-cinema-marlborough", TheParadeCinemaMarlborough),
    vueUk("10098", VueCinemasSwindon),
  )
  private val worcestershireScrapers: Seq[CinemaScraper] = Seq(
    flicks("castlemorton-cinema-morton-majestic", CastlemortonCinemaMortonMajestic),
    flicks("lume-cinema-and-cafe", FuturistCinema),
    flicks("cinema-malvern", MalvernTheatres),
    flicks("number-8-pershore", Number8Pershore),
    odeon("282", OdeonCinemaWorcester),
    flicks("regal-evesham", RegalCinemaEvesham),
    flicks("regal-tenbury-wells", RegalCinemaTenburyWells),
    vueUk("10081", VueCinemasRedditch),
    vueUk("10017", VueCinemasWorcester),
  )
  private val yorkshireScrapers: Seq[CinemaScraper] = Seq(
    cineworld("x079y-cineworld-cinema-sheffield", CineworldSheffield),
    odeon("209", OdeonLuxeSheffield),
    flicks("paramount-penistone", ParamountPenistone),
    flicks("showroom-sheffield", ShowroomSheffield),
    flicks("the-light-cinemas-sheffield", TheLightSheffield),
    vueUk("10042", VueCinemasSheffield),
  )

  // ── Germany (AlloCiné/Filmstarts website-JSON) ───────────────────────────
  private def filmstarts(theaterId: String, cinema: Cinema): WebediaShowtimesClient =
    new WebediaShowtimesClient(http, WebediaMarket.Germany, theaterId, cinema, today = Some(today))
  // Germany — data-driven from the full GermanRoster (158 regions / 1,518 cinemas):
  // one filmstarts scraper per cinema, keyed by region slug (the slug City.slug uses).
  // Each cinema's Filmstarts theaterId comes from GermanRoster.theaterIdByCinema.
  private val germanBaseByCity: Map[String, Seq[CinemaScraper]] =
    models.GermanRoster.regions.map { region =>
      region.slug -> region.cinemas.map(c => filmstarts(models.GermanRoster.theaterIdByCinema(c), c))
    }.toMap

  // ── Spain (AlloCiné/SensaCine website-JSON) ──────────────────────────────
  // The SAME client Germany uses, on a different market — so a different HOST,
  // which is what keeps the two countries' pace gates and 429 back-offs
  // independent of each other; see `WebediaMarket`.
  private def sensacine(theaterId: String, cinema: Cinema): WebediaShowtimesClient =
    new WebediaShowtimesClient(http, WebediaMarket.Spain, theaterId, cinema, today = Some(today))

  // Ocine venues read the chain's own ticketing server instead — SensaCine had
  // no programme at all for most of them, and does not list some at all; see
  // `data/spain/ocine.json` for which and why. Plain `http`: the servers sit
  // behind no bot wall.
  private def ocine(ticketingSlug: String, cinema: Cinema): OcineClient =
    new OcineClient(http, ticketingSlug, cinema, today = Some(today))

  // Spain — data-driven from the full SpanishRoster (52 provinces / 602 cinemas):
  // one scraper per cinema, keyed by the PROVINCE slug City.slug uses — the
  // venue's own chain server where the roster names one, SensaCine otherwise.
  // Keyed off `Country.Spain.cities` rather than off `SpanishRoster.places`,
  // because the slug a province is finally addressable under is decided in
  // `City.spanishCities` (one of them is qualified away from a US metro's) and
  // the catalogue has to agree with the roster the web tier serves.
  private val spanishBaseByCity: Map[String, Seq[CinemaScraper]] =
    models.Country.Spain.cities.map { city =>
      city.slug -> city.cinemas.map { c =>
        models.SpanishRoster.ocineServerByCinema.get(c).map(ocine(_, c))
          .getOrElse(sensacine(models.SpanishRoster.theaterIdByCinema(c), c))
      }
    }.toMap

  // ── United States (chain-primary, Flicks for the rest) ───────────────────
  // Data-driven from the full UsRoster (468 metros and small states / 5,031 cinemas):
  // one scraper per cinema, keyed by the state slug that City.slug uses.
  //
  // A venue named in `UsChainVenues` gets its CHAIN'S OWN site as the primary and
  // keeps flicks.us as the fallback (below); every other venue is flicks.us
  // primary as before, its slug from UsRoster.flicksSlugByCinema. Same client and
  // same residential egress as the UK for the Flicks leg — only the market
  // differs, which is what keeps the two markets' pace gates and 429 back-offs
  // independent (see FlicksMarket).
  //
  // WHY THESE CHAINS AND NOT THE OTHERS. Moving a venue off the aggregator is only
  // safe if the chain's own feed advertises AT LEAST as much of the programme:
  // `MovieCache`'s scrape-prune reads a film's absence from a complete listing as
  // "it stopped screening", so a shorter-horizon primary DELETES the advance-sale
  // tail on every successful scrape (see ScrapeHorizon — it cost the UK its whole
  // event programme once already). All three chains here were measured against
  // flicks.us on the same venues on 2026-08-30 and each reached the same furthest
  // date or better, with equal or more populated days. Regal and AMC were measured
  // too, but their own sites answer only US IPs, so both stay on flicks.us
  // (see `RegalClient` / `AmcClient`, unwired). The other four US mid-tier chains are NOT here, and
  // neither is Cinemark (38-193 days SHORTER than flicks on all 20 venues
  // measured) — `UsChainVenues` names each one and why.
  //
  // Alamo reaches our datacenter egress directly (no Cloudflare challenge from it
  // on 2026-08-30), as do both Webedia hosts — so all three use `http`, not the
  // residential `flicksFetch` the Flicks leg needs.
  private def alamo(venue: UsChainVenues.AlamoVenue, cinema: Cinema): AlamoDrafthouseClient =
    new AlamoDrafthouseClient(http, venue.slug, cinema, ZoneId.of(venue.zoneId), today = Some(today))
  private def webedia(baseUrl: String, venue: UsChainVenues.WebediaVenue, cinema: Cinema): GatsbyBoxOfficeClient =
    new GatsbyBoxOfficeClient(http, baseUrl, venue.theaterId, cinema,
      timeZone = venue.zoneId, venuePath = Some(venue.venuePath), today = today)

  /** The chain-primary scraper for a US venue, or `None` when it stays on Flicks.
   *
   *  The chains key off the venue's DISPLAY NAME (their own rosters name venues,
   *  not slugs), an exact lookup — a venue absent from every map falls through to
   *  Flicks, which is the correct answer for the chain locations whose operator no
   *  longer lists them. */
  private def usChainScraper(cinema: Cinema): Option[CinemaScraper] = {
    val name = cinema.displayName
    UsChainVenues.alamoDrafthouse.get(name).map(alamo(_, cinema))
      .orElse(UsChainVenues.showcaseUs.get(name).map(webedia(GatsbyBoxOfficeClient.ShowcaseUsBaseUrl, _, cinema)))
      .orElse(UsChainVenues.landmark.get(name).map(webedia(GatsbyBoxOfficeClient.LandmarkBaseUrl, _, cinema)))
  }

  private val usBaseByCity: Map[String, Seq[CinemaScraper]] =
    models.Country.UnitedStates.cities.map { city =>
      city.slug -> city.cinemas.map { c =>
        usChainScraper(c).getOrElse(flicksUs(models.UsRoster.flicksSlugByCinema(c), c))
      }
    }.toMap

  /** US chain venues → the flicks.us slug they keep as their FALLBACK.
   *
   *  Derived from the same `UsRoster.flicksSlugByCinema` their Flicks primary used
   *  to be built from, rather than restated in a table: a US venue is a runtime
   *  object with no case-object name to write down, and deriving means the primary
   *  and the fallback cannot drift apart about which venue they mean. */
  private val usFlicksFallback: Map[Cinema, ChainFlicksFallback.FlicksFallback] =
    models.Country.UnitedStates.cities.flatMap(_.cinemas)
      .filter(c => usChainScraper(c).isDefined)
      .flatMap(c => models.UsRoster.flicksSlugByCinema.get(c)
        .map(slug => c -> ChainFlicksFallback.FlicksFallback(FlicksMarket.UnitedStates, slug)))
      .toMap

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
    "piotrkow-trybunalski" -> piotrkowTrybunalskiScrapers,
    "siedlce" -> siedlceScrapers,
    "pila" -> pilaScrapers,
    "ostrowiec-swietokrzyski" -> ostrowiecSwietokrzyskiScrapers,
    "gniezno" -> gnieznoScrapers,
    "suwalki" -> suwalkiScrapers,
    "stalowa-wola" -> stalowaWolaScrapers,
    "zamosc" -> zamoscScrapers,
    "leszno" -> lesznoScrapers,
    "lomza" -> lomzaScrapers,
    "pulawy" -> pulawyScrapers,
    "skierniewice" -> skierniewiceScrapers,
    "starogard-gdanski" -> starogardGdanskiScrapers,
    "ciechanow" -> ciechanowScrapers,
    "wielun" -> wielunScrapers,
    "chojnice" -> chojniceScrapers,
    "zgorzelec" -> zgorzelecScrapers,
    "ilawa" -> ilawaScrapers,
    "ketrzyn" -> ketrzynScrapers,
    "zakopane" -> zakopaneScrapers,
    "wyszkow" -> wyszkowScrapers,
    "zlocieniec" -> zlocieniecScrapers,
    "slubice" -> slubiceScrapers,
    "wlodawa" -> wlodawaScrapers,
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
  ) ++ germanBaseByCity   // Germany: the full 158-region roster (data-driven)
    ++ usBaseByCity       // USA: 461 metros + 7 small states (data-driven)
    ++ spanishBaseByCity  // Spain: the full 52-province roster (data-driven)

  // Venues the 2026-09 sweeps added (Filmweb, bilety24, biletyna, the chains' own
  // branch lists), grouped by the city they were first found near. The key is
  // only where each is WIRED: the page a venue is listed on — its own town, a
  // cluster of small towns, or a major city — is `models.PolishPages`, and
  // `byCity` below files every Polish scraper under that page, whatever key it
  // sits under here.
  private val nearbyTowns: Map[String, Seq[CinemaScraper]] = Map(
    "szczecin" -> Seq(
      filmweb(307, KinoMOKPolice),   // Police
      filmweb(1662, KinoEva),   // Międzyzdroje
      biletyna(KinoGryfinskiDomKultury),   // Gryfino
    ),
    "bielsko-biala" -> Seq(
      biletyna(KinoPromyk),   // Bystra
      biletyna(KinoCentrumWadowice),   // Wadowice
      cinemaCity("1098", CinemaCityCieszyn),   // Cieszyn
      filmweb(2412, KinoCKiF),   // Sucha Beskidzka
    ),
    "warszawa" -> Seq(
      biletyna(KinoDomSztukiUrsynow),   // Warszawa
      biletyna(KinoTerminalKultury),   // Warszawa
      biletyna(KinoBielanskiOsrodekKultury),   // Warszawa
      biletyna(KinoWawerskaStrefaKultury),   // Warszawa
      biletyna(KinoBasn),   // Piastów
      biletyna(KinoKasynoOficerskie),   // Nowy Dwór Mazowiecki
      bilety24("https://www.bilety24.pl/kino/organizator/osrodek-kultury-gminy-grodzisk-mazowiecki-1231", KinoGrodziskieCentrumKultury),   // Grodzisk Mazowiecki
      filmweb(1514, KinoCentrumKulturyBlonie),   // Błonie
      filmweb(1683, KinoGrojeckiOsrodekKultury),   // Grójec
      filmweb(1860, KinoUciecha),   // Góra Kalwaria
      helios(HeliosNuxt.Wolomin),   // Wołomin
      biletyna(KinoKulturaWolomin),   // Wołomin
      filmweb(2079, KinoTeatrKurtyna),   // Sulejówek
      multikino("0039", MultikinoPruszkow),   // Pruszków
      filmweb(3137, KinoCKiSPruszkow),   // Pruszków
      helios(HeliosNuxt.Legionowo),   // Legionowo
      filmweb(2340, KinoOaza),   // Otwock
    ),
    "lodz" -> Seq(
      biletyna(KinoMOKGlowno),   // Głowno
      filmweb(1404, KinoTomi),   // Pabianice
      helios(HeliosNuxt.Pabianice),   // Pabianice
      filmweb(2065, KinoGornikLeczyca),   // Łęczyca
    ),
    "dabrowa-gornicza" -> Seq(
      filmweb(1487, KinoZbyszekOlkusz),   // Olkusz
      multikino("0038", MultikinoJaworzno),   // Jaworzno
      biletyna(KinoSztuka),   // Chrzanów
    ),
    "krakow" -> Seq(
      filmweb(1491, KinoMuzaMyslenice),   // Myślenice
      filmweb(1704, KinoRadosc),   // Wolbrom
      filmweb(1906, KinoGryf),   // Miechów
      bilety24("https://www.bilety24.pl/kino/organizator/kino-wazka-w-niepolomicach-pole-kultury-samorzadowa-instytucja-kultury-miasta-i-gminy-niepolomice-1273", KinoWazka),   // Niepołomice
      filmweb(2434, KinoWielickaMediateka),   // Wieliczka
      bilety24("https://www.bilety24.pl/kino/organizator/kino-lubon-2-0-1549", KinoLubon),   // Mszana Dolna
    ),
    "poznan" -> Seq(
      biletyna(KinoCKPrzezmierowo),   // Przeźmierowo
      biletyna(KinoNOKNowyTomysl),   // Nowy Tomyśl
      bilety24("https://www.bilety24.pl/kino/organizator/kino-baszta-477", KinoBasztaSroda),   // Środa Wielkopolska
      bilety24("https://www.bilety24.pl/kino/organizator/kino-halszka-494", KinoHalszka),   // Szamotuły
      bilety24("https://www.bilety24.pl/kino/organizator/sremski-osrodek-kultury-kinoteatr-slonko-478", KinoteatrSlonko),   // Śrem
      bilety24("https://www.bilety24.pl/kino/organizator/biblioteka-i-kino-miasta-i-gminy-buk-596", KinoWielkopolanin),   // Buk
      bilety24("https://www.bilety24.pl/kino/organizator/przedsiebiorstwo-wielobranzowe-trojka-s-c-475", KinoTrojka),   // Września
      bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-i-biblioteka-imienia-powstancow-wielkopolskich-w-opalenicy-967", KinoOpalenica),   // Opalenica
      bilety24("https://www.bilety24.pl/kino/organizator/kinoteatr-gwiazda-962", KinoteatrGwiazda),   // Wronki
    ),
    "opole" -> Seq(
      bilety24("https://www.bilety24.pl/kino/organizator/brzeskie-centrum-kultury-im-i-j-paderewskiego-1672", KinoCentrumBrzeg),   // Brzeg
      filmweb(1657, KinoCinemaN),   // Nysa
    ),
    "wroclaw" -> Seq(
      biletyna(KinoNowyPafawag),   // Wrocław
      bilety24("https://www.bilety24.pl/kino/organizator/kino-odra-1601", KinoOdraBrzegDolny),   // Brzeg Dolny
      bilety24("https://www.bilety24.pl/kino/organizator/centrum-sztuki-w-olawie-1498", KinoOdraOlawa),   // Oława
      filmweb(2161, GoKinoOlawa),   // Oława
      filmweb(1950, KinoGrazyna),   // Strzelin
      bilety24("https://www.bilety24.pl/kino/organizator/kino-polonia-1090", KinoPoloniaTrzebnica),   // Trzebnica
      bilety24("https://www.bilety24.pl/kino/organizator/miejski-osrodek-kultury-i-sztuki-w-olesnicy-1193", KinoMOKiSOlesnica),   // Oleśnica
    ),
    "kielce" -> Seq(
      filmweb(1720, KinoZdrojBusko),   // Busko-Zdrój
      filmweb(1482, KinoMuzaWloszczowa),   // Włoszczowa
    ),
    "trojmiasto" -> Seq(
      biletyna(TeatrBoto),   // Sopot
      biletyna(TeatrAtelier),   // Sopot
      biletyna(KinoCKGniewino),   // Gniewino
      filmweb(1835, KinoWCK),   // Wejherowo
      biletyna(KinoZaRogiemChmielno),   // Chmielno
      bilety24("https://www.bilety24.pl/kino/organizator/kino-zeglarz-1224", KinoZeglarz),   // Jastarnia
      bilety24("https://www.bilety24.pl/kino/organizator/kartuskie-centrum-kultury-w-kartuzach-1364", KinoKCK),   // Kartuzy
      bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-i-sportu-w-pruszczu-gdanskim-1569", KinoCKiSPruszczGdanski),   // Pruszcz Gdański
    ),
    "bydgoszcz" -> Seq(
      biletyna(KinoCKSwiecie),   // Świecie
      filmweb(2329, KinoRelaks),   // Nakło nad Notecią
      msi(KinoManhattanJanikowo),   // Janikowo
    ),
    "tarnow" -> Seq(
      filmweb(2408, KinoMCKRadlow),   // Radłów
      bilety24("https://www.bilety24.pl/kino/organizator/gminne-centrum-kultury-i-czytelnictwa-w-lisiej-gorze-1658", KinoCentrumLisiaGora),   // Lisia Góra
      filmweb(2314, KinoJDK),   // Jasło
      multikino("0051", MultikinoMielec),   // Mielec
    ),
    "gorzow-wielkopolski" -> Seq(
      biletyna(KinoMewaMiedzychod),   // Międzychód
      bilety24("https://www.bilety24.pl/kino/organizator/barlinecki-osrodek-kultury-1052", KinoPanorama),   // Barlinek
      msi(KinoMOKMiedzyrzecz),   // Międzyrzecz
    ),
    "zielona-gora" -> Seq(
      new KinoZaRogiemSiedlecClient(http, KinoZaRogiemSiedlec, today),   // Siedlec
    ),
    "nowy-sacz" -> Seq(
      bilety24("https://www.bilety24.pl/kino/organizator/mckcie-w-grybowie-kino-biala-1321", KinoMCKGrybow),   // Grybów
    ),
    "lublin" -> Seq(
      bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-promocji-i-turystyki-w-poniatowej-1220", KinoCzyn),   // Poniatowa
      ekobilet("kino-milenium", KinoMilenium),   // Milejów
      // MOK Świdnik's own page embeds a biletyna.pl "widget" iframe (ifid=22) —
      // an UNRELATED biletyna integration from Kino Lot Jelenia Góra, which is a
      // bilety24.pl subdomain instead. Distinct client because it's biletyna's
      // HTML widget template, not the JSON-LD place page BiletynaClient parses.
      new KinoLotSwidnikClient(http, KinoLotSwidnik),   // Świdnik
    ),
    "torun" -> Seq(
      helios(HeliosNuxt.Grudziadz),   // Grudziądz
    ),
    "wloclawek" -> Seq(
      bilety24("https://www.bilety24.pl/kino/organizator/rypinski-dom-kultury-1053", KinoBaltykRypin),   // Rypin
    ),
    "slupsk" -> Seq(
      bilety24("https://www.bilety24.pl/kino/organizator/miejska-biblioteka-publiczna-w-lebie-1722", KinoLeba),   // Łeba
    ),
    "legnica" -> Seq(
      biletyna(KinoMOKGlogow),   // Głogów
      biletyna(TeatrGryphius),   // Głogów
      multikino("0048", MultikinoGlogow),   // Głogów
      new KinoKulturaChojnowClient(http, KinoKulturaChojnow),   // Chojnów
    ),
    "piotrkow-trybunalski" -> Seq(
      biletyna(KinoMCKTkacz),   // Tomaszów Mazowiecki
      biletyna(KinoMDKOpoczno),   // Opoczno
    ),
    "chojnice" -> Seq(
      biletyna(KinoPromienWiecbork),   // Więcbork
      biletyna(KinoCKiSSepolno),   // Sępólno Krajeńskie
      biletyna(KinoGOKLipka),   // Lipka
      biletyna(KinoTucholskiOsrodekKultury),   // Tuchola
    ),
    "tychy" -> Seq(
      msi(KinoAleKinoLibiaz),   // Libiąż
    ),
    "ciechanow" -> Seq(
      biletyna(KinoNOKNasielsk),   // Nasielsk
      ekobilet("kinoton", KinoTon),   // Żuromin
    ),
    "rybnik" -> Seq(
      biletyna(KinoFeniks),   // Rydułtowy
    ),
    "radom" -> Seq(
      ekobilet("centrum-kultury-i-turystyki-w-ilzy-8211", KinoCKiTIlza),   // Iłża
      biletyna(KinoKsiazka),   // Stara Błotnica
      new KinoRCKDrzewicaClient(http, KinoRCKDrzewica),   // Drzewica
    ),
    "zlocieniec" -> Seq(
      biletyna(KinoChDKChoszczno),   // Choszczno
      biletyna(KinoDOKDrawno),   // Drawno
      biletyna(KinoMGOKRecz),   // Recz
      msi(KinoWolnoscSzczecinek),   // Szczecinek
    ),
    "pila" -> Seq(
      biletyna(KinoOKJastrowie),   // Jastrowie
    ),
    "wielun" -> Seq(
      msi(KinoWRatuszu),   // Zduńska Wola
    ),
    "czestochowa" -> Seq(
      msi(KinoPlanetCinemaZawiercie),   // Zawiercie
      new KinoMDKMyszkowClient(http, KinoMDKMyszkow),   // Myszków
    ),
    "ketrzyn" -> Seq(
      new KinoNowaFalaClient(http, KinoNowaFalaGizycko),   // Giżycko
    ),
    "przemysl" -> Seq(
      new SystemBiletowyClient(http, VisualSoftPortal("https://udk.systembiletowy.pl"), KinoOrzelUstrzyki, titles = titles),   // Ustrzyki Dolne
      systemBiletowy(KinoBieszczadzkiDK),   // Lesko
    ),
    "katowice" -> Seq(
      new KinoGrajfkaClient(http, KinoGrajfka),   // Chorzów
      // Kino Frajda (Starochorzowski Dom Kultury) shares its bilety.chck.pl
      // systembiletowy portal with Chorzowskie Centrum Kultury's own events —
      // filmGroups keeps only the "Imprezy SDK" rows, same pattern as
      // KinoBCKBytom below but with this venue's own data-group value.
      new SystemBiletowyClient(http, VisualSoftPortal("https://bilety.chck.pl"), KinoFrajda, titles = titles, filmGroups = Set(EventCategory("Imprezy SDK"))),   // Chorzów
    ),
    "ostrowiec-swietokrzyski" -> Seq(
      systemBiletowy(KinoKadrStaszow),   // Staszów
    ),
    "walbrzych" -> Seq(
      biletyna(KinoSCKStrzegom),   // Strzegom
    ),
    "bialystok" -> Seq(
      new DomKulturyLapyClient(http, DomKulturyLapy, today),   // Łapy
      new KinoBielskClient(http, KinoBielsk),   // Bielsk Podlaski
    ),
    "wyszkow" -> Seq(
      new KinoNarewClient(http, KinoNarew),   // Pułtusk
    ),
    "olsztyn" -> Seq(
      new KinoGrunwaldClient(http, KinoGrunwald, today),   // Olsztynek
    ),
    "wlodawa" -> Seq(
      new KinoParczewClient(http, KinoParczew),   // Parczew
    ),
    "leszno" -> Seq(
      ekobilet("dom-kultury-w-gorze-7114", KinoDKGora),   // Góra
      ekobilet("zpkwasosz", KinoZaciszeWasosz),   // Wąsosz
    ),
    "pulawy" -> Seq(
      ekobilet("ock-opolelubelskie", KinoOpolanka),   // Opole Lubelskie
    ),
    "zamosc" -> Seq(
      new KinoKadrTomaszowLubelskiClient(http, KinoKadrTomaszowLubelski, today),   // Tomaszów Lubelski
    ),
    "stalowa-wola" -> Seq(
      new KinoJOKClient(http, KinoJOK, today),   // Janów Lubelski
      systemBiletowy(KinoCKiBNowaSarzyna),   // Nowa Sarzyna
    ),
    "rzeszow" -> Seq(
      new KinoSokolStrzyzowClient(http, KinoSokolStrzyzow, today),   // Strzyżów
    ),
  )

  /** Per-city scrapers plus that city's catchment venues (Filmweb sweep + nearby towns). */
  val byCity: Map[String, Seq[CinemaScraper]] = {
    val wired = baseByCity.map { case (slug, scrapers) =>
      slug -> (scrapers ++ filmwebExtra.getOrElse(slug, Nil) ++ nearbyTowns.getOrElse(slug, Nil))
    } ++ nearbyTowns.filter { case (slug, _) => !baseByCity.contains(slug) }
    // A Polish scraper is filed under the PAGE its cinema is on, not the group it
    // is wired in above: which page lists which venue is data (`models.PolishPages`,
    // re-clustered by `data/pl/scripts/build_pages.py`), and a re-cluster must not
    // need a single scraper moved here to take effect.
    def polishPage(s: CinemaScraper): Option[City] = City.forCinema(s.cinema).filter(_.country == Country.Poland)
    val polish = wired.values.flatten.toSeq.filter(polishPage(_).isDefined)
    val others = wired.map { case (slug, scrapers) => slug -> scrapers.filterNot(polishPage(_).isDefined) }
      .filter { case (_, scrapers) => scrapers.nonEmpty }
    others ++ polish.groupBy(polishPage(_).get.slug)
  }

  /** Every raw scraper across every city, in city order. */
  val all: Seq[CinemaScraper] =
    City.all.flatMap(c => byCity.getOrElse(c.slug, Nil))

  /** UK chain venues (Cineworld / Vue / Showcase / Everyman / Odeon) whose own-site
   *  client is the catalogue's primary and that keep flicks.co.uk as their aggregator
   *  FALLBACK — the mirror of the Polish own-site→Filmweb arrangement. Maps each such
   *  cinema to the flicks slug it used to be catalogued under, so `WorkerWiring` can
   *  build the fallback `FlicksClient` on demand. Populated by the chain-wiring step;
   *  empty until then (behaviour identical to the pre-chain flicks-primary catalogue). */
  val flicksFallbackSlugs: Map[Cinema, ChainFlicksFallback.FlicksFallback] =
    ChainFlicksFallback.slugs ++ usFlicksFallback

  /** German venues' kinoprogramm.com pages — their showtime FALLBACK behind Filmstarts,
   *  harvested into the roster (`data/germany/kinoprogramm.json`). */
  val kinoprogrammFallbackPaths: Map[Cinema, String] = models.GermanRoster.kinoprogrammPathByCinema

  /** Union of every cinema scraper's HTTP hosts. `MonitoringHttpFetch`
   *  suppresses per-host uptime rows for these — each cinema's health is
   *  already tracked under its `displayName` by `UptimeRecordingScraper`, so a
   *  per-host row would be a duplicate landing in the uptime page's "Other"
   *  bucket. Single source of truth: a new cinema's client declares its host
   *  (forced by the abstract `CinemaScraper.scrapeHosts`) and is suppressed
   *  automatically — no hand-kept host list to drift. */
  val scrapeHosts: Set[String] = all.flatMap(_.scrapeHosts).toSet ++
    // A fallback feed is scraped for a venue too, but by no catalogue scraper.
    Option.when(kinoprogrammFallbackPaths.nonEmpty)(CinemaScraper.hostsOf(KinoprogrammClient.BaseUrl)).toSet.flatten
}
