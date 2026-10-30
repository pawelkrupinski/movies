package services.cinemas

import models._
import tools.HttpFetch
import services.cinemas.common.{CinemaScraper, MultiListingScraper}
import services.cinemas.pl._
import services.movies.TitleNormalizer

import java.time.LocalDate
import scala.concurrent.duration._

/**
 * Every Polish venue's scraper, as [[CinemaScraperCatalog]] files them: the chains' and the
 * ticketing platforms' builders, each platform's per-venue table, and one list of every venue —
 * grouped by the town it was first wired near, which is only where it is written down: the page
 * a venue is listed on is `models.PolishPages`, and the catalog files every scraper under that.
 */
final class PolishScraperCatalog(
  egress:           CinemaEgress,
  venueClock:       VenueClock,
  chainDetailCache: (String, HttpFetch, FiniteDuration) => HttpFetch,
  titles:           TitleNormalizer
) {
  private val http      = egress.direct
  private val mkFetch   = egress.multikino
  private val bnFetch   = egress.biletyna
  private val zyteFetch = egress.zyte

  /** Today in Poland, asked afresh at every read: each client takes it by name, so a
   *  scraper built at boot still plans from the day it scrapes on. */
  private def today: LocalDate = venueClock.todayInPoland

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
  // rosters. The list below keeps naming each venue in scrape order; only
  // what used to be a repeated constructor moved here.

  // MSI / VisualTicket portals (29). `fetch` is Zyte's residential egress for
  // the one venue whose origin firewall blocks both our Fly IP and the Decodo
  // proxy (see [[CinemaEgress.zyte]]); `mvcPath` / `titlePrefix` / `titleSuffix` /
  // `filmLabel` are the per-install quirks `MsiClient` documents.
  private case class MsiVenue(baseUrl:     String,
                              mvcPath:     String         = MsiClient.DefaultMvcPath,
                              titlePrefix: Option[String] = None,
                              titleSuffix: Option[String] = None,
                              filmLabel:   Option[String] = None,
                              fetch:       HttpFetch      = http)
  private val msiVenues: Map[Cinema, MsiVenue] = Map(
    KinoKryterium                -> MsiVenue(CinemaScraperCatalog.KinoKryteriumUrl, fetch = zyteFetch),
    KinoMillenium                -> MsiVenue("https://bilety.csm.tarnow.pl", mvcPath = "/Kino/mvc/pl"),
    KinoKinomax                  -> MsiVenue("https://bilety.kinomax.info.pl"),
    KinoCentrumSkarzyskoKamienna -> MsiVenue("https://bilet-mck.skarzysko.pl"),
    KinoWRatuszu                 -> MsiVenue("https://bilety.mdk-zdunskawola.pl"),
    KinoKozienickiDomKultury     -> MsiVenue("http://bilety.dkkozienice.pl"),
    KinoSwitZwolen               -> MsiVenue("https://bilety.switzwolen.pl"),
    KinoMCK                      -> MsiVenue("https://bilety.kinolezajsk.pl"),
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
    // Filmweb-only until 2026-09-27 (Iława's expired leaf is pinned in TlsTrust).
    KinoCinemaLumiereSuwalki -> MsiVenue("https://suwalki.cinema-lumiere.pl"),
    KinoOstrovia             -> MsiVenue("https://bilety.kinoostrovia.pl"),
    KinoTeatrSieradz         -> MsiVenue("https://bilety.scksieradz.pl", filmLabel = Some("FILM")),
    KinoChDK                 -> MsiVenue("https://bilety.ckchojnice.pl"),
    KinoteatrPasja           -> MsiVenue("https://biletyick.miastoilawa.pl"),
    KinoGornikLeczyca        -> MsiVenue("https://kinogornik.eurobilet.pl"),
    GoKinoOlawa              -> MsiVenue("https://olawa.gokino.pl"),
    KinoGrazyna              -> MsiVenue("https://bilety.kinostrzelin.pl"),
  )
  private def msi(cinema: Cinema): MsiClient = {
    val venue = msiVenues(cinema)
    new MsiClient(venue.fetch, venue.baseUrl, cinema, today,
      mvcPath = venue.mvcPath, titlePrefix = venue.titlePrefix, titleSuffix = venue.titleSuffix,
      filmLabel = venue.filmLabel)
  }

  // biletyna.pl venue pages (plus Końskie's two below). biletyna.pl 403s our datacenter IP (Cloudflare
  // waiting-room), so every one routes through `bnFetch` — the residential
  // proxy in prod, the fixture fake in tests.
  private val biletynaPages: Map[Cinema, String] = Map(
    KinoMCKTkacz -> "https://biletyna.pl/Tomaszow-Mazowiecki/Miejskie-Centrum-Kultury-Filia-Tkacz",
    KinoMDKOpoczno -> "https://biletyna.pl/Opoczno/Miejski-Dom-Kultury-im-Tadeusza-Sygietynskiego",
    KinoDomSztukiUrsynow -> "https://biletyna.pl/Warszawa/Kino-Domu-Sztuki-Ursynow",
    KinoTerminalKultury -> "https://biletyna.pl/Warszawa/Terminal-Kultury",
    KinoBielanskiOsrodekKultury -> "https://biletyna.pl/Warszawa/Bielanski-Osrodek-Kultury",
    KinoWawerskaStrefaKultury -> "https://biletyna.pl/Warszawa/Wawerska-Strefa-Kultury-sala-widowiskowa",
    KinoBasn -> "https://biletyna.pl/Piastow/sala-widowiskowa",
    KinoKasynoOficerskie -> "https://biletyna.pl/Nowy-Dwor-Mazowiecki/Kasyno-Oficerskie-Sala-kinowa",
    KinoGrojeckiOsrodekKultury -> "https://biletyna.pl/Grojec/Grojecki-Osrodek-Kultury",
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
    // Filmweb (3137 / 1532) had gone empty for both by 2026-09-27 while each kept
    // selling its film nights on biletyna. CKiS Pruszków screens in the Centrum
    // Dziedzictwa Kulturowego hall; Grajewo's "Dom Kultury" is the Grajewskie
    // Centrum Kultury, whose page mixes films with concerts and plays.
    KinoCKiSPruszkow            -> "https://biletyna.pl/Pruszkow/Sala-widowiskowa-Centrum-Dziedzictwa-Kulturowego",
    KinoDomKulturyGrajewo       -> "https://biletyna.pl/Grajewo/Grajewskie-Centrum-Kultury",
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
    // Filmweb-only until 2026-09-27; each venue's own box office listed more
    // (usually twice the showtimes over the same fortnight) — see the commit.
    KinoWKadrze         -> "https://biletyna.pl/Warszawa/Dom-Kultury-Kadr",
    KinoKongres         -> "https://biletyna.pl/Wegrow/Wegrowski-Osrodek-Kultury",
    KinoRodlo           -> "https://biletyna.pl/Zlotow/Zlotowskie-Centrum-Aktywnosci-Spolecznej",
    KinoOOK             -> "https://biletyna.pl/Opatow/Opatowski-Osrodek-Kultury",
    KinoECK             -> "https://biletyna.pl/Elk/Elckie-Centrum-Kultury",
    KinoUciechaCzluchow -> "https://biletyna.pl/Czluchow/Kino-Uciecha",
    KinoSokolZakopane   -> "https://biletyna.pl/Zakopane/Kino-Sokol",
    KinoMiejsce         -> "https://biletyna.pl/Zakopane/Kino-Miejsce",
    KinoDobrychFilmow   -> "https://biletyna.pl/Wyszkow/Wyszkowski-Osrodek-Kultury-Hutnik",
    KinoMiGOK           -> "https://biletyna.pl/Lochow/Miejski-i-Gminny-Osrodek-Kultury",
    KinoMOKPolice       -> "https://biletyna.pl/Police/Miejski-Osrodek-Kultury",
    KinoUciecha         -> "https://biletyna.pl/Gora-Kalwaria/Kino-Uciecha",
    KinoOaza            -> "https://biletyna.pl/Otwock/Kino-Oaza-Otwockie-Centrum-Kultury",
    KinoZbyszekOlkusz   -> "https://biletyna.pl/Olkusz/Kino-Zbyszek",
    KinoZdrojBusko      -> "https://biletyna.pl/Busko-Zdroj/BSCK",
    KinoMuzaWloszczowa  -> "https://biletyna.pl/Wloszczowa/Dom-Kultury-we-Wloszczowie",
    KinoRelaks          -> "https://biletyna.pl/Naklo-nad-Notecia/Nakielski-Osrodek-Kultury",
    KinoMorskieOko      -> "https://biletyna.pl/Krasnystaw/Kino-Morskie-Oko",
    KinoKosmosMlawa     -> "https://biletyna.pl/Mlawa/Miejski-Dom-Kultury-kino-Kosmos",
    // One screen Filmweb split across two ids (1658 "Kino Sława", 1850 "Kino za
    // Rogiem", the room's older name), sharing slots; biletyna sells all of it.
    KinoSlawa           -> "https://biletyna.pl/Miedzyrzec-Podlaski/Kino-Slawa",
    // The site's own repertoire is prose; its "Kup online" widget is this page.
    KinoPromienRawicz   -> "https://biletyna.pl/Rawicz/Kino-Promien-w-Rawiczu",
  )
  private def biletyna(cinema: Cinema): BiletynaClient =
    new BiletynaClient(bnFetch, BiletynaPlacePage(biletynaPages(cinema)), cinema, nationalFeed = Some(biletynaNationalFeed))

  // Końskie's two halls, each on a biletyna page of its own (see below).
  private val koneckieHalls: Seq[(BiletynaPlacePage, String)] = Seq(
    BiletynaPlacePage("https://biletyna.pl/Konskie/Koneckie-Centrum-Kultury-sala-kinowa")      -> "sala kinowa",
    BiletynaPlacePage("https://biletyna.pl/Konskie/Koneckie-Centrum-Kultury-sala-widowiskowa") -> "sala widowiskowa",
  )

  // One read of biletyna's national event feed serves every venue above: ~7
  // proxied requests a pass instead of one (or more) per venue.
  private lazy val biletynaNationalFeed: BiletynaNationalFeed =
    new BiletynaNationalFeed(bnFetch, biletynaPages.values.map(BiletynaPlacePage(_)).toSet ++ koneckieHalls.map(_._1))

  // Końskie's culture centre lists each hall on a biletyna page of its own. The
  // stage hall is mostly cabaret and concerts but does screen the odd film
  // ("Popiełuszko - wolność jest w nas", 2026-10-20), so both halls are ONE
  // cinema, each showtime tagged with its hall.
  private val koneckieCentrumKultury: CinemaScraper = new MultiListingScraper(KinoKoneckieCentrumKultury,
    koneckieHalls.map { case (page, hall) =>
      new BiletynaClient(bnFetch, page, KinoKoneckieCentrumKultury, room = Some(hall), nationalFeed = Some(biletynaNationalFeed))
    })

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
    KinoWilga         -> "https://grl.systembiletowy.pl",
    KinoFenix         -> "https://lok.systembiletowy.pl",
    KinoCKiF          -> "https://bilety.ckifsucha.pl",
    KinoMuzaMyslenice -> "https://bilety.kino.myslenice.pl",
    KinoGryf          -> "https://bilety.ckis.miechow.eu",
    KinoCinemaN       -> "https://bilety.kino.nysa.pl",
    KinoJDK           -> "https://jdk.systembiletowy.pl",
    KinoBCKBytom      -> "https://bck.systembiletowy.pl",
    KinoFrajda        -> "https://bilety.chck.pl",
  )
  // Instances that also sell someone else's events, scoped to the venue's own
  // `location.institution_name` (see SystemBiletowyClient).
  private val systemBiletowyInstitutions: Map[Cinema, Institution] = Map(
    KinoNaszeKino  -> Institution("Nasze Kino"),   // Oświęcim's culture centre sells its concerts on the same instance
    KinoMikro      -> Institution("Kino Mikro"),   // the two Mikro screens share one instance
    MikroBronowice -> Institution("Mikro Bronowice"),
  )
  // Instances whose feed mixes the venue's films with its stage programme, kept
  // to the `event.category` the films are filed under (see SystemBiletowyClient).
  private val systemBiletowyFilmGroups: Map[Cinema, EventCategory] = Map(
    KinoBCKBytom -> EventCategory("BCKino"),
    KinoFrajda   -> EventCategory("Imprezy SDK"),
    KinoCKiF     -> EventCategory("Kino"),   // Sucha Beskidzka also sells its cabaret and musicals here
  )
  private def systemBiletowy(cinema: Cinema): SystemBiletowyClient =
    new SystemBiletowyClient(http, VisualSoftPortal(systemBiletowyPortals(cinema)), cinema, titles = titles,
      filmGroups = systemBiletowyFilmGroups.get(cinema).toSet, institution = systemBiletowyInstitutions.get(cinema))

  // Venues on their own `<venue>.bilety24.pl` subdomain (8) — as opposed to the
  // organiser listings on bilety24.pl itself that `bilety24` above builds.
  private val bilety24Subdomains: Map[Cinema, String] = Map(
    KinoLuna            -> "https://kinoluna.bilety24.pl",
    KinoteatrRialto     -> "https://kinoteatrrialto.bilety24.pl",
    KinoCkLublin        -> "https://ck-lublin.bilety24.pl",
    KinoApolloWalbrzych -> "https://kino-apollo.bilety24.pl",
    KinoPiast           -> "https://kino-piast.bilety24.pl",
    KinoLot             -> "https://kino-lot.bilety24.pl",
    KinoOskard          -> "https://ckis-konin.bilety24.pl",
    KinoSniezka         -> "https://mokdebica.bilety24.pl",   // Dębica; left MSI 2026-09
  )
  private def bilety24Subdomain(cinema: Cinema): Bilety24Client =
    new Bilety24Client(http, bilety24Subdomains(cinema), cinema, titles = titles)

  // Shared per-source helper clients the scrapers below reuse.
  private val cinemaCityClient: CinemaCityClient = new CinemaCityClient(http, Some(cinemaCityDetailHttp), titles = titles)
  // One per Cinema City venue.
  private def cinemaCity(cinemaId: String, cinema: Cinema): CinemaCityScraper =
    new CinemaCityScraper(cinemaCityClient, cinemaId, cinema)
  // One per Multikino venue — 31 of them, so the shared `mkFetch` + `titles` are
  // bound here rather than repeated at every site. Defaults mirror
  // `MultikinoClient`'s: Poznań's Stary Browar.
  private def multikino(cinemaId: String = MultikinoClient.PoznanStaryBrowarId,
                        cinema:   Cinema = Multikino): MultikinoClient =
    new MultikinoClient(mkFetch, cinemaId, cinema, titles)
  private val kinoMuzaClient: KinoMuzaClient   = new KinoMuzaClient(http, today, titles = titles)

  /** Every Polish venue's scraper. Each `// ──` group is the town the venues were wired near; the groups stand in the
   *  order the catalog's former per-town map iterated them in, which is each page's scrape order — so moving a group
   *  reorders the scrapers of every page it shares. */
  val scrapers: Seq[CinemaScraper] = Seq(
    // ── katowice
    cinemaCity("1065", CinemaCityPunkt44),
    cinemaCity("1079", CinemaCitySilesia),
    multikino("0035", MultikinoKatowice),
    helios(HeliosNuxt.Katowice),
    // Silesia Film's art-house trio, all Bilety24-hosted: listing at `/repertuar/`
    // linking per-film `/wydarzenie/?id=N` pages, so they reuse the shared Bilety24Client.
    bilety24("https://www.bilety24.pl/kino/organizator/kino-kosmos-1501", KinoKosmos),
    bilety24("https://www.bilety24.pl/kino/organizator/kino-swiatowid-1503", KinoSwiatowid),
    bilety24Subdomain(KinoteatrRialto),
    // ── jelenia-gora
    helios(HeliosNuxt.JeleniaGora), bilety24Subdomain(KinoLot),
    // ── lomza
    helios(HeliosNuxt.Lomza),   // Łomża
    bilety24("https://www.bilety24.pl/kino/organizator/ostroleckie-centrum-kultury-1068", KinoJantar),   // Ostrołęka
    new ModernEventsCalendarClient(http, ModernEventsCalendarPage("https://kino.mokzambrow.pl/"), KinoCKZambrow, today),   // Zambrów
    msi(KinoOstrovia),   // Ostrów Mazowiecka
    new KinoWarsClient(http, KinoWars),   // Wysokie Mazowieckie
    biletyna(KinoDomKulturyGrajewo),   // Grajewo
    // ── warszawa
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
    biletyna(KinoWKadrze),
    // ── gorzow-wielkopolski
    helios(HeliosNuxt.Gorzow), multikino("0047", MultikinoGorzow), bilety24("https://www.bilety24.pl/kino/organizator/miejski-osrodek-sztuki-kino-60-krzesel-dkf-megaron-776", Kino60Krzesel),
    // ── siedlce
    helios(HeliosNuxt.Siedlce),   // Siedlce
    noveKino("siedlce", NoveKinoSiedlce),   // Siedlce
    filmweb(2073, KinoSokolSokolowPodlaski),   // Sokołów Podlaski
    biletyna(KinoSlawa),   // Międzyrzec Podlaski
    biletyna(KinoKongres),   // Węgrów
    new KinoLukowClient(http, KinoLukow),   // Łuków
    cinemaCity("1100", CinemaCityBialaPodlaska),   // Biała Podlaska
    systemBiletowy(KinoWilga),   // Garwolin
    // ── olsztyn
    // ── New mid-size cities ─────────────────────────────────────────────────────
    // Chain ids verified against each chain's own cinema-list API. Each city's
    // independent screen, where one has a machine-readable source, is added via a
    // shared platform client: FilmwebShowtimesClient (Filmweb's seances JSON, by
    // internal cinema id — verified to return non-empty seances), Bilety24Client
    // (a bilety24.pl venue), or NoveKinoClient.
    helios(HeliosNuxt.Olsztyn), multikino("0036", MultikinoOlsztyn), new KinoAwangarda2Client(http, today),
    // ── kielce
    helios(HeliosNuxt.Kielce),
    multikino("0029", MultikinoKielce),
    // iframe639.biletyna.pl 403s our Fly datacenter IP (Cloudflare) on the
    // per-film /artist/view/id detail pages, so route through the biletyna seam
    // (residential proxy → direct) like every other biletyna venue — else the
    // deferred detail enrichment fetches all 403 and the enrichment bar goes red.
    new KinoFenomenClient(bnFetch, KinoFenomen),
    new KinoMoskwaClient(http, KinoMoskwa, today),
    // ── bialystok
    helios(HeliosNuxt.Alfa),
    helios(HeliosNuxt.Biala),
    helios(HeliosNuxt.Jurowiecka),
    new KinoForumClient(http, today),
    // ── konin
    // Konin + its catchment: Helios via the chain client, and each independent off
    // its own ticketing listing (bilety24 organiser / biletyna), Filmweb only where
    // it has none. Neighbouring bilety24 organiser ids are different towns: 1626 is
    // Konin's culture centre, 1621 Koło's MDK. Września's Kino Trójka (1698) is
    // intentionally not wired.
    helios(HeliosNuxt.Konin),
    bilety24Subdomain(KinoOskard),
    filmweb(2405, KinoZacheta),   // Kleczew
    bilety24("https://www.bilety24.pl/kino/organizator/koninskie-centrum-kultury-1626", KinoStudyjneCentrum),   // Konin
    bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-w-kole-1621", KinoNadWarta),   // Koło
    bilety24("https://www.bilety24.pl/kino/organizator/zajezdnia-kultury-w-pleszewie-1255", KinoHel),   // Pleszew
    bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-w-slupcy-1423", KinoSokolnia),   // Słupca
    biletyna(KinoTur),   // Turek
    biletyna(KinoMok),   // Zagórów
    // ── bydgoszcz
    cinemaCity("1086", CinemaCityBydgoszcz),
    multikino("0006", MultikinoBydgoszcz),
    helios(HeliosNuxt.Bydgoszcz),
    new KinoOrzelClient(http, KinoOrzel),
    // ── walbrzych
    cinemaCity("1091", CinemaCityWalbrzych), bilety24Subdomain(KinoApolloWalbrzych),
    // ── poznan
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
    // ── legnica
    helios(HeliosNuxt.Legnica), bilety24Subdomain(KinoPiast),
    // ── zielona-gora
    cinemaCity("1087", CinemaCityZielonaGora),
    // ── lublin
    cinemaCity("1094", CinemaCityLublinFelicity),
    cinemaCity("1084", CinemaCityLublinPlaza),
    multikino("0034", MultikinoLublin),
    new KinoBajkaClient(http, KinoBajka, titles = titles),
    bilety24Subdomain(KinoCkLublin),
    new KinoChatkaZakaClient(http, KinoChatkaZaka),
    // ── koszalin
    helios(HeliosNuxt.Koszalin), multikino("0015", MultikinoKoszalin), msi(KinoKryterium),
    // ── bytom
    // BCKino sells theatre/workshops/concerts through the same listing as its
    // films, tagged with a `data-group` per event ("BCKino" for films) —
    // `filmGroups` keeps only those and peels the resulting "BCKino – " title
    // prefix. Verified screening 2026-09-23.
    cinemaCity("1092", CinemaCityBytom),
    systemBiletowy(KinoBCKBytom),
    // ── wyszkow
    biletyna(KinoDobrychFilmow),   // Wyszków
    biletyna(KinoMiGOK),   // Łochów
    // ── dabrowa-gornicza
    helios(HeliosNuxt.DabrowaGornicza), new VisualTicketClient(http, "https://bilety.palac.art.pl", KinoKadr, locationId = 2),
    // ── przemysl
    helios(HeliosNuxt.Przemysl),
    // ── stalowa-wola
    helios(HeliosNuxt.StalowaWola),   // Stalowa Wola
    bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-stalowa-wola-1419", KinoWrzos),   // Stalowa Wola
    new ModernEventsCalendarClient(http, ModernEventsCalendarPage("https://nck.nisko.pl/kino-sokol/repertuar/"), KinoSokolNisko, today),   // Nisko
    new KinoMetalowiecNowaDebaClient(http, KinoMetalowiecNowaDeba, today),   // Nowa Dęba
    new IksorisBookingClient(http, IksorisBookingPage(IksorisOrigin("https://bilet.bck.lbl.pl")), KinoBCK),   // Biłgoraj
    // ── starogard-gdanski
    cinemaCity("1095", CinemaCityStarogard),   // Starogard Gdański
    new KinoRemusClient(http, KinoRemus),   // Kościerzyna
    // ── radom
    helios(HeliosNuxt.Radom),
    multikino("0026", MultikinoRadom),
    new McswElektrowniaCinemaClient(http, McswElektrowniaCinema, today),
    // ── piotrkow-trybunalski
    // ── Regional hubs (2026-09 sweep) ───────────────────────────────────────────
    // Smaller towns grouped around their largest one, every venue about an hour's
    // drive from it — the shape Konin already had for Turek, Koło and Słupca.
    helios(HeliosNuxt.PiotrkowTrybunalski),   // Piotrków Trybunalski
    helios(HeliosNuxt.Belchatow),   // Bełchatów
    new IksorisBookingClient(http, IksorisBookingPage(IksorisOrigin("https://bilety.mckbelchatow.pl")), KinoKulturaBelchatow),   // Bełchatów
    helios(HeliosNuxt.TomaszowMazowiecki),   // Tomaszów Mazowiecki
    // ── nowy-sacz
    helios(HeliosNuxt.NowySacz), bilety24("https://www.bilety24.pl/kino/organizator/malopolskie-centrum-kultury-sokol-w-nowym-saczu-1225", KinoSokol),
    // ── krakow
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
    // ── gniezno
    helios(HeliosNuxt.Gniezno),   // Gniezno
    bilety24("https://www.bilety24.pl/kino/organizator/mogilenski-dom-kultury-kino-wawrzyn-1644", KinoWawrzyn),   // Mogilno
    bilety24("https://www.bilety24.pl/kino/organizator/biblioteka-publiczna-im-czeslawa-chruszczewskiego-1294", KinoMiescisko),   // Mieścisko
    // ── rybnik
    multikino("0014", MultikinoRybnik), cinemaCity("1082", CinemaCityRybnik),
    // ── tarnow
    multikino("0050", MultikinoTarnow), msi(KinoMillenium), // LiteSpeed reCAPTCHA-gates a browser UA from our Hetzner IP (since 2026-09-30), not a Decodo IP
    new KinoMarzenieClient(bnFetch, KinoMarzenie, today),
    // ── wroclaw
    cinemaCity("1097", CinemaCityWroclavia),
    cinemaCity("1067", CinemaCityKorona),
    multikino("0010", MultikinoPasazGrunwaldzki),
    helios(HeliosNuxt.Magnolia),
    helios(HeliosNuxt.AlejaBielany),
    new NoweHoryzontyClient(http, today),
    new DcfClient(http),
    // ── opole
    helios(HeliosNuxt.OpoleKarolinka), helios(HeliosNuxt.OpoleSolaris), ekobilet("opolskielamy", KinoMeduza),
    // ── zlocieniec
    bilety24("https://www.bilety24.pl/kino/organizator/zlocieniecki-osrodek-kultury-1710", KinoMewaZlocieniec),   // Złocieniec
    bilety24("https://www.bilety24.pl/kino/organizator/osrodek-kultury-w-drawsku-pomorskim-1680", KinoOKDrawsko),   // Drawsko Pomorskie
    // ── pulawy
    noveKino("sybilla", KinoSybilla),   // Puławy
    ekobilet("kino-renesans", KinoRenesans),   // Ryki
    // ── zamosc
    new KinoStylowyClient(http, KinoStylowy, today),   // Zamość
    bilety24("https://www.bilety24.pl/kino/organizator/chelmski-dom-kultury-1718", KinoZorzaChelm),   // Chełm
    biletyna(KinoMorskieOko),   // Krasnystaw
    new IksorisRepertoireClient(http, IksorisOrigin("https://kinoplon.pl"), KinoPlon, today),   // Hrubieszów
    // ── slupsk
    multikino("0030", MultikinoSlupsk), ekobilet("kinorejs", KinoRejs),
    // ── elblag
    multikino("0037", MultikinoElblag), cinemaCity("1099", CinemaCityElblag),
    // ── wielun
    filmweb(2034, KinoSyrena),   // Wieluń
    msi(KinoTeatrSieradz),   // Sieradz
    filmweb(2334, KinoSlonce),   // Wieruszów
    new IksorisRepertoireClient(http, IksorisOrigin("https://kinosokolnia.org"), KinoSokolniaKepno, today),   // Kępno
    // ── slubice
    // Two towns with one cinema each and nothing else within an hour's drive —
    // pages of their own rather than a stretch onto the nearest region.
    bilety24("https://www.bilety24.pl/kino/organizator/slubicki-miejski-osrodek-kultury-1336", KinoSmok),
    // ── leszno
    multikino("0044", MultikinoLeszno),   // Leszno
    bilety24("https://www.bilety24.pl/kino/organizator/teatr-miejski-w-lesznie-1653", KinoTeatrMiejskiLeszno),   // Leszno
    biletyna(KinoPromienRawicz),   // Rawicz
    bilety24("https://www.bilety24.pl/kino/organizator/gostynski-osrodek-kultury-hutnik-z-siedziba-w-gostyniu-679", KinoPodKopula),   // Gostyń
    bilety24("https://www.bilety24.pl/kino/organizator/biblioteka-i-centrum-kultury-rekreacji-im-jana-z-domachowa-bzdegi-w-krobi-1643", KinoKrobia),   // Krobia
    // ── pila
    helios(HeliosNuxt.Pila),   // Piła
    bilety24("https://www.bilety24.pl/kino/organizator/waleckie-centrum-kultury-1598", KinoWCKWalcz),   // Wałcz
    bilety24("https://www.bilety24.pl/kino/organizator/kino-osiedlowe-trzcianka-1559", KinoOsiedlowe),   // Trzcianka
    biletyna(KinoRodlo),   // Złotów
    bilety24("https://www.bilety24.pl/kino/organizator/gminny-osrodek-kultury-685", KinoMewaBudzyn),   // Budzyń
    bilety24("https://www.bilety24.pl/kino/organizator/chodzieski-dom-kultury-591", KinoNotec),   // Chodzież
    bilety24("https://www.bilety24.pl/kino/organizator/nck-w-wieleniu-1344", KinoZaRogiemWielen),   // Wieleń
    bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-623", KinoMDKWagrowiec),   // Wągrowiec
    bilety24("https://www.bilety24.pl/kino/organizator/mck-kino-swiatowid-831", KinoSwiatowidCzarnkow),   // Czarnków
    // ── chojnice
    msi(KinoChDK),   // Chojnice
    biletyna(KinoUciechaCzluchow),   // Człuchów
    bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-i-biblioteki-im-jana-karnowskiego-1410", KinoZaRogiemBrusy),   // Brusy
    // ── szczecin
    helios(HeliosNuxt.Szczecin),
    multikino("0007", MultikinoSzczecin),
    new PionierClient(http, KinoPionier),
    helios(HeliosNuxt.SzczecinOutletPark),
    new KinoZamekClient(http, KinoZamekSzczecin, today),
    // ── gliwice
    cinemaCity("1085", CinemaCityGliwice),
    new KinoAmokClient(http, KinoAmok, today),
    // ── ostrowiec-swietokrzyski
    bilety24("https://www.bilety24.pl/kino/organizator/miejskie-centrum-kultury-w-ostrowcu-swietokrzyskim-1389", KinoEtiuda),   // Ostrowiec Świętokrzyski
    biletyna(KinoOOK),   // Opatów
    ekobilet("kinostarowka", KinoStarowka),   // Sandomierz
    // ── wloclawek
    multikino("0008", MultikinoWloclawek),
    // ── skierniewice
    biletyna(KinoPolonez),   // Skierniewice
    biletyna(KinoCKiSSkierniewice),   // Skierniewice
    biletyna(KinoLen),   // Żyrardów
    new KinoRomaRawaClient(http, KinoRomaRawa, today),   // Rawa Mazowiecka
    systemBiletowy(KinoFenix),   // Łowicz
    // ── zakopane
    biletyna(KinoSokolZakopane),   // Zakopane
    biletyna(KinoGiewont),   // Zakopane
    biletyna(KinoMiejsce),   // Zakopane
    bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-sportu-i-promocji-w-rabce-zdroju-625", KinoSniezkaRabka),   // Rabka-Zdrój
    msi(KinoStopiak),   // Nowy Targ — own site links bilety.stopiakcinema.pl, a standard MSI portal
    // ── bielsko-biala
    helios(HeliosNuxt.BielskoBiala), cinemaCity("1088", CinemaCityBielskoBiala), new KinoKreskaClient(http, KinoKreska),
    // ── ketrzyn
    new KinoGwiazdaKetrzynClient(http, KinoGwiazdaKetrzyn),   // Kętrzyn
    ekobilet("mck-mragowo", KinoZodiak),   // Mrągowo
    // ── ciechanow
    bilety24("https://www.bilety24.pl/kino/organizator/powiatowe-centrum-kultury-i-sztuki-im-marii-konopnickiej-1307", KinoLydynia),   // Ciechanów
    biletyna(KinoKosmosMlawa),   // Mława
    bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-im-stanislawa-ostoi-kotkowskiego-w-przasnyszu-1482", KinoSwiatowidPrzasnysz),   // Przasnysz
    bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-w-makowie-mazowieckim-1485", KinoMDKMakow),   // Maków Mazowiecki
    biletyna(KinoApolloDzialdowo),   // Działdowo
    // ── kalisz
    helios(HeliosNuxt.Kalisz), multikino("0042", MultikinoKalisz),
    // ── wlodawa
    filmweb(2312, KinoWDK),
    // ── sosnowiec
    helios(HeliosNuxt.Sosnowiec),
    cinemaCity("1083", CinemaCitySosnowiec),
    // ── rzeszow
    helios(HeliosNuxt.Rzeszow),
    multikino("0028", MultikinoRzeszow),
    new KinoZorzaClient(http, KinoZorza, today),
    new KinoZaRogiemCafeClient(http, KinoZaRogiemCafe, today),
    // ── plock
    helios(HeliosNuxt.Plock), noveKino("przedwiosnie", KinoPrzedwiosnie),
    // ── zgorzelec
    multikino("0031", MultikinoZgorzelec),   // Zgorzelec
    bilety24("https://www.bilety24.pl/kino/organizator/bogatynski-osrodek-kultury-1659", KinoKadrBogatynia),   // Bogatynia
    // ── trojmiasto
    multikino("0004", MultikinoGdansk),
    helios(HeliosNuxt.Metropolia),
    helios(HeliosNuxt.Forum),
    helios(HeliosNuxt.Riviera),
    new KinoSpektrumClient(http, KinoSpektrum),
    // biletyna.pl 403s our datacenter IP, so route through `bnFetch` — the
    // residential proxy in production, the fixture fake in
    // tests. See EgressWiring.biletynaFetch.
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
    // ── ilawa
    msi(KinoteatrPasja),   // Iława
    ekobilet("mok-lubawa", KinoPokoj),   // Lubawa
    bilety24("https://www.bilety24.pl/kino/organizator/miejskie-centrum-kultury-1291", KinoteatrHarmonia),   // Nowe Miasto Lubawskie
    // ── tychy
    multikino("0053", MultikinoTychy),
    // ── torun
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
    // ── lodz
    cinemaCity("1080", CinemaCityManufaktura),
    multikino("0023", MultikinoLodz),
    helios(HeliosNuxt.Lodz),
    new CharlieClient(http, KinoCharlie),
    new KinematografLodzClient(http, KinematografLodz, today, titles = titles),
    new NckfClient(http, Nckf, today),
    new KinoTatryClient(http, KinoTatry, today),
    // ── czestochowa
    cinemaCity("1089", CinemaCityCzestochowaJurajska),
    cinemaCity("1075", CinemaCityCzestochowaWolnosc),
    new OkfIluzjaClient(http, OkfIluzja),
    // ── suwalki
    msi(KinoCinemaLumiereSuwalki),   // Suwałki
    new KinoIskraClient(http, KinoIskra, today),   // Augustów
    bilety24("https://www.bilety24.pl/kino/organizator/dom-kultury-w-goldapi-1089", KinoKulturaGoldap),   // Gołdap
    ekobilet("mazury-olecko", KinoMazur),   // Olecko
    biletyna(KinoECK),   // Ełk
    msi(KinoPlanetCinemaElk),   // Ełk
    // ── zabrze
    multikino("0003", MultikinoZabrze),
    new KinoRomaClient(http, KinoRoma, today),
    msi(KinoPlanetCinemaZabrze),
    // ── Filmweb sweep catchments
    // Catchment cinemas (nearby towns) and a few in-city venues that came in via a
    // Filmweb sweep, each by Filmweb internal cinema id — except the handful that
    // have since moved to their own site (Kino Spójnia, Kino Praha), which fed
    // Filmweb too thinly.
    // jelenia-gora
    bilety24("https://www.bilety24.pl/kino/organizator/kino-wawel-1489", KinoWawel),
    // warszawa
    new PrahaClient(http, KinoMazowieckiTeatrMuzycznyImJanaKiepuryKinoPraha, today),
    // olsztyn
    msi(KinoCinemaLumiere), msi(KinoIgnacy), bilety24("https://www.bilety24.pl/kino/organizator/moraski-dom-kultury-1682", KinoNarie),
    // kielce
    bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-w-jedrzejowie-1458", KinoCK), koneckieCentrumKultury,
    // bialystok
    new KinoSokolSokolkaClient(http, KinoSokolSokolka),
    // koszalin
    msi(KinoBajkaDarlowo), bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-i-spotkan-europejskich-w-bialogardzie-1685", KinoCentrumBialogard), biletyna(KinoDK), msi(KinoGOK), msi(KinoGoplana), msi(KinoWybrzeze),
    // przemysl
    new KinoCentrum3DPrzemyslClient(http, KinoCentrum3DPrzemysl), msi(KinoIkar), msi(KinoNaBiegunach), new KinoSDKSanokClient(http, KinoSDK, today),
    // radom
    helios(HeliosNuxt.Starachowice), msi(KinoCentrumSkarzyskoKamienna), bilety24("https://www.bilety24.pl/kino/organizator/szydlowieckie-centrum-kultury-zamek-1320", KinoGornik), msi(KinoKozienickiDomKultury), systemBiletowy(KinoKuznica), msi(KinoSwitZwolen),
    // nowy-sacz
    ekobilet("kino-jaworzyna", KinoJaworzyna), bilety24("https://www.bilety24.pl/kino/organizator/limanowski-dom-kultury-1368", KinoKlaps),
    // katowice
    cinemaCity("1062", CinemaCity), new KinoPatriaClient(http, KinoPatria, today),
    // opole
    helios(HeliosNuxt.KedzierzynKozle), bilety24("https://www.bilety24.pl/kino/organizator/centrum-aktywnosci-lokalnej-w-kluczborku-kino-bajka-1467", KinoBajkaKluczbork), msi(KinoChemik), new KinoDianaClient(http, KinoDiana), new KdkKrapkowiceClient(http, KinoKrapkowice), new KinoStudioClient(http, KinoStudio, today), msi(KinoTwierdza),
    // kalisz
    helios(HeliosNuxt.OstrowWielkopolski), systemBiletowy(KinoCentrum3D), bilety24("https://www.bilety24.pl/kino/organizator/kino-echo-1159", KinoEcho), bilety24("https://www.bilety24.pl/kino/organizator/ostrzeszowskie-centrum-kultury-601", KinoPiastOstrzeszow), bilety24("https://www.bilety24.pl/kino/organizator/krotoszynski-osrodek-kultury-1668", KinoPrzedwiosnieKrotoszyn),
    // legnica
    helios(HeliosNuxt.Lubin), new KinoAurumClient(http, KinoAurum), new CyfroweKinoClient(http, KinoCyfroweKino, titles = titles), bilety24("https://www.bilety24.pl/kino/organizator/boleslawiecki-osrodek-kultury-miedzynarodowe-centrum-ceramiki-w-boleslawcu-1586", KinoForumBoleslawiec), bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-muza-w-lubinie-dolny-slask-1375", KinoMuzaLubin), bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-w-polkowicach-1689", KinoPCA),
    // bydgoszcz
    msi(KinoKinomax), biletyna(KinoRondo),
    // walbrzych
    msi(KinoMOKNowaRuda), ekobilet("mokis-bielawa", KinoMOKiS), new KinoSlezaClient(http, KinoSleza), new KinoZbyszekClient(http, KinoZbyszek), multikino("0041", MultikinoKlodzko), multikino("0043", MultikinoSwidnica),
    // zielona-gora
    bilety24("https://www.bilety24.pl/kino/organizator/nowosolski-dom-kultury-1679", KinoEuropa), msi(KinoMaxKino), bilety24("https://www.bilety24.pl/kino/organizator/kino-pionier-1492", KinoPionierZary), msi(KinoSDKSwiebodzin),
    // lublin
    bilety24("https://www.bilety24.pl/kino/organizator/lubartowski-osrodek-kultury-1382", KinoLewart), bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-i-promocji-w-krasniku-1529", KinoMetalowiec),
    // rybnik
    helios(HeliosNuxt.Zory), bilety24("https://www.bilety24.pl/kino/organizator/kino-baltyk-1499", KinoBaltyk), ekobilet("kino-centrum-jastrzebiezdrj", KinoCentrum), systemBiletowy(KinoNaStarowce), biletyna(KinoPegaz), new TeatrZiemiRybnickiejClient(http),
    // tarnow
    systemBiletowy(KinoFarys), biletyna(KinoGCK), bilety24("https://www.bilety24.pl/kino/organizator/gorlickie-centrum-kultury-1581", KinoKolory), msi(KinoPlaneta), new KinoPromienClient(http, KinoPromien, today), systemBiletowy(KinoRegis), bilety24("https://www.bilety24.pl/kino/organizator/dabrowski-dom-kultury-1303", KinoSokolDabrowaTarnowska),
    // wroclaw
    new Bilety24SubdomainClient(http, "https://kulturalne-oborniki.bilety24.pl/repertuar/", KinoAstra, today = today, titles = titles), filmweb(1645, KinoDyskusyjnyKlubFilmowyPolitechnika),
    // slupsk
    bilety24("https://www.bilety24.pl/kino/organizator/leborskie-centrum-kultury-fregata-1683", KinoFregata),
    // elblag
    helios(HeliosNuxt.Tczew), filmweb(2352, KinoBaszta), msi(KinoPowisle), biletyna(KinoZulawskiOsrodekKultury), new KinoSwiatowidElblagClient(http, KinoSwiatowidElblag, today),
    // szczecin
    systemBiletowy(KinoKawiarnia), biletyna(KinoPDK), new SckStargardClient(http, KinoSCK),
    // gliwice
    new KinoScenaKulturaClient(http, KinoScenaKultura),
    // wloclawek
    msi(KinoJutrzenka), biletyna(KinoNawojka), msi(KinoNoweKinoWarszawa),
    // bielsko-biala
    bilety24("https://www.bilety24.pl/kino/organizator/kino-janosik-1500", KinoJanosik), systemBiletowy(KinoPckulKino), biletyna(KinoSwitCzechowiceDziedzice), biletyna(KinoTeatrElektryczny), bilety24("https://www.bilety24.pl/kino/organizator/osrodek-kultury-w-brzeszczach-1539", KinoWislaBrzeszcze), multikino("0033", MultikinoCzechowiceDziedzice),
    // rzeszow
    helios(HeliosNuxt.Krosno), new ArtKinoKrosnoClient(http, KinoArtKino, today), new KinoJednoscClient(http, KinoJednosc), msi(KinoMCK), bilety24Subdomain(KinoSniezka), new KinoSokolBrzozowClient(http, KinoSokolBrzozow), msi(KinoWarszawa),
    // plock
    msi(KinoZaRogiem), bilety24("https://www.bilety24.pl/kino/organizator/kutnowski-dom-kultury-1474", KinoKDK), systemBiletowy(KinoKalejdoskop), msi(KinoODEON),
    // trojmiasto
    biletyna(KinoNaSzekspirowskim), multikino("0027", MultikinoRumia),
    // tychy
    systemBiletowy(KinoNaszeKino), msi(KinoPlanetCinema),
    // torun
    biletyna(KinoMiejskieCentrumKultury), biletyna(KinoZdroj),
    // lodz
    new KinoSpojniaClient(http, KinoSpojnia), bilety24("https://www.bilety24.pl/kino/organizator/miejski-osrodek-kultury-stary-mlyn-w-zgierzu-1697", KinoStaryMlyn),
    // czestochowa
    new KinoDKFRumcajsClient(http, KinoDKFRumcajs, today = today), new KinoKarolinkaClient(http, KinoKarolinka), bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-w-radomsku-1546", KinoMDK), bilety24("https://www.bilety24.pl/kino/organizator/miejski-osrodek-kultury-centrum-im-adama-mickiewicza-w-zawierciu-1305", KinoMOKCentrum), new KinoZaciszeClient(http, KinoZacisze),
    // ── Nearby towns
    // Venues the 2026-09 sweeps added (Filmweb, bilety24, biletyna, the chains' own
    // branch lists), grouped by the city they were first found near. The key is
    // only where each is WIRED: the page a venue is listed on — its own town, a
    // cluster of small towns, or a major city — is `models.PolishPages`, and
    // `CinemaScraperCatalog.byCity` files every Polish scraper under that page,
    // whatever group it sits in here.
    // olsztyn
    new KinoGrunwaldClient(http, KinoGrunwald, today),   // Olsztynek
    // kielce
    biletyna(KinoZdrojBusko),   // Busko-Zdrój
    biletyna(KinoMuzaWloszczowa),   // Włoszczowa
    // bialystok
    new DomKulturyLapyClient(http, DomKulturyLapy, today),   // Łapy
    new KinoBielskClient(http, KinoBielsk),   // Bielsk Podlaski
    // wyszkow
    new KinoNarewClient(http, KinoNarew),   // Pułtusk
    // dabrowa-gornicza
    biletyna(KinoZbyszekOlkusz),   // Olkusz
    multikino("0038", MultikinoJaworzno),   // Jaworzno
    biletyna(KinoSztuka),   // Chrzanów
    // radom
    ekobilet("centrum-kultury-i-turystyki-w-ilzy-8211", KinoCKiTIlza),   // Iłża
    biletyna(KinoKsiazka),   // Stara Błotnica
    new IksorisBookingClient(http, IksorisBookingPage(IksorisOrigin("https://bilety.rck.drzewica.pl")), KinoRCKDrzewica),   // Drzewica
    // katowice
    new KinoGrajfkaClient(http, KinoGrajfka),   // Chorzów
    // Kino Frajda (Starochorzowski Dom Kultury) shares its bilety.chck.pl
    // systembiletowy portal with Chorzowskie Centrum Kultury's own events —
    // filmGroups keeps only the "Imprezy SDK" rows, same pattern as
    // KinoBCKBytom below but with this venue's own data-group value.
    systemBiletowy(KinoFrajda),   // Chorzów
    // chojnice
    biletyna(KinoPromienWiecbork),   // Więcbork
    biletyna(KinoCKiSSepolno),   // Sępólno Krajeńskie
    biletyna(KinoGOKLipka),   // Lipka
    biletyna(KinoTucholskiOsrodekKultury),   // Tuchola
    // szczecin
    biletyna(KinoMOKPolice),   // Police
    new KinoEvaClient(http, KinoEva),   // Międzyzdroje
    biletyna(KinoGryfinskiDomKultury),   // Gryfino
    // rzeszow
    new KinoSokolStrzyzowClient(http, KinoSokolStrzyzow, today),   // Strzyżów
    // warszawa
    biletyna(KinoDomSztukiUrsynow),   // Warszawa
    biletyna(KinoTerminalKultury),   // Warszawa
    biletyna(KinoBielanskiOsrodekKultury),   // Warszawa
    biletyna(KinoWawerskaStrefaKultury),   // Warszawa
    biletyna(KinoBasn),   // Piastów
    biletyna(KinoKasynoOficerskie),   // Nowy Dwór Mazowiecki
    bilety24("https://www.bilety24.pl/kino/organizator/osrodek-kultury-gminy-grodzisk-mazowiecki-1231", KinoGrodziskieCentrumKultury),   // Grodzisk Mazowiecki
    new KinoBajkaBlonieClient(http, KinoBajkaBlonie, today),   // Błonie
    biletyna(KinoGrojeckiOsrodekKultury),   // Grójec
    biletyna(KinoUciecha),   // Góra Kalwaria
    helios(HeliosNuxt.Wolomin),   // Wołomin
    biletyna(KinoKulturaWolomin),   // Wołomin
    filmweb(2079, KinoTeatrKurtyna),   // Sulejówek
    multikino("0039", MultikinoPruszkow),   // Pruszków
    biletyna(KinoCKiSPruszkow),   // Pruszków
    helios(HeliosNuxt.Legionowo),   // Legionowo
    biletyna(KinoOaza),   // Otwock
    // gorzow-wielkopolski
    biletyna(KinoMewaMiedzychod),   // Międzychód
    bilety24("https://www.bilety24.pl/kino/organizator/barlinecki-osrodek-kultury-1052", KinoPanorama),   // Barlinek
    msi(KinoMOKMiedzyrzecz),   // Międzyrzecz
    // legnica
    biletyna(KinoMOKGlogow),   // Głogów
    biletyna(TeatrGryphius),   // Głogów
    multikino("0048", MultikinoGlogow),   // Głogów
    new KinoKulturaChojnowClient(http, KinoKulturaChojnow),   // Chojnów
    // bydgoszcz
    biletyna(KinoCKSwiecie),   // Świecie
    biletyna(KinoRelaks),   // Nakło nad Notecią
    msi(KinoManhattanJanikowo),   // Janikowo
    // walbrzych
    biletyna(KinoSCKStrzegom),   // Strzegom
    // poznan
    biletyna(KinoCKPrzezmierowo),   // Przeźmierowo
    biletyna(KinoNOKNowyTomysl),   // Nowy Tomyśl
    bilety24("https://www.bilety24.pl/kino/organizator/kino-baszta-477", KinoBasztaSroda),   // Środa Wielkopolska
    bilety24("https://www.bilety24.pl/kino/organizator/kino-halszka-494", KinoHalszka),   // Szamotuły
    bilety24("https://www.bilety24.pl/kino/organizator/sremski-osrodek-kultury-kinoteatr-slonko-478", KinoteatrSlonko),   // Śrem
    bilety24("https://www.bilety24.pl/kino/organizator/biblioteka-i-kino-miasta-i-gminy-buk-596", KinoWielkopolanin),   // Buk
    bilety24("https://www.bilety24.pl/kino/organizator/przedsiebiorstwo-wielobranzowe-trojka-s-c-475", KinoTrojka),   // Września
    bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-i-biblioteka-imienia-powstancow-wielkopolskich-w-opalenicy-967", KinoOpalenica),   // Opalenica
    bilety24("https://www.bilety24.pl/kino/organizator/kinoteatr-gwiazda-962", KinoteatrGwiazda),   // Wronki
    // zielona-gora
    new KinoZaRogiemSiedlecClient(http, KinoZaRogiemSiedlec, today),   // Siedlec
    // lublin
    bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-promocji-i-turystyki-w-poniatowej-1220", KinoCzyn),   // Poniatowa
    ekobilet("kino-milenium", KinoMilenium),   // Milejów
    // MOK Świdnik's own page embeds a biletyna.pl "widget" iframe (ifid=22) —
    // an UNRELATED biletyna integration from Kino Lot Jelenia Góra, which is a
    // bilety24.pl subdomain instead. Distinct client because it's biletyna's
    // HTML widget template, not the JSON-LD place page BiletynaClient parses.
    new KinoLotSwidnikClient(http, KinoLotSwidnik),   // Świdnik
    // przemysl
    new SystemBiletowyClient(http, VisualSoftPortal("https://udk.systembiletowy.pl"), KinoOrzelUstrzyki, titles = titles),   // Ustrzyki Dolne
    systemBiletowy(KinoBieszczadzkiDK),   // Lesko
    // stalowa-wola
    new KinoJOKClient(http, KinoJOK, today),   // Janów Lubelski
    systemBiletowy(KinoCKiBNowaSarzyna),   // Nowa Sarzyna
    // krakow
    systemBiletowy(KinoMuzaMyslenice),   // Myślenice
    ekobilet("dk-wolbrom", KinoRadosc),   // Wolbrom
    systemBiletowy(KinoGryf),   // Miechów
    bilety24("https://www.bilety24.pl/kino/organizator/kino-wazka-w-niepolomicach-pole-kultury-samorzadowa-instytucja-kultury-miasta-i-gminy-niepolomice-1273", KinoWazka),   // Niepołomice
    ekobilet("kino-wielicka-mediateka", KinoWielickaMediateka),   // Wieliczka
    bilety24("https://www.bilety24.pl/kino/organizator/kino-lubon-2-0-1549", KinoLubon),   // Mszana Dolna
    // piotrkow-trybunalski
    biletyna(KinoMCKTkacz),   // Tomaszów Mazowiecki
    biletyna(KinoMDKOpoczno),   // Opoczno
    // nowy-sacz
    bilety24("https://www.bilety24.pl/kino/organizator/mckcie-w-grybowie-kino-biala-1321", KinoMCKGrybow),   // Grybów
    // rybnik
    biletyna(KinoFeniks),   // Rydułtowy
    // tarnow
    filmweb(2408, KinoMCKRadlow),   // Radłów
    bilety24("https://www.bilety24.pl/kino/organizator/gminne-centrum-kultury-i-czytelnictwa-w-lisiej-gorze-1658", KinoCentrumLisiaGora),   // Lisia Góra
    systemBiletowy(KinoJDK),   // Jasło
    multikino("0051", MultikinoMielec),   // Mielec
    // wroclaw
    biletyna(KinoNowyPafawag),   // Wrocław
    bilety24("https://www.bilety24.pl/kino/organizator/kino-odra-1601", KinoOdraBrzegDolny),   // Brzeg Dolny
    bilety24("https://www.bilety24.pl/kino/organizator/centrum-sztuki-w-olawie-1498", KinoOdraOlawa),   // Oława
    msi(GoKinoOlawa),   // Oława
    msi(KinoGrazyna),   // Strzelin
    bilety24("https://www.bilety24.pl/kino/organizator/kino-polonia-1090", KinoPoloniaTrzebnica),   // Trzebnica
    bilety24("https://www.bilety24.pl/kino/organizator/miejski-osrodek-kultury-i-sztuki-w-olesnicy-1193", KinoMOKiSOlesnica),   // Oleśnica
    // opole
    bilety24("https://www.bilety24.pl/kino/organizator/brzeskie-centrum-kultury-im-i-j-paderewskiego-1672", KinoCentrumBrzeg),   // Brzeg
    systemBiletowy(KinoCinemaN),   // Nysa
    // zlocieniec
    biletyna(KinoChDKChoszczno),   // Choszczno
    biletyna(KinoDOKDrawno),   // Drawno
    biletyna(KinoMGOKRecz),   // Recz
    msi(KinoWolnoscSzczecinek),   // Szczecinek
    // pulawy
    ekobilet("ock-opolelubelskie", KinoOpolanka),   // Opole Lubelskie
    // zamosc
    new KinoKadrTomaszowLubelskiClient(http, KinoKadrTomaszowLubelski, today),   // Tomaszów Lubelski
    // slupsk
    bilety24("https://www.bilety24.pl/kino/organizator/miejska-biblioteka-publiczna-w-lebie-1722", KinoLeba),   // Łeba
    // wielun
    msi(KinoWRatuszu),   // Zduńska Wola
    // leszno
    ekobilet("dom-kultury-w-gorze-7114", KinoDKGora),   // Góra
    ekobilet("zpkwasosz", KinoZaciszeWasosz),   // Wąsosz
    // pila
    biletyna(KinoOKJastrowie),   // Jastrowie
    // ostrowiec-swietokrzyski
    systemBiletowy(KinoKadrStaszow),   // Staszów
    // wloclawek
    bilety24("https://www.bilety24.pl/kino/organizator/rypinski-dom-kultury-1053", KinoBaltykRypin),   // Rypin
    // bielsko-biala
    biletyna(KinoPromyk),   // Bystra
    biletyna(KinoCentrumWadowice),   // Wadowice
    cinemaCity("1098", CinemaCityCieszyn),   // Cieszyn
    systemBiletowy(KinoCKiF),   // Sucha Beskidzka
    // ketrzyn
    new KinoNowaFalaClient(http, KinoNowaFalaGizycko),   // Giżycko
    // ciechanow
    biletyna(KinoNOKNasielsk),   // Nasielsk
    ekobilet("kinoton", KinoTon),   // Żuromin
    // wlodawa
    new KinoParczewClient(http, KinoParczew),   // Parczew
    // trojmiasto
    biletyna(TeatrBoto),   // Sopot
    biletyna(TeatrAtelier),   // Sopot
    biletyna(KinoCKGniewino),   // Gniewino
    new IksorisCalendarClient(http, IksorisBookingPage(IksorisOrigin("https://bilety.wck.org.pl")), KinoWCK, today),   // Wejherowo
    biletyna(KinoZaRogiemChmielno),   // Chmielno
    bilety24("https://www.bilety24.pl/kino/organizator/kino-zeglarz-1224", KinoZeglarz),   // Jastarnia
    bilety24("https://www.bilety24.pl/kino/organizator/kartuskie-centrum-kultury-w-kartuzach-1364", KinoKCK),   // Kartuzy
    bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-i-sportu-w-pruszczu-gdanskim-1569", KinoCKiSPruszczGdanski),   // Pruszcz Gdański
    // tychy
    msi(KinoAleKinoLibiaz),   // Libiąż
    // torun
    helios(HeliosNuxt.Grudziadz),   // Grudziądz
    // lodz
    biletyna(KinoMOKGlowno),   // Głowno
    new KinoTomiClient(http, KinoTomi),   // Pabianice
    helios(HeliosNuxt.Pabianice),   // Pabianice
    msi(KinoGornikLeczyca),   // Łęczyca
    // czestochowa
    msi(KinoPlanetCinemaZawiercie),   // Zawiercie
    new KinoMDKMyszkowClient(http, KinoMDKMyszkow),   // Myszków
  )

  /** The hosts of the ticketing platforms' venue pages read through an egress other than direct — a host that blocks the
   *  worker's IP — each by that egress: read off the platform tables above, so a venue routed there routes every other
   *  link of its host (its posters) the same way, with no list of hosts kept beside them. */
  val egressByHost: Map[String, HttpFetch] =
    (msiVenues.values.filter(_.fetch ne http).map(venue => venue.baseUrl -> venue.fetch) ++
      (biletynaPages.values ++ koneckieHalls.map(_._1.url)).map(_ -> bnFetch))
      .flatMap { case (url, fetch) => CinemaScraper.hostsOf(url).map(_ -> fetch) }.toMap
}
