package clients

import clients.tools.FakeHttpFetch
import models.{City, Country, AdaKinoStudyjne, KinoKulturaWolomin, KinoCentrumWadowice, KinoLen, KinoApolloDzialdowo, KinoZaRogiemChmielno, KinoCKiSSkierniewice, KinoPolonez, KinoZacheta, KinoKoneckieCentrumKultury, KinoBaszta, KinoNadWarta, KinoEcho, KinoMewaBudzyn, UsRoster, HeliosSiedlce, KinoGiewont, KinoMuranow, KinoWCKWalcz, MultikinoPruszkow, ArcCinemaGreatYarmouth, Cinema, CineworldSheffield, KinoFenomen, KinoKameralne, KinoKryterium, KinoPiastOstrzeszow, KinoPort, KinoWislaBrzeszcze, OdeonCinemaActon, VueCinemasSheffield}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{ChainFlicksFallback, CinemaScraperCatalog}
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.cinemas.common.{CinemaScraper, FlicksClient, FlicksMarket, GatsbyBoxOfficeClient, MultiListingScraper}
import services.cinemas.pl.{BiletynaClient, Bilety24OrganizerClient, FilmwebShowtimesClient, HeliosClient, MultikinoClient}
import services.cinemas.us.{AlamoDrafthouseClient, UsChainVenues}
import services.cinemas.uk.CineworldClient
import services.cinemas.us.{AmcClient, RegalClient}
import _root_.tools.{CachingDetailFetch, GetOnlyHttpFetch, HttpFetch}
import services.freshness.{Freshness, FreshnessKind}

import java.time.LocalDate
import scala.concurrent.duration._

/**
 * Guards the per-cinema fetch seams the catalog wires for venues whose origin
 * blocks our Fly datacenter IP, so they must fetch through their own injected
 * seam — NOT the shared `http` — or the live scrape regresses:
 *   - biletyna.pl 403s our IP (Cloudflare waiting-room) → Kino Kameralne /
 *     ADA Kino Studyjne fetch through `bnFetch` (Zyte in prod, fixture fake here).
 *   - bilety.ck105.koszalin.pl (Kino Kryterium) times out our IP AND every Decodo
 *     proxy IP at the TCP layer → fetches through `zyteFetch` (Zyte's
 *     true-residential network in prod, the one egress that reaches it).
 *   - www.flicks.co.uk 403s our IP behind Cloudflare → every UK venue fetches
 *     through `flicksFetch` (the Decodo residential proxy in prod).
 *   - vwc.odeon.co.uk 403s our IP behind Cloudflare too (since the 2026-08-29 move
 *     to Hetzner changed the egress IP) → all 102 Odeon venues fetch through
 *     `odeonFetch`.
 * Each seam's fixture-less `http` makes a leaked fetch throw / come back empty,
 * so a refactor that re-buries the fetch on `http` is caught here. (CI also sets
 * ZYTE_API_KEY, so a leak onto `http` would route biletyna through real Zyte.)
 */
class CinemaScraperCatalogSpec extends AnyFlatSpec with Matchers with OptionValues {

  // `http` deliberately points at a fixture directory without any biletyna/ck105
  // capture, so any cinema that wrongly fetches through it (instead of its own
  // seam) fails.
  private val http = new FakeHttpFetch("does-not-exist")

  /** Catalog with each Fly-IP-blocked seam pointed at a fixture directory (or the
   *  fixture-less `http` by default); a cinema that leaks onto the wrong seam
   *  throws / returns empty. */
  private def catalog(biletyna:   String = "does-not-exist",
                      zyte:       String = "does-not-exist",
                      flicks:     HttpFetch = http,
                      vue:        HttpFetch = http,
                      odeon:      HttpFetch = http,
                      odeonToken: Option[String] = None): CinemaScraperCatalog =
    new CinemaScraperCatalog(
      http, mkFetch = http, bnFetch = new FakeHttpFetch(biletyna), today = LocalDate.of(2026, 6, 6),
      chainDetailCache = (_, h, ttl) => new CachingDetailFetch(h, ttl),
      zyteFetch = new FakeHttpFetch(zyte), flicksFetch = flicks, vueFetch = vue,
      odeonFetch = odeon, odeonAuthToken = () => odeonToken, titles = titleNormalizer
    )

  /** The slug of the page a cinema is listed on. */
  private def pageOf(cinema: Cinema): String = City.forCinema(cinema).value.slug

  /** An HttpFetch that fails every GET and POST with a uniquely-identifiable
   *  message, so a test can prove WHICH seam a scraper egressed through by catching
   *  it (Vue POSTs its token, so POST must be tagged too). */
  private def probe(tag: String): HttpFetch = new HttpFetch {
    def get(url: String): String = throw new RuntimeException(s"SEAM:$tag GET $url")
    def post(url: String, body: String, contentType: String): String = throw new RuntimeException(s"SEAM:$tag POST $url")
  }

  /** The `SEAM:` tag reachable anywhere in a throwable's cause chain. */
  private def seamChain(t: Throwable): String =
    Iterator.iterate(t)(_.getCause).takeWhile(_ != null).flatMap(x => Option(x.getMessage)).mkString(" | ")

  "CinemaScraperCatalog" should "route Kino Kameralne through the injected biletyna seam, not the shared http" in {
    val scraper = catalog(biletyna = "kino-kameralne").all.find(_.cinema == KinoKameralne).value
    val movies  = scraper.fetch()  // reads the kino-kameralne fixture via bnFetch
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoKameralne)
  }

  it should "route ADA Kino Studyjne through the injected biletyna seam, not the shared http" in {
    val scraper = catalog(biletyna = "ada-kino-studyjne").all.find(_.cinema == AdaKinoStudyjne).value
    val movies  = scraper.fetch()  // reads the ada-kino-studyjne fixture via bnFetch
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(AdaKinoStudyjne)
  }

  // Both Skierniewice venues sell every ticket through biletyna, one place page
  // each: Kinoteatr Polonez (the cinema, two halls) and the CKiS concert hall
  // at ul. Reymonta 33, which screens the odd film between concerts. Filmweb
  // (320 / 3149) had gone empty for both — CKiS white from 2026-09-22, Polonez
  // kept "green" on one stray slot against 50 real screenings. Fixtures
  // captured live 2026-09-26.
  it should "route Kino Polonez through its biletyna place page, not Filmweb" in {
    val movies = catalog(biletyna = "biletyna-skierniewice").all.find(_.cinema == KinoPolonez).value.fetch()
    movies.map(_.cinema).toSet shouldBe Set(KinoPolonez)
    val slot = movies.find(_.movie.title == "Mistyczka").value.showtimes
      .find(_.dateTime == java.time.LocalDateTime.of(2026, 9, 26, 18, 15)).value
    slot.bookingUrl.value shouldBe "https://biletyna.pl/film/Mistyczka?eid=704171#opis"
  }

  it should "route Kino CKiS Skierniewice through its biletyna concert-hall page, keeping only the films" in {
    val movies = catalog(biletyna = "biletyna-skierniewice").all.find(_.cinema == KinoCKiSSkierniewice).value.fetch()
    movies.map(_.cinema).toSet shouldBe Set(KinoCKiSSkierniewice)
    val slot = movies.find(_.movie.title == "Krajobraz po bitwie").value.showtimes
      .find(_.dateTime == java.time.LocalDateTime.of(2026, 9, 29, 19, 0)).value
    slot.bookingUrl.value shouldBe "https://biletyna.pl/film/Krajobraz-po-bitwie-1970?eid=694150#opis"
    movies.size should be <= 9   // the page's 9 ScreeningEvents; its 19 concerts/plays stay out
  }

  // Six biletyna-ticketed venues whose Filmweb listing had fallen out of step
  // with the box office: green on one or two stray slots while biletyna sold
  // their real week (the `thin` uptime flag found them, 2026-09-26). Each now
  // reads its own biletyna place page. Street addresses were matched against
  // Filmweb's /info for each. Fixtures captured live 2026-09-26.
  private val desyncedFilmwebVenues = Seq(
    (KinoGiewont, "resident evil", java.time.LocalDateTime.of(2026, 9, 29, 19, 50),
      "https://biletyna.pl/film/Resident-Evil?eid=704440#opis"),
    (KinoKulturaWolomin, "folwark zwierzęcy", java.time.LocalDateTime.of(2026, 9, 29, 15, 0),
      "https://biletyna.pl/film/Folwark-zwierzecy?eid=704317#opis"),
    (KinoCentrumWadowice, "lalka", java.time.LocalDateTime.of(2026, 10, 4, 19, 0),
      "https://biletyna.pl/film/Lalka-2026?eid=695958#opis"),
    (KinoLen, "lalka", java.time.LocalDateTime.of(2026, 10, 6, 17, 0),
      "https://biletyna.pl/film/Lalka-2026?eid=699411#opis"),
    (KinoApolloDzialdowo, "tedi i magiczna lampa", java.time.LocalDateTime.of(2026, 10, 1, 16, 0),
      "https://biletyna.pl/film/Tedi-i-magiczna-lampa?eid=702412#opis"),
    (KinoZaRogiemChmielno, "wojna z dziadkiem", java.time.LocalDateTime.of(2026, 9, 30, 13, 0),
      "https://biletyna.pl/film/Wojna-z-dziadkiem?eid=699434#opis"),
  )
  for ((cinema, title, when, booking) <- desyncedFilmwebVenues)
    it should s"route ${cinema.displayName} through its biletyna place page, not Filmweb" in {
      val movies = catalog(biletyna = "biletyna-filmweb-desynced").all.find(_.cinema == cinema).value.fetch()
      movies.map(_.cinema).toSet shouldBe Set(cinema)
      val film = movies.find(_.movie.title.toLowerCase.contains(title)).value
      film.showtimes.find(_.dateTime == when).value.bookingUrl.value shouldBe booking
    }

  // Kino Fenomen (WDK) is iframe639.biletyna.pl — a biletyna host whose per-film
  // /artist/view/id detail pages 403 our Fly IP behind Cloudflare, so its deferred
  // detail enrichment must fetch through `bnFetch`, not the shared `http`. Wired
  // on `http` (as it was when the venue was added), every detail fetch 403s → the
  // enrichment /uptime bar goes red; here the fixture-less `http` makes the leak a
  // hard failure. Fixture captured 2026-07-04 from `iframe639.biletyna.pl/?display=events`.
  it should "route Kino Fenomen through the injected biletyna seam, not the shared http" in {
    val scraper = catalog(biletyna = "kino-fenomen").all.find(_.cinema == KinoFenomen).value
    val movies  = scraper.fetch()  // reads the kino-fenomen fixture via bnFetch
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoFenomen)
  }

  // Kino Kryterium's origin (bilety.ck105.koszalin.pl) silently times out our Fly
  // egress IP AND every Decodo proxy IP at the TCP layer, so a direct scrape came
  // back empty → a permanent white /uptime bar. It must fetch through the Zyte
  // seam (the one egress that reaches it). The shared `http` has no ck105 fixture,
  // so a fetch that leaked onto it returns empty and this fails. Fixture captured
  // 2026-06-15 from the live month pages `/MSI/mvc/pl?sort=Name&date=2026-06` (+ 2026-07).
  it should "route Kino Kryterium through the injected Zyte seam, not the shared http" in {
    val scraper = catalog(zyte = "kino-kryterium").all.find(_.cinema == KinoKryterium).value
    val movies  = scraper.fetch()  // reads the kino-kryterium fixture via zyteFetch
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoKryterium)
  }

  // www.flicks.co.uk sits behind Cloudflare and 403s our Fly datacenter egress IP
  // (proven 2026-07-26 from kinowo-worker-uk: the identical GET + UA returns 403
  // from Fly and 200 from a residential IP), which took ALL 843 UK venues down at
  // once — Flicks is the only UK source. So every Flicks venue must fetch through
  // `flicksFetch` (the Decodo residential proxy in prod), never the shared `http`.
  // The fixture-less `http` makes a leak throw here. The programme page lists the
  // one recorded day so the chunked scrape lands on the captured sessions fragment.
  it should "route UK Flicks venues through the injected residential-proxy seam, not the shared http" in {
    val sessions  = new FakeHttpFetch("flicks")
    val programme = s"${FlicksMarket.UnitedKingdom.baseUrl}/cinema/arc-cinema-at-the-royalty-great-yarmouth/"
    val flicks = new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url == programme) """<div class="timetable__day" data-date="2026-07-11"></div>"""
        else sessions.get(url)
    }
    // Arc Cinema Great Yarmouth is a genuine flicks-primary independent (unlike the
    // chain venues, which moved to own-site clients with flicks only as fallback).
    val scraper = catalog(flicks = flicks).all.find(_.cinema == ArcCinemaGreatYarmouth).value
    val movies  = scraper.fetch()  // reads the flicks fixture via flicksFetch
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(ArcCinemaGreatYarmouth)
  }

  // UK chain venues (Cineworld / Vue / Showcase / Everyman) are now own-site
  // PRIMARY, with flicks.co.uk kept as the aggregator fallback via
  // ChainFlicksFallback + WorkerWiring.recordingScraper (the mirror of Poland's
  // own-site→Filmweb setup). Guard both halves: the catalogue wires the chain
  // client (not flicks) for those venues, AND every one keeps a flicks fallback
  // slug — a regression that dropped either would silently lose coverage the day a
  // chain API changed shape. (Before this wiring these venues WERE FlicksClients
  // and the slug map was empty, so this test fails on the pre-change catalogue.)
  it should "wire UK chain venues to their own-site client with flicks kept as the fallback" in {
    val c = catalog()
    c.all.find(_.cinema == CineworldSheffield).value shouldBe a [CineworldClient]
    c.flicksFallbackSlugs.get(CineworldSheffield).value.slug shouldBe "cineworld-sheffield"
    // A UK venue's fallback must name the UK market — looking it up on flicks.us 404s.
    c.flicksFallbackSlugs.get(CineworldSheffield).value.market shouldBe FlicksMarket.UnitedKingdom
    // Cineworld 87 + Vue 88 + Showcase 16 + Everyman 50 + Odeon 101 = 342 UK,
    // plus the US chain venues that are reachable without Zyte:
    //   Alamo 39 + Landmark 26 + Showcase US 13 = 78 (Alamo Naples closed 9 Sep 2026).
    // Every one of them is own-site PRIMARY with flicks.us kept as the fallback, so
    // this total moves whenever a chain is added, dropped, or a venue map changes.
    // Odeon was 102 until Basingstoke closed on 31 Aug 2026 and Odeon dropped it
    // from its own estate — see OdeonVenueMapSpec.
    // AMC and Regal are NOT among them — see the US chain test below for why.
    ChainFlicksFallback.ukSlugs should have size 342
    c.flicksFallbackSlugs should have size 342 + 78

    // Regal stays ON the aggregator, and must NOT carry a fallback entry either —
    // a flicks primary with a flicks fallback would just re-fetch the same URL on
    // failure. See the US chain test below for why it is not own-site.
    val northHollywood = UsRoster.flicksSlugByCinema
      .collectFirst { case (cinema, "regal-north-hollywood") => cinema }.value
    c.all.find(_.cinema == northHollywood).value shouldBe a [FlicksClient]
    c.flicksFallbackSlugs.get(northHollywood) shouldBe None

    // A Regal location Regal's own roster no longer lists is on flicks.us for a
    // second, independent reason, and carries no fallback entry either.
    val sonora = UsRoster.flicksSlugByCinema
      .collectFirst { case (cinema, "regal-sonora") => cinema }.value
    c.all.find(_.cinema == sonora).value shouldBe a [FlicksClient]
    c.flicksFallbackSlugs.get(sonora) shouldBe None
    c.flicksFallbackSlugs.keys.foreach { cin =>
      val primary = c.all.find(_.cinema == cin).value
      primary should not be a [FlicksClient]      // moved off the aggregator…
      primary.chain shouldBe true                 // …onto an own-site chain source
    }
  }

  // The US mid-tier chains, same arrangement — own-site PRIMARY with flicks.us
  // kept as the fallback. Two things can silently break here that the UK wiring
  // cannot: the fallback MARKET (a US venue handed to a UK FlicksClient would ask
  // flicks.co.uk for an American slug and 404 forever — a fallback that looks
  // wired and never fires), and the venue mapping itself, since US cinemas are
  // built at runtime from data/us/venues.json and are matched by DISPLAY NAME.
  it should "wire US chain venues to their own-site client with flicks.us kept as the fallback" in {
    val c = catalog()

    def primaryFor(name: String) =
      c.all.find(_.cinema.displayName == name).value

    primaryFor("Alamo Drafthouse Lakeline") shouldBe a [AlamoDrafthouseClient]
    primaryFor("Landmark Nuart Theatre") shouldBe a [GatsbyBoxOfficeClient]
    primaryFor("Showcase Legacy Place Dedham") shouldBe a [GatsbyBoxOfficeClient]

    // AMC is the exception: its origins are GEO-FENCED to the United States.
    // www.amctheatres.com and graph.amctheatres.com answer 200 to a US IP and
    // refuse every European one — measured 2026-08-30 from a Polish residential IP
    // (403 Cloudflare on 29 of 29 sampled venues, `/robots.txt` included), from the
    // Decodo pool (connection reset) and from Zyte's FI/DE/GB/PL pools, against 200
    // on Zyte US. The worker egresses from Hetzner Helsinki, so `flicksFetch` — the
    // Decodo residential path AmcClient was wired through — cannot reach it: in
    // prod it produced 144 failures and ZERO usable scrapes before being reverted.
    //
    // Regal (www.regmovies.com) is geo-fenced the same way and fails on the same
    // three paths. Zyte's default pool DOES clear it, which is how it was wired —
    // but flicks.us already serves every one of these venues, so Zyte is a cost
    // with an alternative rather than a last resort, and both chains sit on the
    // aggregator until a US egress exists.
    primaryFor("AMC Town Center 20") shouldBe a [FlicksClient]
    primaryFor("AMC CLASSIC Farmington 4") shouldBe a [FlicksClient]
    c.all.count(_.isInstanceOf[AmcClient]) shouldBe 0
    c.all.count(_.isInstanceOf[RegalClient]) shouldBe 0

    val usFallbacks = c.flicksFallbackSlugs.filter { case (cin, _) =>
      UsChainVenues.all.contains(cin.displayName)
    }
    usFallbacks should have size 78
    // Every US chain venue falls back to the US market, never the UK one.
    all(usFallbacks.values.map(_.market).toSeq) shouldBe FlicksMarket.UnitedStates
    // …and to the very slug it used to be catalogued under, derived rather than
    // restated, so primary and fallback cannot drift about which venue they mean.
    usFallbacks.foreach { case (cin, fallback) =>
      fallback.slug shouldBe UsRoster.flicksSlugByCinema(cin)
      c.all.find(_.cinema == cin).value should not be a [FlicksClient]
    }
  }

  // US chain venues are matched by DISPLAY NAME, which makes a typo silent: the
  // name simply never matches, the venue quietly stays on flicks.us, and nothing
  // fails. Worse, `UsRoster` QUALIFIES a display name that collides with a Polish,
  // UK or German venue (appending its state), so a future roster collision could
  // rename one out from under these maps. Pin that every mapped name is real.
  it should "name only venues that actually exist in the US roster" in {
    val unknown = UsChainVenues.all.filterNot(UsRoster.byDisplayName.contains)
    withClue("these UsChainVenues names match no US roster venue, so they silently stay on flicks.us: ") {
      unknown shouldBe empty
    }
    UsChainVenues.all should have size 78
  }

  // A NEW CHAIN ORIGIN MUST ARRIVE PACED. HostPolicy rows match by host SUFFIX, so
  // a host with no row of its own is not paced at all — the exact condition that
  // produced this repo's self-inflicted 429 storm on flicks.co.uk. Asserting the
  // three literal hosts would not catch the case that actually bites (someone adds
  // a fourth US chain and forgets the row), so this reads the hosts off the
  // scrapers themselves and demands a pace for each.
  it should "pace every host the US chain primaries fetch from" in {
    val c = catalog()
    val chainHosts = c.all
      .filter(s => UsChainVenues.all.contains(s.cinema.displayName))
      .flatMap(_.scrapeHosts)
      .distinct
    chainHosts should contain theSameElementsAs
      Seq("drafthouse.com", "www.showcasecinemas.com", "www.landmarktheatres.com")
    chainHosts.foreach { host =>
      withClue(s"$host has no HostPolicy pace row, so it is UNPACED: ") {
        _root_.tools.RateLimitedHttpFetch.configuredInterval(new _root_.settings.ProcessConfiguration(_root_.tools.Env.of()))(s"https://$host/") should not be empty
      }
    }
  }

  // ── Spain: Ocine on its own ticketing servers ────────────────────────────────

  // SensaCine theaterId -> the venue's own ticketing host. Twelve of these had
  // NEVER carried a real programme on SensaCine (its per-venue page advertised no
  // days at all, 2026-09-25), so the Ocine venues read their chain's own API
  // instead. Keyed by theaterId, not display name, because `SpanishRoster`
  // qualifies a name that collides with another country's and the id is stable.
  private val OcineHosts: Map[String, String] = Map(
    "E0651" -> "tickets.ocinearenys.es",
    "E0462" -> "tickets.ocineblanes.es",
    "E0509" -> "tickets.ocinegavarres.es",
    "E0362" -> "tickets.ocinegirona.es",
    "E0507" -> "tickets.ocinegranollers.es",
    "E0713" -> "tickets.ocinemagic.es",
    "E0537" -> "tickets.ocinemendibil.es",
    "E0554" -> "tickets.ocineplatjadaro.es",
    "E2900" -> "tickets.ocineplazaeboli.es",
    "E0474" -> "tickets.ocinepremiumaqua.es",
    "E1045" -> "tickets.ocinepremiumbahiareal.es",
    "E0925" -> "tickets.ocinepremiumestepark.es",
    "E0796" -> "tickets.ocinerioshopping.es",
    "E0556" -> "tickets.ocineroquetes.es",
    "E0787" -> "tickets.ocineserrallo.es",
    "E1004" -> "tickets.ocineurbanxmadrid.es",
    "E0727" -> "tickets.ocinevilaseca.es",
  )

  // (province slug, display name, cartelera) of the chain's venues SensaCine does
  // not list at all, so the harvest never had them: the roster adds them from
  // `data/spain/ocine.json`, and their own server is their only source. Each
  // server verified 2026-09-25 to list 15-27 films. Porto Pi's answers only on
  // :8444 — the port its own site links, 443 never connects — so the port has to
  // survive into the URL the scraper fetches.
  private val UnlistedOcine: Seq[(String, String, String)] = Seq(
    ("almeria",        "Ocine Copo",                     "https://tickets.ocinecopo.es/"),
    ("tarragona",      "Ocine El Vendrell",              "https://tickets.ocinevendrell.es/"),
    ("las-palmas",     "Ocine Premium 7 Palmas",         "https://tickets.ocinepremium7palmas.es/"),
    ("pontevedra",     "Ocine Premium Gran Vía de Vigo", "https://tickets.ocinepremiumgranvia.es/"),
    ("lerida",         "Ocine Premium Lleida",           "https://tickets.ocinepremiumlleida.es/"),
    ("asturias",       "Ocine Premium Los Fresnos",      "https://tickets.ocinepremiumlosfresnos.es/"),
    ("islas-baleares", "Ocine Premium Porto Pi",         "https://tickets.ocinepremiumportopi.es:8444/"),
    ("madrid",         "Ocine Quadernillos",             "https://tickets.ocinequadernillos.es/"),
    ("madrid",         "Ocine Urban Caleido",            "https://tickets.ocineurbancaleido.es/"),
  )

  private def spanishVenue(theaterId: String): Cinema =
    models.SpanishRoster.theaterIdByCinema.collectFirst { case (c, id) if id == theaterId => c }
      .getOrElse(fail(s"no Spanish roster venue has SensaCine id $theaterId"))

  "Spain's Ocine venues" should "each read the chain's own ticketing server, not SensaCine" in {
    val c = catalog()
    OcineHosts.foreach { case (theaterId, host) =>
      val venue    = spanishVenue(theaterId)
      val scrapers = c.all.filter(_.cinema == venue)
      withClue(s"${venue.displayName} ($theaterId): ") {
        scrapers should have size 1
        scrapers.head.scrapeHosts shouldBe Set(host)
      }
    }
  }

  it should "add the chain's venues SensaCine does not list, under their province, on their own server" in {
    val c = catalog()
    UnlistedOcine.foreach { case (province, name, cartelera) =>
      withClue(s"$name: ") {
        val venue = Country.Spain.bySlug(province).cinemas.find(_.displayName == name)
          .getOrElse(fail(s"not in the roster under $province"))
        val scrapers = c.all.filter(_.cinema == venue)
        scrapers should have size 1
        scrapers.head.sourceUrl shouldBe Some(cartelera)
      }
    }
  }

  it should "pace every Ocine ticketing host" in {
    (OcineHosts.values ++ UnlistedOcine.map(_._3).flatMap(CinemaScraper.hostsOf(_))).foreach { host =>
      withClue(s"$host has no HostPolicy pace row, so it is UNPACED: ") {
        _root_.tools.RateLimitedHttpFetch.configuredInterval(new _root_.settings.ProcessConfiguration(_root_.tools.Env.of()))(s"https://$host/api/v1/sessions") should not be empty
      }
    }
  }

  // Ocine Tudela's ticketing server did not answer on 443 (connect timeout,
  // 2026-09-25) while SensaCine lists its full week, and Ocine Sant Celoni Altrium
  // has no ticketing server the chain links to at all — both stay on SensaCine.
  it should "leave the Ocine venues without a reachable own server on SensaCine" in {
    val c = catalog()
    Seq("E0317", "E0745").foreach { theaterId =>
      val venue = spanishVenue(theaterId)
      withClue(s"${venue.displayName}: ") {
        c.all.find(_.cinema == venue).value.scrapeHosts shouldBe Set("www.sensacine.com")
      }
    }
  }

  // The venues we could NOT verify against a chain's own roster must stay on the
  // aggregator. Wiring one to a chain client on a guessed id would 404 it into a
  // permanently red venue; leaving it on flicks.us keeps it working. All three are
  // in our roster but absent from their chain's own venue list.
  it should "leave the unmapped chain venues on flicks.us" in {
    val c = catalog()
    Seq("Showcase Randolph", "Landmark Esquire Theatre", "Landmark Plaza Frontenac Cinema")
      .foreach { name =>
        withClue(s"$name should still be a FlicksClient: ") {
          c.all.find(_.cinema.displayName == name).value shouldBe a [FlicksClient]
        }
      }
  }

  // Cineworld + Vue are Cloudflare-403'd from our Fly datacenter IP (verified in
  // prod 2026-07-27 — the pre-merge check tested the roster from a residential IP,
  // not the data endpoints from Fly), so their scrapes MUST egress through the
  // residential seam, never the shared `http`. A leak onto `http` would 403 every
  // UK chain scrape from prod. The `probe` seams throw a tagged error, so catching
  // it proves which fetch the scraper actually used.
  it should "route Cineworld through the flicks residential seam, not the shared http" in {
    val ex = intercept[Exception] {
      catalog(flicks = probe("FLICKS")).all.find(_.cinema == CineworldSheffield).value.fetch()
    }
    seamChain(ex) should include ("SEAM:FLICKS")
  }

  it should "route Vue through the host-sticky vue residential seam, not the shared http" in {
    val ex = intercept[Exception] {
      catalog(vue = probe("VUE")).all.find(_.cinema == VueCinemasSheffield).value.fetch()
    }
    seamChain(ex) should include ("SEAM:VUE")
  }

  // Odeon's Vista `ocapi` backend was the one UK chain that reached the origin from
  // our datacenter IP, so it was wired on the shared `http`. The 2026-08-29 move off
  // Fly changed the egress IP and Cloudflare now serves it an "Attention Required"
  // 403 — the identical unauthenticated GET returns 401 (i.e. reaches the origin)
  // from a residential IP and from every Decodo port — which took all 102 Odeon
  // venues red at once. So it must egress residential like Cineworld/Vue. A token is
  // supplied because the client throws before fetching without one, which would make
  // this pass for the wrong reason.
  it should "route Odeon through the odeon residential seam, not the shared http" in {
    val ex = intercept[Exception] {
      catalog(odeon = probe("ODEON"), odeonToken = Some("jwt"))
        .all.find(_.cinema == OdeonCinemaActon).value.fetch()
    }
    seamChain(ex) should include ("SEAM:ODEON")
  }

  // KinoPort was moved onto Filmweb (id 1735) when a 2026-06 rebuild retired its
  // gcsw.pl/kino/ programme alias — and Filmweb then went silently empty for it,
  // serving `[]` on every date while the venue screened five films a day. It now
  // reads GCSW's own repertoire post again, via the WP REST route. Guard the
  // seam: it must read gcsw.pl, never fall back to the empty Filmweb source.
  it should "scrape KinoPort off gcsw.pl, not the silently-empty Filmweb source" in {
    val scraper = catalog(biletyna = "kino-kameralne").all.find(_.cinema == KinoPort).value
    scraper.scrapeHosts should contain ("gcsw.pl")
    scraper.scrapeHosts should not contain "www.filmweb.pl"
  }

  // Both venues renamed themselves away from "Kino …" in bilety24's own slug
  // (`kino-piast-w-ostrzeszowie-601` → `ostrzeszowskie-centrum-kultury-601`,
  // `kino-wisla-w-brzeszczach-1539` → `osrodek-kultury-w-brzeszczach-1539`),
  // keeping the numeric id. bilety24 currently 301s the old slug to the new one,
  // so nothing was broken — but an aggregator that stops honouring a retired
  // slug is exactly how the Helios rename turned into 0 films, and depending on
  // someone else's redirect is a dependency we don't need. Address them by the
  // slug they publish today.
  //
  // A 2026-09-23 sweep found 47 more organisers wired by a slug bilety24 only
  // 301s: 14 older venues that had renamed since, and 33 of the ~50 the
  // regional-hubs commit added, whose slugs were composed from the venue name
  // rather than copied off bilety24. One of each is pinned here.
  it should "address the renamed bilety24 organisers by their canonical slug, not the redirecting one" in {
    val scrapers = catalog(biletyna = "kino-kameralne").byCity.values.flatten.toSeq
    def sourceUrlOf(cinema: Cinema): String = scrapers.find(_.cinema == cinema).value.sourceUrl.value

    sourceUrlOf(KinoPiastOstrzeszow) shouldBe
      "https://www.bilety24.pl/kino/organizator/ostrzeszowskie-centrum-kultury-601"
    sourceUrlOf(KinoWislaBrzeszcze) shouldBe
      "https://www.bilety24.pl/kino/organizator/osrodek-kultury-w-brzeszczach-1539"
    sourceUrlOf(KinoEcho) shouldBe
      "https://www.bilety24.pl/kino/organizator/kino-echo-1159"
    sourceUrlOf(KinoMewaBudzyn) shouldBe
      "https://www.bilety24.pl/kino/organizator/gminny-osrodek-kultury-685"
  }

  // Braniewo's Baszta was wired (2026-06) to bilety24 organiser 477 — Kino
  // Baszta in Środa Wielkopolska — under another slug, so Elbląg's page listed
  // Środa's programme. One scraper per upstream listing is now held for every
  // source by `services.cinemas.roster.CinemaRosterAuditSpec`.
  it should "scrape Braniewo's Baszta off its own Filmweb id, not Środa Wielkopolska's bilety24 organiser" in {
    val scraper = catalog(biletyna = "kino-kameralne").byCity(pageOf(KinoBaszta)).find(_.cinema == KinoBaszta).value
    scraper shouldBe a [FilmwebShowtimesClient]
    scraper.sourceUrl.value should endWith ("-2352")
  }

  // Kino nad Wartą is Miejski Dom Kultury w Kole (Słowackiego 5), whose own
  // bilety24 organiser is 1621. The June wiring used 1626 — Konińskie Centrum
  // Kultury, Kino Studyjne Centrum's source — so Koło listed Konin's programme
  // showtime for showtime; Filmweb 1526 is the right venue but lists only a day
  // or two of it (one screening in the next ten days, against bilety24's month).
  it should "scrape Koło's Kino nad Wartą off its own bilety24 organiser, not Konin's or Filmweb's" in {
    val scraper = catalog(biletyna = "kino-kameralne").byCity(pageOf(KinoNadWarta)).find(_.cinema == KinoNadWarta).value
    scraper shouldBe a [Bilety24OrganizerClient]
    scraper.sourceKey.value shouldBe "bilety24.pl/kino/organizator/1621"
  }

  // The one Konin-catchment venue still on Filmweb; its capture is replayed in KoninFilmwebShowtimesSpec.
  it should "scrape Kleczew's Kino Zachęta off Filmweb id 2405" in {
    val scraper = catalog().byCity(pageOf(KinoZacheta)).find(_.cinema == KinoZacheta).value
    scraper shouldBe a [FilmwebShowtimesClient]
    scraper.sourceUrl.value should endWith ("-2405")
  }

  // Filmweb's "Etiuda OBK" (3024) is the same screen as bilety24's MCK Ostrowiec
  // organiser (Siennieńska 54, identical showtimes), which lists its whole
  // programme — so wiring both showed Kino Etiuda twice under two names.
  it should "scrape Ostrowiec's Kino Etiuda once, off its bilety24 organiser" in {
    val etiudas = catalog(biletyna = "kino-kameralne").byCity("ostrowiec-swietokrzyski")
      .filter(_.cinema.displayName.contains("Etiuda"))
    etiudas.map(_.cinema.displayName) shouldBe Seq("Kino Etiuda")
    etiudas.head shouldBe a [Bilety24OrganizerClient]
  }

  // Końskie's culture centre lists its cinema hall and its stage hall on two
  // biletyna pages. The stage hall screens the odd film, so it was wired as a
  // second cinema — one venue shown twice under two names. It is ONE cinema read
  // off both pages, each showtime tagged with its hall.
  it should "scrape Końskie's culture centre as one cinema off both its halls' pages" in {
    val konskie = catalog(biletyna = "08-06-2026").byCity(pageOf(KinoKoneckieCentrumKultury)).filter(_.cinema.displayName.startsWith("Koneckie"))
    konskie.map(_.cinema) shouldBe Seq(KinoKoneckieCentrumKultury)
    val venue = konskie.head.asInstanceOf[MultiListingScraper]
    venue.listings.flatMap(_.sourceUrl) shouldBe Seq(
      "https://biletyna.pl/Konskie/Koneckie-Centrum-Kultury-sala-kinowa",
      "https://biletyna.pl/Konskie/Koneckie-Centrum-Kultury-sala-widowiskowa")
    Cinema.all.map(_.displayName).filter(_.startsWith("Koneckie")) shouldBe Seq("Koneckie Centrum Kultury")
    venue.fetch().flatMap(_.showtimes).flatMap(_.room).toSet shouldBe Set("sala kinowa", "sala widowiskowa")
  }

  // A `Cinema` that's modelled (so it shows on the web/in a city) but has no
  // scraper is silently never populated — the city renders empty forever. This
  // also catches a `City.slug` ↔ `byCity` key mismatch: `catalog.all` resolves
  // scrapers by `c.slug`, so a typo drops that city's cinemas out of `all`.
  it should "wire a scraper for every modelled cinema" in {
    // Over the WHOLE `byCity` map (every modelled city, including UK cities that
    // are currently disabled and so absent from the live `catalog.all`), not the
    // live subset — otherwise a disabled city's unwired cinema would slip by.
    val scraped  = catalog(biletyna = "kino-kameralne").byCity.values.flatten.map(_.cinema).toSet
    val modelled = Cinema.all.toSet
    withClue(s"modelled but unscraped: ${(modelled diff scraped).map(_.displayName).toSeq.sorted}") {
      (modelled diff scraped) shouldBe empty
    }
  }

  // Which Polish page lists which venue is data (`models.PolishPages`), not the
  // group a scraper happens to be wired in here: a major city scrapes only its
  // own venues, and a venue outside every major city is scraped under the town
  // or cluster page it is on. Each still comes off its own source.
  it should "file every Polish scraper under the page its cinema is on" in {
    val c = catalog(biletyna = "kino-kameralne")
    for ((slug, scrapers) <- c.byCity; s <- scrapers if City.forCinema(s.cinema).exists(_.country == Country.Poland))
      withClue(s"${s.cinema.displayName}: ") { City.forCinema(s.cinema).value.slug shouldBe slug }
  }

  it should "scrape a suburb's venue under its own page, not the major city it used to be folded into" in {
    val c = catalog(biletyna = "kino-kameralne")
    c.byCity("warszawa").map(_.cinema) should (contain (KinoMuranow) and not contain (MultikinoPruszkow))
    c.byCity("pruszkow").find(_.cinema == MultikinoPruszkow).value shouldBe a [MultikinoClient]
    c.byCity(pageOf(HeliosSiedlce)).find(_.cinema == HeliosSiedlce).value shouldBe a [HeliosClient]
    c.byCity(pageOf(KinoWCKWalcz)).find(_.cinema == KinoWCKWalcz).value shouldBe a [Bilety24OrganizerClient]
    c.byCity(pageOf(KinoGiewont)).find(_.cinema == KinoGiewont).value shouldBe a [BiletynaClient]
  }

  // `MonitoringHttpFetch` suppresses these hosts so cinema scrapes don't
  // double-record under their host in the uptime page's "Other" bucket. A
  // scraper that returns an EMPTY set leaks its host into "Other" — exactly the
  // bug this guards. The abstract `CinemaScraper.scrapeHosts` makes the compiler
  // demand the method; this makes the runtime demand it be non-trivial.
  it should "declare a non-empty scrapeHosts for every scraper" in {
    val leaking = catalog(biletyna = "kino-kameralne").all.filter(_.scrapeHosts.isEmpty).map(_.cinema.displayName)
    withClue(s"scrapers with empty scrapeHosts: ${leaking.sorted}") { leaking shouldBe empty }
  }

  // Spot-check the union covers the host shapes that were leaking into "Other":
  // bespoke per-cinema domains, shared national-chain hosts, and the per-venue
  // bilety24 subdomains. A missed client or a re-spelled host fails here.
  it should "cover representative bespoke, shared-chain and per-venue hosts" in {
    val hosts = catalog(biletyna = "kino-kameralne").scrapeHosts
    val expected = Set(
      "kinomuranow.pl", "amok.gliwice.pl", "stacjafalenica.pl",   // bespoke per-cinema
      "www.multikino.pl", "www.cinema-city.pl", "restapi.helios.pl", "www.filmweb.pl", "www.novekino.pl", // shared chains
      "kinoluna.bilety24.pl", "www.bilety24.pl",   // legacy per-venue bilety24 subdomain + migrated organizer host
    )
    withClue(s"missing from catalog.scrapeHosts: ${(expected diff hosts).toSeq.sorted}") {
      (expected diff hosts) shouldBe empty
    }
  }

  // Both chain detail caches refresh on the same 2h beat, and the number is not
  // arbitrary: it has to expire INSIDE the DetailEnrich window (see the invariant
  // below). Cinema City was 6h — exactly the window — so whether a scheduled
  // refresh did any work depended on which timer won.
  it should "cache both chains' detail for 2h" in {
    val built = catalog(biletyna = "kino-kameralne")
    built.heliosDetailTtl shouldBe 2.hours
    built.cinemaCityDetailTtl shouldBe 2.hours
  }

  it should "expose only bare lower-case hosts (no scheme, port or path)" in {
    catalog(biletyna = "kino-kameralne").scrapeHosts.foreach { h =>
      withClue(s"malformed host: '$h'") {
        h shouldBe h.toLowerCase
        h should not include "/"
        h should not include ":"
      }
    }
  }

  /** Each chain's detail cache carries its own TTL, and under
   *  the worker that cache is a Mongo collection whose TTL index is named for the
   *  collection. Two chains asking for one store therefore means one expiry silently
   *  loses — Mongo rejects the second `createIndex` with `IndexOptionsConflict` and
   *  `MongoCachingDetailFetch` logs it and carries on. Whatever the chains are, distinct
   *  TTLs must come with distinct cache names. */
  /** EVERY chain detail TTL must expire before the refresh window it sits in
   *  front of. Detail is re-fetched once per `FreshnessKind.DetailEnrich` window;
   *  a cache that outlives that window answers the refresh from its own copy, so
   *  the fetch re-parses bytes it already had, produces the identical detail, and
   *  stamps `lastFetchedAt = now` — a refresh that cannot observe a change,
   *  recorded as though it had.
   *
   *  `CachingDetailFetch.DefaultTtl` is pinned this way in its own spec (12h over
   *  a 6h window was exactly that bug). The CHAIN TTLs are set here instead, and
   *  were never covered: Cinema City sat at 6h, EQUAL to the window, which is the
   *  same defect decided by a race rather than by arithmetic. */
  it should "give every chain a detail TTL that expires inside the refresh window" in {
    val window = Freshness.ttlFor(FreshnessKind.DetailEnrich)
      .getOrElse(fail("DetailEnrich lost its TTL; the chain detail TTLs are defined against it"))
    val requested = scala.collection.mutable.ListBuffer.empty[(String, FiniteDuration)]
    new CinemaScraperCatalog(
      http, mkFetch = http, bnFetch = http, today = LocalDate.of(2026, 6, 6),
      chainDetailCache = (chain, h, ttl) => { requested += (chain -> ttl); new CachingDetailFetch(h, ttl) },
      zyteFetch = http, flicksFetch = http, vueFetch = http, odeonFetch = http,
      odeonAuthToken = () => None, titles = titleNormalizer)

    requested should not be empty
    val tooLong = requested.filter { case (_, ttl) => ttl >= window }
    withClue(s"these chain detail caches outlive the $window refresh window, so a scheduled " +
      s"refresh is answered from cache: ${tooLong.mkString(", ")} — ") {
      tooLong shouldBe empty
    }
  }

  it should "never let two chains with different detail TTLs share one cache" in {
    val requested = scala.collection.mutable.ListBuffer.empty[(String, FiniteDuration)]
    new CinemaScraperCatalog(
      http, mkFetch = http, bnFetch = http, today = LocalDate.of(2026, 6, 6),
      chainDetailCache = (chain, h, ttl) => { requested += (chain -> ttl); new CachingDetailFetch(h, ttl) },
      zyteFetch = http, flicksFetch = http, vueFetch = http, odeonFetch = http, odeonAuthToken = () => None, titles = titleNormalizer)

    requested.size should be > 1
    withClue(s"chains built: ${requested.mkString(", ")} — ") {
      requested.groupBy(_._1).foreach { case (chain, entries) =>
        withClue(s"chain '$chain' asked for ${entries.size} different TTLs: ") {
          entries.map(_._2).distinct should have size 1
        }
      }
    }
  }

  // The EnrichDetails handler looks an enricher up BY `detailGroup` and writes the
  // fetched detail to THAT enricher's `detailTarget`. Two standalone venues sharing a
  // group therefore collapse to whichever the map kept last, and every other venue's
  // film detail lands on that one cinema's slot: every bilety24-hosted film grew a
  // phantom "Janosik" (Żywiec) cinema, in some arrival orders and not others — found by
  // HardClusterConvergenceIntegrationSpec as a Bałtyk (Rybnik) listing.
  it should "never let two detail enrichers that write to different targets share a detail group" in {
    import services.cinemas.common.DetailEnricher
    val clashes = catalog().all.collect { case de: DetailEnricher => de }
      .groupBy(_.detailGroup).toSeq.sortBy(_._1)
      .collect { case (group, enrichers) if enrichers.map(_.detailTarget).distinct.size > 1 =>
        s"$group -> ${enrichers.map(_.detailTarget.displayName).distinct.sorted.take(4).mkString(", ")}" }
    clashes shouldBe empty
  }
}
