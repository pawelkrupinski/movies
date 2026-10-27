package tools

import testsupport.TestMessages.given

import models.City

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.time.LocalDateTime
import java.util.concurrent.CountDownLatch

/**
 * Boots the same fixture corpus + Twirl-rendered routes that
 * `PageJsBehaviourSpec` serves over `TestHttpServer`, but as a
 * standalone main so the Playwright suite (which can't share JVM
 * state with the Scala test runner) can hit it over HTTP from a
 * separate process.
 *
 * Usage (one terminal):
 *   sbt 'PageTest/runMain tools.FixtureServerMain /temporary/kinowo-port.txt'
 *
 * Then in another (or the same CI job):
 *   PORT=$(cat /temporary/kinowo-port.txt)
 *   KINOWO_BASE_URL=http://127.0.0.1:$PORT npx playwright test
 *
 * Why a port file: the server binds to port 0 (kernel-assigned free
 * port) so multiple jobs on the same runner can't collide. The caller
 * needs to know which port; printing it via the file is more robust
 * than parsing sbt's chatty stdout.
 */
object FixtureServerMain {

  // Fixture corpus is anchored at the `08-06-2026` snapshot, same as
  // `PageJsBehaviourSpec`. `now` lives at midnight that day so the
  // controller's "future-only" filter doesn't drop showings before any
  // test sees them — independent of the wall-clock when CI runs.
  private val now = LocalDateTime.of(2026, 6, 8, 0, 0)

  /** Every landing page this server serves, keyed by the path it answers on.
   *
   *  BUILT HERE RATHER THAN INLINE IN `main`, so the routes and the spec read
   *  the same values and cannot drift. That drift is exactly what happened
   *  before: the union render lived in a method only the spec called while the
   *  `/` route built its own string, so narrowing the route left
   *  `FixtureServerLandingSpec` green over a page nothing served — the second
   *  time the same class of regression got through, and the first time it did so
   *  with a spec supposedly guarding it.
   *
   *  `/` is the DEFAULT country's own list, exactly as a deployment serves it.
   *  The per-country paths are fixture-only, for the browser specs written
   *  against a country this server is not defaulting to; production has one
   *  country per deployment and so only ever one of these shapes.
   */
  private[tools] def landings(picker: controllers.PickerFragments = controllers.PickerFragments.rendered()): Map[String, String] = Map(
    "/"             -> views.html.landing(models.Country.default, isApex = false, picker).body,
    "/landing-us"   -> views.html.landing(models.Country.UnitedStates, isApex = false, picker).body,
    "/landing-uk"   -> views.html.landing(models.Country.UnitedKingdom, isApex = false, picker).body,
    "/landing-de"   -> views.html.landing(models.Country.Germany, isApex = false, picker).body,
    "/landing-apex" -> views.html.landing(models.Country.default, isApex = true, picker).body,
  )

  def main(args: Array[String]): Unit = {
    val portFile = args.headOption.map(Paths.get(_)).getOrElse {
      System.err.println("usage: FixtureServerMain <port-file>")
      sys.exit(1)
    }

    System.err.println("[FixtureServerMain] booting FixtureTestWiring(\"08-06-2026\")…")
    val wiring = new FixtureTestWiring("08-06-2026")
    // Load the checked-in read-model snapshot (≈instant) instead of recomputing
    // the ~110s corpus pipeline — see ReadModelSnapshot. Falls back to the full
    // boot if the snapshot is missing.
    wiring.bootFromSnapshotOrPipeline()

    // The read transform is the web app's, built from the read model the worker
    // projected (the seam the two apps share in production).
    val service = new controllers.MovieControllerService(wiring.webReadModel, wiring.clock)

    // Render with a non-empty oauthProviders set so the Twirl
    // `@if(oauthProviders.nonEmpty)` branches surface the anon-nag
    // toast + Zaloguj się pill in the navbar. The Scala spec uses an
    // empty set + injects the Zaloguj się pill manually; Playwright
    // tests get the production-shaped DOM directly so flows like the
    // anonymous-nag toast lifecycle can be tested end-to-end.
    val oauthConfigured = Set("google")

    // Every page is served city-scoped under `/{city}/…`, mirroring
    // production's hard-cut routing. The fixture corpus is Poznań's; the other
    // cities resolve to empty schedules (no scrapers wired in tests), exactly
    // as production serves a not-yet-populated city. The listing page (`/`,
    // `/movies`) steps the selected day on a horizontal swipe — there's no
    // longer a separate Kina page or in-place view swap.
    def schedulesFor(c: City) = service.toSchedules(c, now)

    def indexPageFor(c: City): String = {
      implicit val ci: City = c
      views.html.repertoire(schedulesFor(c), c.cinemaDisplayNames, c.cinemaPillMap, minifier = tools.Minify, pinnedToday = wiring.pinnedToday,
        oauthProviders = oauthConfigured, renderedAt = now).body
    }
    def browsePageFor(c: City): String = {
      implicit val ci: City = c
      views.html.browse(schedulesFor(c), "Filmy", minifier = tools.Minify, pinnedToday = wiring.pinnedToday, oauthProviders = oauthConfigured).body
    }
    // Mirrors `MovieController.filmBySlug`: re-slug the corpus's titles and
    // match, since the slug fold is lossy and can't be reversed.
    def filmPageFor(c: City, slug: String): String = {
      implicit val ci: City = c
      schedulesFor(c).find(s => tools.Slugify(s.movie.title) == slug) match {
        case Some(s) =>
          views.html.film(s, s"http://test.local/${c.slug}/movie/$slug",
            ogDescription = "", minifier = tools.Minify, pinnedToday = wiring.pinnedToday).body
        case None    => "<html><body>Film not found</body></html>"
      }
    }

    // `/{city}/movie-many` — the same film page, re-seated at 12 cinemas on one
    // date so the Playwright suite can drive /movie's cinema fold. No corpus
    // film reaches the ten-cinema threshold on its own. `/{city}/movie-no-poster`
    // is the same page with no poster at all, which no corpus film lacks either.
    // `/{city}/movie-other-cities` is the first film's page with two sibling-city
    // links, so the suite can drive the "other cities" popup: only Poznań
    // carries a corpus here, so no film is ever showing anywhere else.
    def shapedFilmPageFor(c: City, path: String, shape: controllers.FilmSchedule => controllers.FilmSchedule,
                          otherCities: Seq[(City, String)] = Nil): String = {
      implicit val ci: City = c
      // Only Poznań carries a corpus in this harness; the other cities render
      // the same empty-handed page a real unpopulated city would.
      schedulesFor(c).headOption match {
        case Some(base) => views.html.film(shape(base),
          s"http://test.local/${c.slug}$path", ogDescription = "", minifier = tools.Minify, pinnedToday = wiring.pinnedToday,
          otherCities = otherCities).body
        case None       => "<html><body>Film not found</body></html>"
      }
    }

    // `/{city}/many-showtimes` — the listing re-seated with 10,500 synthetic
    // showtimes on one film so it crosses
    // `MovieControllerService.LargeCityShowtimeThreshold`, driving the day
    // carousel's instant-swap path (shared.js `usesInstantDayChange`). No
    // corpus city in this fixture is remotely that size on its own.
    def manyShowtimesPageFor(c: City): String = {
      implicit val ci: City = c
      val base    = schedulesFor(c)
      val bumped  = ManyShowtimesCity(base, now)
      val isLarge = controllers.MovieControllerService.totalShowtimes(bumped) >
        controllers.MovieControllerService.LargeCityShowtimeThreshold
      views.html.repertoire(bumped, c.cinemaDisplayNames, c.cinemaPillMap, minifier = tools.Minify, pinnedToday = wiring.pinnedToday,
        oauthProviders = oauthConfigured, renderedAt = now, isLargeCity = isLarge).body
    }

    // `/{city}/us-clock` — the listing as a US deployment prints it: the city's corpus on a
    // US city's 12-hour clock ("7:30 PM", `ClockStyle.TwelveHour`), so the browser suite can
    // drive the pills and the from-hour picker a US visitor gets. Only Poland carries a corpus.
    def usClockPageFor(c: City): String = {
      implicit val ci: City = models.Country.UnitedStates.cities.find(_.zoneId == models.TimeZones.UsEastern)
        .getOrElse(sys.error("no Eastern US city"))
      views.html.repertoire(schedulesFor(c), c.cinemaDisplayNames, c.cinemaPillMap, minifier = tools.Minify,
        pinnedToday = wiring.pinnedToday, oauthProviders = oauthConfigured, renderedAt = now).body
    }

    // The city-selection screens, from `landings()` so this route table and
    // `FixtureServerLandingSpec` read the same strings.
    //
    // Production serves TWO screens at `/` — a country picker when the request
    // Host is the bare showtimes.cc apex, the city picker otherwise (see
    // LandingController) — and this harness only ever renders the second,
    // because its routes are keyed on the path alone and have no request to read
    // a Host off. The apex branch is covered where the decision actually lives,
    // in controllers.LandingApexSpec.
    //
    // The per-country paths beside it are fixture-only, like `/{city}/movie-many`,
    // and all three exist because they differ in exactly the places a browser
    // spec is for: the US nests ONE level, 461 metros under 55 states, with seven
    // states that are a place at once; Germany nests one, 158 regions under 16
    // Bundesländer, and is the only roster whose names carry umlauts; the UK
    // nests TWO, its places under a county under a nation — the only one with a
    // heading inside a heading.
    val pickerFragments = controllers.PickerFragments.rendered()
    val landingPages: Map[String, String] = landings(pickerFragments)
    // The other countries' picker panes those landings fetch once they are up.
    val pickerApi = new controllers.PickerController(play.api.test.Helpers.stubControllerComponents(), pickerFragments)
    // The country-agnostic update-gate rule the iOS LocalServer suite decodes.
    val clientSupportApi = new controllers.ClientSupportController(play.api.test.Helpers.stubControllerComponents())

    // Resolve `/{cc}/{city}/…` — a city of the country with that code, as the
    // deployed sites are mounted — or the bare `/{city}/…` the specs have always
    // used, to (City, in-city sub-path). A slug is only a country's own, so the
    // bare form takes the first country in `Country.all`'s order that has the slug
    // (`/london/` is the UK's London; `/us/london/` Kentucky's). An unknown first
    // segment → None.
    def resolve(p: String): Option[(City, String)] = {
      def segment(path: String) = path.stripPrefix("/").takeWhile(ch => ch != '/' && ch != '?')
      def within(country: models.Country, path: String): Option[(City, String)] = {
        val slug = segment(path)
        country.bySlug.get(slug).map { c =>
          val prefix = "/" + slug
          (c, if (path == prefix) "/" else path.stripPrefix(prefix))
        }
      }
      val first = segment(p)
      models.Country.byCode(first).filter(_ => p.startsWith(s"/$first/")).flatMap(c => within(c, p.stripPrefix(s"/$first")))
        .orElse(models.Country.all.view.flatMap(within(_, p)).headOption)
    }

    // The page a `/{city}/…` sub-path renders, or None — so that an in-city path
    // this table does not know is a 404, not a `MatchError` the server reports
    // as a 500. That is how a mobile contract test asking for an API route this
    // file had not been taught read as "bad server response" on 2026-09-06.
    def pageFor(c: City, sub: String): Option[String] = sub match {
      case s if s == "/"     || s.startsWith("/?")     => Some(indexPageFor(c))
      case "/movies"                                    => Some(indexPageFor(c))
      case s if s.startsWith("/movies?") &&
                 (s.contains("country=") || s.contains("director=") || s.contains("cast=")) => Some(browsePageFor(c))
      case s if s.startsWith("/movies?")                => Some(indexPageFor(c))
      case "/movie-many"                                => Some(shapedFilmPageFor(c, "/movie-many", ManyCinemaFilm(_)))
      case "/movie-no-poster"                           => Some(shapedFilmPageFor(c, "/movie-no-poster", ManyCinemaFilm.withoutPoster))
      case "/movie-other-cities"                        => Some(shapedFilmPageFor(c, "/movie-other-cities", identity, OtherCitiesFixture.links))
      case "/many-showtimes"                             => Some(manyShowtimesPageFor(c))
      case "/us-clock"                                  => Some(usClockPageFor(c))
      case s if s.startsWith("/movie/")                 => Some(filmPageFor(c, s.stripPrefix("/movie/")))
      case _                                            => None
    }

    val routes: PartialFunction[String, String] = Function.unlift { p =>
      // Bare `/` → the city-selection landing (hard-cut: not a repertoire page);
      // everything else under `/{city}/…`. Each route tolerates a `?…` suffix.
      landingPages.get(p.takeWhile(_ != '?'))
        .orElse(resolve(p).flatMap { case (c, sub) => pageFor(c, sub) })
    }

    // The three JSON endpoints the mobile apps consume — the Android `KinowoApi`
    // and iOS `RepertoireStore` / `DetailsStore` / cinema filter all decode these.
    // Answered by the production `MovieController` actions themselves, over the
    // same read model the HTML routes render, so the mobile LocalServer suites
    // see the real wire: the per-film JSON cache and array writer, `?days=`,
    // and the server's own `Last-Modified` / `ETag` / 304 — which a route
    // projecting `ApiFilm` itself, stamping a constant date and ignoring
    // `If-Modified-Since`, never let them assert on.
    val api = controllers.TestMovieController.build(
      Nil, readModel = Some(wiring.webReadModel),
      clock = java.time.Clock.fixed(now.atZone(models.Country.default.zone).toInstant, java.time.ZoneOffset.UTC))._1

    def apiAction(c: City, sub: String, request: play.api.mvc.RequestHeader): Option[play.api.mvc.Action[play.api.mvc.AnyContent]] =
      sub.takeWhile(_ != '?') match {
        case "/api/repertoire" => Some(api.apiRepertoire(c.slug, request.getQueryString("days").flatMap(_.toIntOption)))
        case "/api/details"    => Some(api.apiDetails(c.slug))
        case "/api/cinemas"    => Some(api.apiCinemas(c.slug))
        case _                 => None
      }

    def apiRoute(exchange: com.sun.net.httpserver.HttpExchange): Boolean = {
      val uri     = exchange.getRequestURI.toString
      val request = play.api.test.FakeRequest("GET", uri).withHeaders(
        Seq("If-Modified-Since", "If-None-Match").flatMap(name => Option(exchange.getRequestHeaders.getFirst(name)).map(name -> _))*)
      val path    = exchange.getRequestURI.getPath
      val action  =
        if (path.startsWith("/api/picker/")) Some(pickerApi.fragment(path.stripPrefix("/api/picker/"), request.getQueryString("v")))
        else if (path == "/api/client-support") Some(clientSupportApi.clientSupport())
        else resolve(path).flatMap { case (c, sub) => apiAction(c, sub, request) }
      action match {
        case None         => false
        case Some(action) =>
          import play.api.test.Helpers.{contentAsBytes, contentType, defaultAwaitTimeout, headers, status}
          val result = action(request)
          headers(result).foreach { case (name, value) => exchange.getResponseHeaders.add(name, value) }
          val body = contentAsBytes(result).toArray
          if (body.isEmpty) exchange.sendResponseHeaders(status(result), -1)
          else {
            contentType(result).foreach(ct => exchange.getResponseHeaders.add("Content-Type", s"$ct; charset=UTF-8"))
            exchange.sendResponseHeaders(status(result), body.length.toLong)
            val os = exchange.getResponseBody
            try os.write(body) finally os.close()
          }
          true
      }
    }

    val server = new TestHttpServer(routes, dynamicRoute = apiRoute)

    Files.write(portFile, server.port.toString.getBytes(StandardCharsets.UTF_8))
    System.err.println(s"[FixtureServerMain] listening on ${server.baseUrl} — wrote port $portFile")

    // Block until the JVM is signalled. A shutdown hook closes the
    // server cleanly so the embedded `HttpServer` doesn't keep the
    // process alive after Ctrl-C / CI kill.
    val shutdown = new CountDownLatch(1)
    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      System.err.println("[FixtureServerMain] shutting down")
      try server.close() catch { case _: Throwable => () }
      try Files.deleteIfExists(portFile) catch { case _: Throwable => () }
      shutdown.countDown()
    }, "fixture-server-shutdown"))

    shutdown.await()
  }
}
