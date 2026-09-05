package tools

import testsupport.TestMessages.given

import controllers.{ApiFilm, ApiFilmDetails}
import models.City
import play.api.libs.json.Json

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

  /** The bare `/` landing (place-selection screen). Production serves TWO screens
   *  here — a country picker when the request Host is the bare showtimes.cc apex,
   *  the place picker otherwise (see `LandingController`) — and this harness only
   *  ever renders the second, because its routes are keyed on the path alone and
   *  have no request to read a Host off. The apex branch is covered where the
   *  decision actually lives, in `controllers.LandingApexSpec`.
   *
   *  IT LISTS `City.all`, THE UNION ACROSS EVERY COUNTRY, which no real deployment
   *  ever renders — a Polish visitor is offered Polish cities and nothing else.
   *  This harness serves ONE `/` to browser specs written for four countries at
   *  once (`city-select.spec.ts` clicks Poznań, München AND California on this one
   *  page), so a country-scoped list leaves three of them with nothing to click.
   *  The COPY still comes from the default country, which is why those specs read
   *  Polish nouns off the pages they land on.
   *
   *  A METHOD, SO A SPEC CAN CALL IT. The union was asserted nowhere but a
   *  Playwright spec several CI shards away, so narrowing it to one country
   *  compiled, passed every Scala layer, and surfaced as a 30s browser timeout
   *  with nothing in it naming a fixture server; `FixtureServerLandingSpec` now
   *  says it in two seconds.
   */
  private[tools] def renderLanding(): String =
    views.html.landing(models.Country.default, Some(City.all)).body

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
    val service = new controllers.MovieControllerService(wiring.webReadModel)

    val anon    = Option.empty[models.User]
    // Render with a non-empty oauthProviders set so the Twirl
    // `@if(oauthProviders.nonEmpty)` branches surface the anon-nag
    // toast + Zaloguj się pill in the navbar. The Scala spec uses an
    // empty set + injects the Zaloguj się pill manually; Playwright
    // tests get the production-shaped DOM directly so flows like the
    // anonymous-nag toast lifecycle can be tested end-to-end.
    val noOauth = Set("google")

    // Every page is served city-scoped under `/{city}/…`, mirroring
    // production's hard-cut routing. The fixture corpus is Poznań's; the other
    // cities resolve to empty schedules (no scrapers wired in tests), exactly
    // as production serves a not-yet-populated city. The listing page (`/`,
    // `/movies`) steps the selected day on a horizontal swipe — there's no
    // longer a separate Kina page or in-place view swap.
    def schedulesFor(c: City) = service.toSchedules(c, now)

    def indexPageFor(c: City): String = {
      implicit val ci: City = c
      views.html.repertoire(schedulesFor(c), c.cinemaDisplayNames, c.cinemaPillMap, devMode = false,
        currentUser = anon, oauthProviders = noOauth, renderedAt = now).body
    }
    def browsePageFor(c: City): String = {
      implicit val ci: City = c
      views.html.browse(schedulesFor(c), "Filmy", devMode = false, currentUser = anon, oauthProviders = noOauth).body
    }
    // Mirrors `MovieController.filmBySlug`: re-slug the corpus's titles and
    // match, since the slug fold is lossy and can't be reversed.
    def filmPageFor(c: City, slug: String): String = {
      implicit val ci: City = c
      schedulesFor(c).find(s => tools.Slugify(s.movie.title) == slug) match {
        case Some(s) =>
          views.html.film(s, s"http://test.local/${c.slug}/movie/$slug",
            ogDescription = "", devMode = false).body
        case None    => "<html><body>Film not found</body></html>"
      }
    }

    // `/{city}/movie-many` — the same film page, re-seated at 12 cinemas on one
    // date so the Playwright suite can drive /movie's cinema fold. No corpus
    // film reaches the ten-cinema threshold on its own.
    def manyCinemaFilmPageFor(c: City): String = {
      implicit val ci: City = c
      // Only Poznań carries a corpus in this harness; the other cities render
      // the same empty-handed page a real unpopulated city would.
      schedulesFor(c).headOption match {
        case Some(base) => views.html.film(ManyCinemaFilm(base),
          s"http://test.local/${c.slug}/movie-many", ogDescription = "", devMode = false).body
        case None       => "<html><body>Film not found</body></html>"
      }
    }

    // The bare `/` landing (city-selection screen). Production serves TWO screens
    // here — a country picker when the request Host is the bare showtimes.cc apex,
    // the city picker otherwise (see LandingController) — and this harness only
    // ever renders the second, because its routes are keyed on the path alone and
    // have no request to read a Host off. The apex branch is covered where the
    // decision actually lives, in controllers.LandingApexSpec.
    val landingHtml: String = views.html.landing(models.Country.default).body

    // The same screen for a country whose place list is GROUPED — the US, whose
    // metros are found under their state's heading, and the UK, whose counties
    // are found under their nation's. Fixture-only paths, like
    // `/{city}/movie-many`: production serves one country per deployment, so its
    // `/` can only ever be one of the two shapes, and the grouped one is not the
    // shape this harness's default country has.
    //
    // Both, not one standing for the other: they differ in exactly the place a
    // browser spec is for. The US groups 461 metros under 55 states and has seven
    // that are a state and a place at once; the UK groups 79 counties under five
    // nations and has none.
    val usLandingHtml: String = views.html.landing(models.Country.UnitedStates).body
    val ukLandingHtml: String = views.html.landing(models.Country.UnitedKingdom).body

    // Resolve `/{city}/…` to (City, in-city sub-path). The first path segment
    // is matched against the known cities; an unknown first segment → None.
    def resolve(p: String): Option[(City, String)] = {
      val firstSeg = p.stripPrefix("/").takeWhile(ch => ch != '/' && ch != '?')
      City.bySlug(firstSeg).map { c =>
        val prefix = "/" + firstSeg
        (c, if (p == prefix) "/" else p.stripPrefix(prefix))
      }
    }

    val routes: PartialFunction[String, String] = {
      // Bare `/` → the city-selection landing (hard-cut: not a repertoire page).
      case p if p == "/" || p.startsWith("/?") => landingHtml
      case p if p == "/landing-us" || p.startsWith("/landing-us?") => usLandingHtml
      case p if p == "/landing-uk" || p.startsWith("/landing-uk?") => ukLandingHtml
      // Everything else under `/{city}/…`. Each route tolerates a `?…` suffix.
      case p if resolve(p).isDefined =>
        val (c, sub) = resolve(p).get
        sub match {
          case s if s == "/"     || s.startsWith("/?")     => indexPageFor(c)
          case "/movies"                                    => indexPageFor(c)
          case s if s.startsWith("/movies?") &&
                     (s.contains("country=") || s.contains("director=") || s.contains("cast=")) => browsePageFor(c)
          case s if s.startsWith("/movies?")                => indexPageFor(c)
          case "/movie-many"                                => manyCinemaFilmPageFor(c)
          case s if s.startsWith("/movie/") =>
            filmPageFor(c, s.stripPrefix("/movie/"))
        }
    }

    // The two JSON endpoints the mobile apps consume — the Android `KinowoApi`
    // and iOS `RepertoireStore` both decode these. Rendered from the same
    // fixture schedules the HTML routes use, via the production `ApiFilm` /
    // `ApiFilmDetails` projections, so a wire-shape drift in `MovieController`'s
    // JSON is caught by the mobile LocalServer suites.
    def repertoireJsonFor(c: City): String = Json.toJson(schedulesFor(c).map(ApiFilm.from)).toString
    def detailsJsonFor(c: City): String =
      Json.toJson(schedulesFor(c).map(ApiFilmDetails.from).filter(ApiFilmDetails.hasContent)).toString

    val jsonRoutes: PartialFunction[String, String] = {
      case p if resolve(p).exists(_._2.startsWith("/api/repertoire")) => repertoireJsonFor(resolve(p).get._1)
      case p if resolve(p).exists(_._2.startsWith("/api/details"))    => detailsJsonFor(resolve(p).get._1)
    }

    val server = new TestHttpServer(routes, jsonRoutes = jsonRoutes)

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
