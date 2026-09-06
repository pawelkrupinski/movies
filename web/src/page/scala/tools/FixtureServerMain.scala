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
  private[tools] def landings(): Map[String, String] = Map(
    "/"           -> views.html.landing(models.Country.default).body,
    "/landing-us" -> views.html.landing(models.Country.UnitedStates).body,
    "/landing-uk" -> views.html.landing(models.Country.UnitedKingdom).body,
    "/landing-de" -> views.html.landing(models.Country.Germany).body,
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
    val service = new controllers.MovieControllerService(wiring.webReadModel)

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
      views.html.repertoire(schedulesFor(c), c.cinemaDisplayNames, c.cinemaPillMap, devMode = false,
        oauthProviders = oauthConfigured, renderedAt = now).body
    }
    def browsePageFor(c: City): String = {
      implicit val ci: City = c
      views.html.browse(schedulesFor(c), "Filmy", devMode = false, oauthProviders = oauthConfigured).body
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
    val landingPages: Map[String, String] = landings()

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
      case p if landingPages.contains(p.takeWhile(_ != '?')) =>
        landingPages(p.takeWhile(_ != '?'))
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
