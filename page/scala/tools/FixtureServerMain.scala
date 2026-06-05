package tools

import controllers.{ApiFilm, ApiFilmDetails}
import models.{City, Poznan}
import play.api.libs.json.Json

import java.net.URLDecoder
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
 *   sbt 'PageTest/runMain tools.FixtureServerMain /tmp/kinowo-port.txt'
 *
 * Then in another (or the same CI job):
 *   PORT=$(cat /tmp/kinowo-port.txt)
 *   KINOWO_BASE_URL=http://127.0.0.1:$PORT npx playwright test
 *
 * Why a port file: the server binds to port 0 (kernel-assigned free
 * port) so multiple jobs on the same runner can't collide. The caller
 * needs to know which port; printing it via the file is more robust
 * than parsing sbt's chatty stdout.
 */
object FixtureServerMain {

  // Fixture corpus is anchored at the `17-05-2026` snapshot, same as
  // `PageJsBehaviourSpec`. `now` lives at midnight that day so the
  // controller's "future-only" filter doesn't drop showings before any
  // test sees them — independent of the wall-clock when CI runs.
  private val now = LocalDateTime.of(2026, 5, 17, 0, 0)

  def main(args: Array[String]): Unit = {
    val portFile = args.headOption.map(Paths.get(_)).getOrElse {
      System.err.println("usage: FixtureServerMain <port-file>")
      sys.exit(1)
    }

    System.err.println("[FixtureServerMain] booting FixtureTestWiring(\"17-05-2026\")…")
    val wiring = new FixtureTestWiring("17-05-2026")
    wiring.bootStartup()

    val anon    = Option.empty[models.User]
    // Render with a non-empty oauthProviders set so the Twirl
    // `@if(oauthProviders.nonEmpty)` branches surface the anon-nag
    // toast + Zaloguj się pill in the navbar. The Scala spec uses an
    // empty set + injects the Zaloguj się pill manually; Playwright
    // tests get the production-shaped DOM directly so flows like the
    // anonymous-nag toast lifecycle can be tested end-to-end.
    val noOauth = Set("google")
    // Single supported city today; every page is rendered city-scoped and
    // served under `/poznan/…`, mirroring production's hard-cut routing.
    implicit val city: City = Poznan
    val cinemas = city.cinemaDisplayNames
    val schedules       = wiring.movieControllerService.toSchedules(city, now)
    val cinemaSchedules = wiring.movieControllerService.toCinemaSchedules(city, now)

    val pills = city.cinemaPillMap

    // `#view-root`-only fragments served for `X-Requested-With: view-swap`
    // requests — mirrors the production controller so the in-place swap (and
    // its tests) exercise the real "fetch just the fragment" path. The full
    // pages also embed the SIBLING fragment as the prefetch seed (same as the
    // controller), so the embed-seed test sees it.
    val indexFragment: String = views.html._repertoireView(schedules, devMode = false).body
    def renderKinaFragment(pinned: Option[String]): String =
      views.html._kinaView(cinemaSchedules, pinned, devMode = false).body

    def renderKina(pinned: Option[String]): String = views.html.kina(
      cinemaSchedules, cinemas, pills, devMode = false,
      currentUser = anon, oauthProviders = noOauth,
      pinnedCinema = pinned,
      siblingPath = s"/${city.slug}/", siblingHtml = indexFragment
    ).body

    val indexHtml: String = views.html.repertoire(
      schedules, cinemas, pills, devMode = false,
      currentUser = anon, oauthProviders = noOauth,
      siblingPath = s"/${city.slug}/kina", siblingHtml = renderKinaFragment(None)
    ).body

    val filmyHtml: String = views.html.browse(
      schedules, "Filmy", devMode = false,
      currentUser = anon, oauthProviders = noOauth
    ).body

    val planHtml: String = {
      val data = controllers.PlanController.viewData(city, schedules)
      views.html.plan(data, cinemas, pills, devMode = false,
        currentUser = anon, oauthProviders = noOauth).body
    }

    // The bare `/` landing (city-selection screen), same as production.
    val landingHtml: String = views.html.landing(City.all).body

    def renderFilm(title: String): String = {
      val target = URLDecoder.decode(title, "UTF-8")
      schedules.find(_.movie.title == target) match {
        case Some(s) =>
          views.html.film(s, s"http://test.local/${city.slug}/film?title=$title",
            ogDescription = "", devMode = false).body
        case None    => "<html><body>Film not found</body></html>"
      }
    }

    // Strip the `/{city}` prefix so the in-city arms below match the same
    // sub-paths production serves under `/poznan/…`. A bare `/` (or `/?…`) is
    // the landing screen, not a repertoire page — hard-cut, mirroring prod.
    val cityPrefix = s"/${city.slug}"
    def inCity(p: String): Option[String] =
      if (p == cityPrefix) Some("/")
      else if (p.startsWith(cityPrefix + "/") || p.startsWith(cityPrefix + "?")) Some(p.stripPrefix(cityPrefix))
      else None

    val routes: PartialFunction[String, String] = {
      // Bare `/` → the city-selection landing.
      case p if p == "/" || p.startsWith("/?") => landingHtml
      // Everything else must be under `/{city}/…`; strip and match. Each route
      // tolerates an arbitrary `?…` suffix (real Play ignores unknown query
      // params; Playwright boots directly with `?date=` etc.).
      case p if inCity(p).isDefined =>
        val sub = inCity(p).get
        sub match {
          case s if s == "/"     || s.startsWith("/?")     => indexHtml
          case "/filmy"                                    => indexHtml
          case s if s.startsWith("/filmy?") &&
                     (s.contains("kraj=") || s.contains("rezyser=") || s.contains("aktor=")) => filmyHtml
          case s if s.startsWith("/filmy?")                => indexHtml
          case s if s == "/kina" || s.startsWith("/kina?") => renderKina(None)
          case s if s == "/plan" || s.startsWith("/plan?") => planHtml
          case s if s.startsWith("/kina/") =>
            val raw = URLDecoder.decode(s.stripPrefix("/kina/").takeWhile(_ != '?'), "UTF-8")
            renderKina(cinemas.find(_ == raw))
          case s if s.startsWith("/film?title=") =>
            renderFilm(s.stripPrefix("/film?title="))
        }
    }

    // `#view-root`-only responses for view-swap requests — the swap-managed
    // routes (`/{city}/`, `/{city}/filmy`, `/{city}/kina`, `/{city}/kina/<cinema>`).
    val swapRoutes: PartialFunction[String, String] = {
      case p if inCity(p).isDefined && {
        val s = inCity(p).get
        s == "/" || s.startsWith("/?") || s == "/filmy" || s.startsWith("/filmy?") ||
          s == "/kina" || s.startsWith("/kina?") || s.startsWith("/kina/")
      } =>
        val s = inCity(p).get
        if (s.startsWith("/kina/")) {
          val raw = URLDecoder.decode(s.stripPrefix("/kina/").takeWhile(_ != '?'), "UTF-8")
          renderKinaFragment(cinemas.find(_ == raw))
        } else if (s == "/kina" || s.startsWith("/kina?")) renderKinaFragment(None)
        else indexFragment
    }

    // The two JSON endpoints the mobile apps consume — the Android `KinowoApi`
    // and iOS `RepertoireStore` both decode these. Rendered from the same
    // fixture `schedules` the HTML routes use, via the production
    // `ApiFilm` / `ApiFilmDetails` projections, so a wire-shape drift in
    // `MovieController`'s JSON is caught by the mobile LocalServer suites.
    val repertoireJson: String = Json.toJson(schedules.map(ApiFilm.from)).toString
    val detailsJson: String = Json.toJson(
      schedules.map(ApiFilmDetails.from).filter(ApiFilmDetails.hasContent)
    ).toString

    val jsonRoutes: PartialFunction[String, String] = {
      case p if inCity(p).contains("/api/repertoire") => repertoireJson
      case p if inCity(p).contains("/api/details")    => detailsJson
    }

    val server = new TestHttpServer(routes, swapRoutes, jsonRoutes)

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
