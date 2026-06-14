package tools

import controllers.{ApiFilm, ApiFilmDetails}
import models.City
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

  // Fixture corpus is anchored at the `08-06-2026` snapshot, same as
  // `PageJsBehaviourSpec`. `now` lives at midnight that day so the
  // controller's "future-only" filter doesn't drop showings before any
  // test sees them — independent of the wall-clock when CI runs.
  private val now = LocalDateTime.of(2026, 6, 8, 0, 0)

  def main(args: Array[String]): Unit = {
    val portFile = args.headOption.map(Paths.get(_)).getOrElse {
      System.err.println("usage: FixtureServerMain <port-file>")
      sys.exit(1)
    }

    System.err.println("[FixtureServerMain] booting FixtureTestWiring(\"08-06-2026\")…")
    val wiring = new FixtureTestWiring("08-06-2026")
    wiring.bootStartup()

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
    // `/filmy`) steps the selected day on a horizontal swipe — there's no
    // longer a separate Kina page or in-place view swap.
    def schedulesFor(c: City) = service.toSchedules(c, now)

    def indexPageFor(c: City): String = {
      implicit val ci: City = c
      views.html.repertoire(schedulesFor(c), c.cinemaDisplayNames, c.cinemaPillMap, devMode = false,
        currentUser = anon, oauthProviders = noOauth).body
    }
    def filmyPageFor(c: City): String = {
      implicit val ci: City = c
      views.html.browse(schedulesFor(c), "Filmy", devMode = false, currentUser = anon, oauthProviders = noOauth).body
    }
    def planPageFor(c: City): String = {
      implicit val ci: City = c
      views.html.plan(controllers.PlanController.viewData(c, schedulesFor(c)),
        c.cinemaDisplayNames, c.cinemaPillMap, devMode = false, currentUser = anon, oauthProviders = noOauth).body
    }
    def filmPageFor(c: City, title: String): String = {
      implicit val ci: City = c
      val target = URLDecoder.decode(title, "UTF-8")
      schedulesFor(c).find(_.movie.title == target) match {
        case Some(s) =>
          views.html.film(s, s"http://test.local/${c.slug}/film?title=$title",
            ogDescription = "", devMode = false).body
        case None    => "<html><body>Film not found</body></html>"
      }
    }

    // The bare `/` landing (city-selection screen), same as production.
    val landingHtml: String = views.html.landing(City.all).body

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
      // Everything else under `/{city}/…`. Each route tolerates a `?…` suffix.
      case p if resolve(p).isDefined =>
        val (c, sub) = resolve(p).get
        sub match {
          case s if s == "/"     || s.startsWith("/?")     => indexPageFor(c)
          case "/filmy"                                    => indexPageFor(c)
          case s if s.startsWith("/filmy?") &&
                     (s.contains("kraj=") || s.contains("rezyser=") || s.contains("aktor=")) => filmyPageFor(c)
          case s if s.startsWith("/filmy?")                => indexPageFor(c)
          case s if s == "/plan" || s.startsWith("/plan?") => planPageFor(c)
          case s if s.startsWith("/film?title=") =>
            filmPageFor(c, s.stripPrefix("/film?title="))
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
