package tools

import models.Cinema

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
    val cinemas = Cinema.all.map(_.displayName)
    val schedules       = wiring.movieControllerService.toSchedules(now)
    val cinemaSchedules = wiring.movieControllerService.toCinemaSchedules(now)

    val pills = Cinema.pillMap
    def renderKina(pinned: Option[String]): String = views.html.kina(
      cinemaSchedules, cinemas, pills, devMode = false,
      currentUser = anon, oauthProviders = noOauth,
      pinnedCinema = pinned
    ).body

    val indexHtml: String = views.html.repertoire(
      schedules, cinemas, pills, devMode = false,
      currentUser = anon, oauthProviders = noOauth
    ).body

    val filmyHtml: String = views.html.browse(
      schedules, "Filmy", devMode = false,
      currentUser = anon, oauthProviders = noOauth
    ).body

    val planHtml: String = {
      val data = controllers.PlanController.viewData(schedules)
      views.html.plan(data, cinemas, pills, devMode = false,
        currentUser = anon, oauthProviders = noOauth).body
    }

    def renderFilm(title: String): String = {
      val target = URLDecoder.decode(title, "UTF-8")
      schedules.find(_.movie.title == target) match {
        case Some(s) =>
          views.html.film(s, s"http://test.local/film?title=$title",
            ogDescription = "", devMode = false).body
        case None    => "<html><body>Film not found</body></html>"
      }
    }

    val server = new TestHttpServer({
      // Each top-level route accepts an arbitrary `?…` suffix — the real Play
      // routes ignore unknown query params on these paths, and the
      // day-selector ↔ URL Playwright tests boot directly with `?date=` in the
      // request URL. Without the query-tolerant match the JDK HttpServer
      // would 404 those requests, which Firefox surfaces as
      // `NS_ERROR_NET_EMPTY_RESPONSE` rather than a clean status code.
      case p if p == "/"      || p.startsWith("/?")       => indexHtml
      // `/filmy` and `/` are aliases for the main listing — both render
      // the repertoire view. `/filmy?kraj=X` / `/filmy?rezyser=Y` /
      // `/filmy?aktor=Z` still hit the per-axis browse view; everything
      // else under `/filmy?` is the main listing with filter state in
      // the query string.
      case p if p == "/filmy"                              => indexHtml
      case p if p.startsWith("/filmy?") &&
                 (p.contains("kraj=") || p.contains("rezyser=") || p.contains("aktor=")) => filmyHtml
      case p if p.startsWith("/filmy?")                    => indexHtml
      case p if p == "/kina"  || p.startsWith("/kina?")   => renderKina(None)
      case p if p == "/plan"  || p.startsWith("/plan?")   => planHtml
      case p if p.startsWith("/kina/") =>
        val rawWithQuery = p.stripPrefix("/kina/")
        val raw          = URLDecoder.decode(rawWithQuery.takeWhile(_ != '?'), "UTF-8")
        val pinned       = cinemas.find(_ == raw)
        renderKina(pinned)
      case p if p.startsWith("/film?title=") =>
        renderFilm(p.stripPrefix("/film?title="))
    })

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
