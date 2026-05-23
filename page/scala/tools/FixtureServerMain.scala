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
    val noFav   = Set.empty[String]
    val cinemas = Cinema.all.map(_.displayName)
    val schedules       = wiring.movieControllerService.toSchedules(now)
    val cinemaSchedules = wiring.movieControllerService.toCinemaSchedules(now)

    def renderKina(pinned: Option[String]): String = views.html.kina(
      cinemaSchedules, cinemas, devMode = false,
      currentUser = anon, oauthProviders = noOauth,
      favouriteMovies = noFav, favouriteScreenings = noFav,
      pinnedCinema = pinned
    ).body

    val indexHtml: String = views.html.repertoire(
      schedules, cinemas, devMode = false,
      currentUser = anon, oauthProviders = noOauth,
      favouriteMovies = noFav, favouriteScreenings = noFav,
      favouritesMode = false
    ).body

    // /ulubione re-renders `repertoire` with `favouritesMode = true`
    // so the page's inline `applyFilters` switches to the favourites
    // branch. Server side passes the same schedules; client side
    // narrows to `getFavMovies() ∪ getFavScreenings()` per the
    // localStorage state Playwright tests can seed.
    val ulubioneHtml: String = views.html.repertoire(
      schedules, cinemas, devMode = false,
      currentUser = anon, oauthProviders = noOauth,
      favouriteMovies = noFav, favouriteScreenings = noFav,
      favouritesMode = true
    ).body

    def renderFilm(title: String): String = {
      val target = URLDecoder.decode(title, "UTF-8")
      schedules.find(_.movie.title == target) match {
        case Some(s) =>
          views.html.film(s, s"http://test.local/film?title=$title",
            ogDescription = "", isFavourite = false,
            favouriteScreenings = noFav, devMode = false).body
        case None    => "<html><body>Film not found</body></html>"
      }
    }

    val server = new TestHttpServer({
      case "/"                          => indexHtml
      case "/ulubione"                  => ulubioneHtml
      case "/kina"                      => renderKina(None)
      case p if p.startsWith("/kina/") =>
        val raw    = URLDecoder.decode(p.stripPrefix("/kina/"), "UTF-8")
        val pinned = cinemas.find(_ == raw)
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
