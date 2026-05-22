package tools

import models.Cinema

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.time.LocalDateTime
import java.util.Base64

/**
 * Screenshot generator for the mobile-scale refactor. Loads each
 * navbar-bearing fixture page (`/`, `/kina`, `/film?title=…`) in a
 * real headless Chrome, resizes the viewport through every entry in
 * `Viewports`, and writes one PNG per (page, width) pair to
 * `page/screenshots/<slug>-<width>px.png`.
 *
 * Mirrors the boot sequence in `views.PageJsBehaviourSpec` —
 * `FixtureTestWiring` produces the same Twirl-rendered HTML the
 * assertion-based spec drives, so the screenshots and the layout-
 * sweep test see byte-identical markup.
 *
 * Usage: `sbt "PageTest/runMain tools.MobileScreenshots"`.
 *
 * Re-runnable diagnostic, not a one-shot — re-run any time the
 * stylesheet or the mobile-scale formula changes to refresh the
 * captures. Prints every PNG path it wrote (per the project rule:
 * scripts must print what they did).
 */
object MobileScreenshots {

  /** Same widths the layout-sweep test asserts against. Keep in sync. */
  private val Viewports: Seq[Int] = Seq(360, 375, 390, 412, 430, 540, 575)

  /** Tall enough that the navbar, top of the grid and a row or two of
   *  cards all land inside the same screenshot — short enough to keep
   *  the PNG payloads small. The page itself can scroll past this; the
   *  screenshot only captures the viewport at this height. */
  private val ViewportHeight: Int = 900

  private val now = LocalDateTime.of(2026, 5, 17, 0, 0)

  def main(args: Array[String]): Unit = {
    val outDir: Path = if (args.nonEmpty) Paths.get(args(0))
                      else Paths.get("page/screenshots")
    Files.createDirectories(outDir)

    val chrome = Chrome.tryStart().getOrElse {
      System.err.println("Chrome not installed — install Google Chrome or Chromium and retry.")
      sys.exit(2)
    }

    try {
      val wiring = new FixtureTestWiring("17-05-2026")
      wiring.bootStartup()
      val cinemas  = Cinema.all.map(_.displayName)
      val schedules       = wiring.movieControllerService.toSchedules(now)
      val cinemaSchedules = wiring.movieControllerService.toCinemaSchedules(now)
      val anon    = Option.empty[models.User]
      val noOauth = Set.empty[String]
      val noFav   = Set.empty[String]

      val indexHtml: String = views.html.repertoire(
        schedules, cinemas, devMode = false,
        currentUser    = anon, oauthProviders = noOauth,
        favouriteMovies = noFav, favouriteScreenings = noFav,
        favouritesMode = false
      ).body

      val kinaHtml: String = views.html.kina(
        cinemaSchedules, cinemas, devMode = false,
        currentUser    = anon, oauthProviders = noOauth,
        favouriteMovies = noFav, favouriteScreenings = noFav,
        pinnedCinema = None
      ).body

      // Pick a well-populated film from the fixture corpus — Diabeł ubiera
      // się u Prady 2 has multiple cinemas, a trailer-link row, and a
      // long-ish Polish title, so the screenshot exercises every chrome
      // axis (.film-title wrap, .cinema-link pill row, .trailer-link).
      val filmTitle = "Diabeł ubiera się u Prady 2"
      val filmSchedule = schedules.find(_.movie.title == filmTitle).getOrElse {
        System.err.println(s"Fixture missing expected film: '$filmTitle'")
        sys.exit(3)
      }
      val filmHtml: String = views.html.film(
        filmSchedule, "http://test.local/film", "", isFavourite = false,
        favouriteScreenings = noFav, devMode = false
      ).body
      val filmQuery = "/film?title=" +
        java.net.URLEncoder.encode(filmTitle, "UTF-8")

      val server = new TestHttpServer({
        case "/"     => indexHtml
        case "/kina" => kinaHtml
        case `filmQuery` => filmHtml
      })
      try {
        val pages: Seq[(String, String)] = Seq(
          "index" -> "/",
          "kina"  -> "/kina",
          "film"  -> filmQuery
        )
        val t0 = System.currentTimeMillis()
        var count = 0
        for ((slug, path) <- pages) {
          chrome.openPage(server.baseUrl + path) { page =>
            // Inject the Zaloguj-się pill so the screenshot matches the
            // production anonymous-user navbar (the fixture renders with
            // no OAuth providers configured, leaving `.navbar-auth`
            // empty). /film doesn't have `.navbar-auth` — the IIFE is a
            // no-op there because the querySelector returns null.
            page.eval(
              "(() => { const a = document.querySelector('.navbar-auth');" +
              "          if (a && !a.children.length) {" +
              "            const btn = document.createElement('button');" +
              "            btn.type = 'button';" +
              "            btn.className = 'nav-tab nav-tab-login';" +
              "            btn.textContent = 'Zaloguj się';" +
              "            a.appendChild(btn);" +
              "          } })()"
            )
            for (w <- Viewports) {
              page.setViewport(w, ViewportHeight)
              // setDeviceMetricsOverride is synchronous, but a beat lets
              // resize-listeners + flex reflow settle before the capture.
              Thread.sleep(80L)
              val pngBase64 = page.screenshot()
              val out = outDir.resolve(s"$slug-$w" + "px.png")
              Files.write(out, Base64.getDecoder.decode(pngBase64),
                StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
              val bytes = Files.size(out)
              println(f"$slug%-5s $w%3d px → ${out} (${bytes / 1024}%4d KiB)")
              count += 1
            }
          }
        }
        val dtMs = System.currentTimeMillis() - t0
        println(f"done in ${dtMs / 1000.0}%.1fs, $count captures")
      } finally server.close()
    } finally chrome.close()
  }
}
