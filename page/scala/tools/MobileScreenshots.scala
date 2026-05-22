package tools

import models.Cinema

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.time.LocalDateTime
import java.util.Base64

/**
 * Screenshot generator for the mobile-scale refactor. Loads the
 * fixture-rendered `/` page in a real headless Chrome, then resizes
 * the viewport through every entry in `Viewports` and writes a PNG
 * per width to `page/screenshots/<width>px.png`.
 *
 * Mirrors the boot sequence in `views.PageJsBehaviourSpec` —
 * `FixtureTestWiring` produces the same Twirl-rendered HTML the
 * assertion-based spec drives, so the screenshots and the
 * `wraps to ≤ 2 rows with zero horizontal overflow` test see byte-
 * identical markup.
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
      // Render the same `/` HTML the spec uses.
      val wiring = new FixtureTestWiring("17-05-2026")
      wiring.bootStartup()
      val cinemas  = Cinema.all.map(_.displayName)
      val schedules = wiring.movieControllerService.toSchedules(now)

      val indexHtml: String = views.html.repertoire(
        schedules, cinemas, devMode = false,
        currentUser    = Option.empty[models.User],
        oauthProviders = Set.empty,
        favouriteMovies     = Set.empty,
        favouriteScreenings = Set.empty,
        favouritesMode = false
      ).body

      val server = new TestHttpServer({ case "/" => indexHtml })
      try {
        chrome.openPage(server.baseUrl + "/") { page =>
          // Inject the Zaloguj-się pill — `oauthProviders = Set.empty`
          // leaves `.navbar-auth` empty in the rendered HTML; production
          // always carries the pill, so seed it before measuring layout
          // and capturing screenshots. Same trick the spec uses.
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

          val t0 = System.currentTimeMillis()
          for (w <- Viewports) {
            page.setViewport(w, ViewportHeight)
            // setDeviceMetricsOverride is synchronous, but a beat lets
            // resize-listeners + flex reflow settle before the capture.
            Thread.sleep(80L)
            val pngBase64 = page.screenshot()
            val out = outDir.resolve(s"$w" + "px.png")
            Files.write(out, Base64.getDecoder.decode(pngBase64),
              StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
            val bytes = Files.size(out)
            println(f"$w%3d px → ${out} (${bytes / 1024}%4d KiB)")
          }
          val dtMs = System.currentTimeMillis() - t0
          println(f"done in ${dtMs / 1000.0}%.1fs, ${Viewports.size} captures")
        }
      } finally server.close()
    } finally chrome.close()
  }
}
