package views

import models.Cinema
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{CdpPage, Chrome, FixtureTestWiring, TestHttpServer}

import java.net.URLDecoder
import java.time.LocalDateTime

/**
 * JavaScript-behaviour regression for the rendered pages. Spins up a
 * tiny embedded HTTP server in front of the same `17-05-2026` fixture-
 * rendered HTML the snapshot spec asserts on, drives the pages in a
 * headless Chrome over CDP, and asserts on actual DOM state after JS
 * interactions (pill click, search input, URL pinning, reload).
 *
 * Complements `PageSnapshotSpec`: that spec catches "the markup the
 * server emits" regressions; this spec catches "the JS that decides
 * what to show in that markup" regressions — `applyFilters` math, pill
 * toggle semantics, `_kinaPinned` lifecycle, the /kina ↔ /kina/<cinema>
 * URL ↔ pin sync via `history.replaceState`.
 *
 * Why an HTTP server and not file://: `history.replaceState` (called by
 * `toggleCinemaPill` to rewrite the URL on pin/un-pin) throws a
 * SecurityError under file:// — the target URL isn't same-origin with
 * the file's directory-scoped origin. Without http://, the rewrite
 * throws synchronously and the rest of the click handler (the
 * `buildCinemaPills()` / `applyFilters()` calls that update the DOM)
 * never runs, so every test would look broken even when prod is fine.
 *
 * Skips gracefully when Chrome isn't installed locally — CI images that
 * lack a browser get a green spec with `cancelled` tests; developers
 * with Chrome installed get the full coverage.
 */
class PageJsBehaviourSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private val now = LocalDateTime.of(2026, 5, 17, 0, 0)

  // One Chrome + one TestHttpServer for the whole spec. Each individual
  // test still gets a fresh tab (clean localStorage, fresh module state)
  // via `openPage`, but pays the cost of wiring + rendering + Chrome
  // boot exactly once across all tests.
  private var chrome: Option[Chrome] = None
  private var server: TestHttpServer = _

  override def beforeAll(): Unit = {
    chrome = Chrome.tryStart()
    if (chrome.nonEmpty) {
      val wiring = new FixtureTestWiring("17-05-2026")
      wiring.bootStartup()
      val anon    = Option.empty[models.User]
      val noOauth = Set.empty[String]
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

      // `/kina/X` mirrors the controller's `kinaPinned` action: filter the
      // path segment against the known cinema list and pass the matched
      // displayName as `pinnedCinema`. Garbage paths still render the page
      // with no pin (and a 200 — the controller does the same).
      server = new TestHttpServer({
        case "/"                          => indexHtml
        case "/kina"                      => renderKina(None)
        case p if p.startsWith("/kina/") =>
          val raw    = URLDecoder.decode(p.stripPrefix("/kina/"), "UTF-8")
          val pinned = cinemas.find(_ == raw)
          renderKina(pinned)
      })
    }
  }

  override def afterAll(): Unit = {
    if (server != null) server.close()
    chrome.foreach(_.close())
  }

  /** Open `path` (relative to the test server) in a fresh tab. Cancels
   *  the test cleanly when Chrome wasn't located in beforeAll. */
  private def onPath(path: String)(body: CdpPage => Any): Unit =
    chrome match {
      case Some(c) => c.openPage(server.baseUrl + path)(body(_))
      case None    => cancel("Chrome not installed — skipping JS behaviour test")
    }

  // ── /kina pill behaviour ─────────────────────────────────────────────────

  "the /kina pill row" should "filter to only the pinned cinema's section on click" in {
    onPath("/kina") { page =>
      val totalSections = page.evalInt("document.querySelectorAll('.cinema-section').length")
      totalSections should be > 1

      // First `.cinema-section` data-cinema — robust to future
      // re-orderings of Cinema.all. Pinning a known-empty cinema (e.g.
      // Kino Apollo with no showtimes today) would mask a filter
      // regression with a legitimately empty result.
      val pinTarget = page.evalString("document.querySelector('.cinema-section').dataset.cinema")
      clickPill(page, pinTarget)

      val visibleSections = page.evalInt(
        "[...document.querySelectorAll('.cinema-section')].filter(s => s.style.display !== 'none').length"
      )
      visibleSections shouldBe 1
      page.evalString(
        "document.querySelector('.cinema-section:not([style*=\"none\"])').dataset.cinema"
      ) shouldBe pinTarget
      page.evalInt("document.querySelectorAll('#cinema-pills .cinema-pill.active').length") shouldBe 1
    }
  }

  it should "restore every cinema section when the active pill is clicked again" in {
    onPath("/kina") { page =>
      val totalVisibleAtBoot = page.evalInt(
        "[...document.querySelectorAll('.cinema-section')].filter(s => s.style.display !== 'none').length"
      )
      val pinTarget = page.evalString("document.querySelector('.cinema-section').dataset.cinema")

      clickPill(page, pinTarget)
      page.evalInt("document.querySelectorAll('#cinema-pills .cinema-pill.active').length") shouldBe 1

      clickPill(page, pinTarget)
      page.evalInt("document.querySelectorAll('#cinema-pills .cinema-pill.active').length") shouldBe 0
      page.evalInt(
        "[...document.querySelectorAll('.cinema-section')].filter(s => s.style.display !== 'none').length"
      ) shouldBe totalVisibleAtBoot
    }
  }

  it should "rewrite the URL path to /kina/<cinema> when a pill is pinned" in {
    onPath("/kina") { page =>
      val pinTarget = page.evalString("document.querySelector('.cinema-section').dataset.cinema")
      clickPill(page, pinTarget)

      // history.replaceState moves the address bar to /kina/<cinema> so
      // a refresh keeps the pin. Comparing the decoded path sidesteps
      // the `encodeURIComponent` (browser, RFC3986, `%20`) vs
      // `URLEncoder.encode` (Java, form-encoded, `+`) mismatch — the
      // assertion is "the URL means /kina/<this cinema>", not "the
      // bytes are exactly this encoding".
      java.net.URLDecoder.decode(page.evalString("location.pathname"), "UTF-8") shouldBe ("/kina/" + pinTarget)

      clickPill(page, pinTarget)
      page.evalString("location.pathname") shouldBe "/kina"
    }
  }

  it should "not write the pin into the shared `disabledCinemas` localStorage" in {
    onPath("/kina") { page =>
      // Pre-seed localStorage as if Filtry on `/` had set it. /kina
      // ignores this on load AND must not overwrite it when a pill is
      // clicked — the persistent filter on / / /ulubione stays intact.
      val preset = """["Multikino Stary Browar","Helios Posnania"]"""
      page.eval(s"localStorage.setItem('disabledCinemas', ${jsString(preset)})")
      page.reload()

      page.evalInt("document.querySelectorAll('#cinema-pills .cinema-pill.active').length") shouldBe 0
      val pinTarget = page.evalString("document.querySelector('.cinema-section').dataset.cinema")
      clickPill(page, pinTarget)
      page.evalString("localStorage.getItem('disabledCinemas')") shouldBe preset
    }
  }

  // ── /kina/<cinema> URL-pinning ───────────────────────────────────────────

  "/kina/<cinema>" should "seed _kinaPinned from the URL path on load" in {
    // Pick a cinema known to be in the fixture corpus. Cinema City
    // Kinepolis has 84 Prada showtimes alone — reliably present.
    val target = "Cinema City Kinepolis"
    onPath("/kina/" + java.net.URLEncoder.encode(target, "UTF-8")) { page =>
      page.evalString("_kinaPinned") shouldBe target
      page.evalInt("document.querySelectorAll('#cinema-pills .cinema-pill.active').length") shouldBe 1
      page.evalString(
        "document.querySelector('#cinema-pills .cinema-pill.active').dataset.cinema"
      ) shouldBe target
      page.evalInt(
        "[...document.querySelectorAll('.cinema-section')].filter(s => s.style.display !== 'none').length"
      ) shouldBe 1
    }
  }

  it should "still not remember the pin across a plain /kina reload" in {
    onPath("/kina") { page =>
      val pinTarget = page.evalString("document.querySelector('.cinema-section').dataset.cinema")
      clickPill(page, pinTarget)
      page.evalString("_kinaPinned") shouldBe pinTarget

      // Force a navigation back to plain /kina (the test server, like
      // the real controller, serves the no-pin variant for the bare
      // path). The URL pin is the source of truth — bare /kina = no pin.
      page.eval(s"location.assign(${jsString(server.baseUrl + "/kina")})")
      page.waitFor("document.readyState === 'complete'", timeoutMs = 5000)
      page.evalBool("_kinaPinned === null") shouldBe true
    }
  }

  // ── / page filters ───────────────────────────────────────────────────────

  "the / page search input" should "filter visible film cards by title substring" in {
    onPath("/") { page =>
      // Pin the date filter to 'anytime' so the assertion below depends
      // ONLY on the search input. The page's default `<select>` value
      // is "Dzisiaj" (today, the first <option>), which makes
      // visibility a function of the browser's wall-clock — a card
      // whose last showing is on the fixture's 2026-05-21 falls out
      // of the visible set once the wall clock passes that date. The
      // fixture's screenings are anchored to a fixed `now =
      // 2026-05-17`, but `dateBounds()` reads the live `new Date()`,
      // so without this pin the test silently regresses each day past
      // a fixture-card's last showtime.
      pinDateFilterAnytime(page)

      val totalCards = page.evalInt("document.querySelectorAll('.col[data-title]').length")
      totalCards should be > 5

      // "Diabeł" matches the Polish-titled Prada rows in the 17-05-2026
      // corpus: the regular row + the Polish-titled Ukrainian-dub row
      // ("Diabeł ubiera się u Prady 2 ukraiński dubbing"). The Cyrillic
      // dub ("ДИЯВОЛ НОСИТЬ ПРАДА 2") is a separate card and doesn't
      // share the substring. Asserting exactly 2 catches a regression
      // where the search either over-matches (folds in the Cyrillic
      // card, e.g. by stripping diacritics too aggressively) or under-
      // matches (drops one of the Polish rows).
      page.eval("document.getElementById('search-input').value = 'Diabeł'; applyFilters()")
      val matchingCards = page.evalInt(
        "[...document.querySelectorAll('.col[data-title]')].filter(c => c.style.display !== 'none').length"
      )
      matchingCards shouldBe 2
      page.evalBool(
        "[...document.querySelectorAll('.col[data-title]')]" +
          ".filter(c => c.style.display !== 'none')" +
          ".every(c => c.dataset.title.toLowerCase().includes('diabeł'))"
      ) shouldBe true
    }
  }

  it should "show every previously-visible card again after clearing the search input" in {
    onPath("/") { page =>
      // Same wall-clock-stability reasoning as the previous test: pin
      // the date filter so the baseline + post-clear counts depend
      // only on the search input.
      pinDateFilterAnytime(page)

      val baselineVisible = page.evalInt(
        "[...document.querySelectorAll('.col[data-title]')].filter(c => c.style.display !== 'none').length"
      )
      page.eval("document.getElementById('search-input').value = 'Diabeł'; applyFilters()")
      page.eval("document.getElementById('search-input').value = '';        applyFilters()")
      page.evalInt(
        "[...document.querySelectorAll('.col[data-title]')].filter(c => c.style.display !== 'none').length"
      ) shouldBe baselineVisible
    }
  }

  /** Switch the date filter to "Kiedykolwiek" (anytime) and re-run
   *  `applyFilters()` so the visible-card set no longer depends on the
   *  browser's wall-clock relative to the fixture's recorded dates. */
  private def pinDateFilterAnytime(page: CdpPage): Unit =
    page.eval("document.getElementById('date-filter').value = 'anytime'; applyFilters()")

  // ── Hidden-films modal search ────────────────────────────────────────────

  "the hidden-films modal search input" should "filter listed titles by substring as the user types" in {
    onPath("/") { page =>
      // Seed a handful of hidden films directly via localStorage so the
      // modal has something to filter. Reload picks up the new state.
      val seeded = """["Diabeł ubiera się u Prady 2","Avatar","Cars"]"""
      page.eval(s"localStorage.setItem('hiddenFilms', ${jsString(seeded)})")
      page.reload()
      page.eval("openHiddenModal()")
      page.evalInt("document.querySelectorAll('#hidden-modal-list .panel-item').length") shouldBe 3

      page.eval("document.getElementById('hidden-modal-search').value = 'avat'; filterHiddenModal()")
      page.evalInt(
        "[...document.querySelectorAll('#hidden-modal-list .panel-item')].filter(i => i.style.display !== 'none').length"
      ) shouldBe 1
      page.evalString(
        "document.querySelector('#hidden-modal-list .panel-item:not([style*=\"none\"])').textContent"
      ) shouldBe "Avatar"
    }
  }

  it should "reset the search box on close so reopening shows the full list" in {
    onPath("/") { page =>
      val seeded = """["Avatar","Cars"]"""
      page.eval(s"localStorage.setItem('hiddenFilms', ${jsString(seeded)})")
      page.reload()
      page.eval("openHiddenModal()")
      page.eval("document.getElementById('hidden-modal-search').value = 'avat'; filterHiddenModal()")
      page.eval("closeHiddenModal()")
      page.evalString("document.getElementById('hidden-modal-search').value") shouldBe ""

      page.eval("openHiddenModal()")
      page.evalInt(
        "[...document.querySelectorAll('#hidden-modal-list .panel-item')].filter(i => i.style.display !== 'none').length"
      ) shouldBe 2
    }
  }

  // ── Mobile navbar layout ─────────────────────────────────────────────────
  //
  // CSS-driven layout regression for the mobile (≤ 575 px) two-row
  // navbar. Resizes the tab to an iPhone-ish width via CDP's
  // `Emulation.setDeviceMetricsOverride`, then reads
  // `getBoundingClientRect()` on each navbar item to assert the
  // physical layout matches the spec:
  //
  //   row 1: [logo+tabs] … [search] [auth]
  //   row 2: …             [date  ] [filtry]
  //
  // Why this test exists: the orders + `margin-left: auto` + the
  // 100%-wide `.navbar-row-break` are subtle — easy to break with an
  // unrelated edit. Asserting on rendered geometry catches a regression
  // that snapshot diffs alone wouldn't (the markup can look fine but
  // the visual layout flips).

  "the mobile navbar (≤ 575 px)" should "place search to the left of auth on row 1, date to the left of filtry on row 2" in {
    onPath("/") { page =>
      // 500 × 896 — widest mobile viewport (below the 576 px breakpoint
      // where the mobile media-query stops applying). Picked because
      // headless Chrome on the CI image renders Linux-fallback fonts
      // ~3 % wider than macOS Chrome locally — at 414 the navbar
      // cluster [logo+tabs] [search] [auth] just barely overflows
      // there, wrapping auth to its own row and tripping the
      // assertion. 500 gives ~80 px headroom over the cluster's
      // natural width on either platform.
      // `mobile: true` switches CDP's emulation to mobile mode, which
      // on Linux headless Chrome ignores the width override; staying
      // desktop-shaped (`mobile: false`, dpr 1.0) applies cleanly.
      page.send("Emulation.setDeviceMetricsOverride", play.api.libs.json.Json.obj(
        "width" -> 500, "height" -> 896, "deviceScaleFactor" -> 1.0, "mobile" -> false
      ))
      // The override triggers a re-layout but doesn't always re-fire
      // applyFilters etc.; wait a frame for the resize listeners to
      // settle.
      Thread.sleep(100L)
      // beforeAll renders the corpus with `oauthProviders = Set.empty`,
      // which leaves `<div class="navbar-auth">` empty (no Zaloguj-się
      // pill). An empty flex item has zero size and its `getBoundingClientRect`
      // reports a degenerate position that doesn't reflect production
      // (where an OAuth provider is always configured and the pill is
      // visible). Inject the prod-shaped child here so the layout
      // assertions exercise the realistic anonymous-user navbar.
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

      def rect(sel: String): (Double, Double) =
        page.evalString(
          s"(() => { const r = document.querySelector(${jsString(sel)}).getBoundingClientRect();" +
          s"          return r.top + '|' + r.left; })()"
        ).split('|') match {
          case Array(t, l) => (t.toDouble, l.toDouble)
        }
      val (searchTop, searchLeft) = rect(".navbar-search")
      val (authTop,   authLeft  ) = rect(".navbar-auth")
      val (dateTop,   dateLeft  ) = rect(".navbar-date")
      val (filtryTop, filtryLeft) = rect(".navbar-filtry")

      val viewportWidth = page.evalInt("window.innerWidth")
      // Row 1: search and auth share the same top. Allow a 4 px
      // tolerance for sub-pixel alignment + line-height variance.
      withClue(s"viewport=$viewportWidth search=($searchTop,$searchLeft) auth=($authTop,$authLeft) date=($dateTop,$dateLeft) filtry=($filtryTop,$filtryLeft) ") {
        viewportWidth shouldBe 500
        math.abs(searchTop - authTop) should be < 4.0
        searchLeft should be < authLeft
      }

      // Row 2: date on left, filtry flush right, both at the same top.
      math.abs(dateTop - filtryTop) should be < 4.0
      dateTop should be > authTop
      dateLeft should be < filtryLeft

      // Filtry button truncation — pile on every active filter the
      // navbar can carry, watch the label balloon, and confirm the
      // button STILL sits next to date on row 2 instead of being
      // pushed onto a row of its own. Tests the `max-width: 50%` +
      // `text-overflow: ellipsis` belt-and-braces on the button.
      page.eval(
        "document.querySelector('input[name=\"format-dim\"][value=\"2D\"]').click(); " +
        "document.querySelector('input[name=\"format-lang\"][value=\"NAP\"]').click(); " +
        "document.getElementById('format-imax').click(); " +
        "document.getElementById('from-hour').value = '18'; onFormatChange(); " +
        "updateFormatBtn();"
      )
      Thread.sleep(50L)
      val (dateTop2,   _) = rect(".navbar-date")
      val (filtryTop2, _) = rect(".navbar-filtry")
      withClue(s"after filters: dateTop=$dateTop2 filtryTop=$filtryTop2") {
        math.abs(dateTop2 - filtryTop2) should be < 4.0
      }
      // The button's max-width clamp + overflow + text-overflow
      // guarantee a long label can't push the button off the row.
      // Assert the three properties are computed as expected — this
      // is the durable invariant; whether a particular label happens
      // to overflow at a particular viewport width is incidental.
      val truncationCss = page.evalString(
        "(() => { const s = getComputedStyle(document.getElementById('format-filter-btn'));" +
        "          return s.maxWidth + '|' + s.overflowX + '|' + s.textOverflow; })()"
      )
      val Array(maxW, overflowX, textOverflow) = truncationCss.split('|')
      withClue(s"maxWidth=$maxW overflowX=$overflowX textOverflow=$textOverflow") {
        maxW         should not be "none"
        overflowX    shouldBe "hidden"
        textOverflow shouldBe "ellipsis"
      }

      // Reset emulation so the next test starts at the default viewport.
      page.send("Emulation.clearDeviceMetricsOverride", play.api.libs.json.Json.obj())
    }
  }

  // ── Mobile-scale invariants across common phone widths ───────────────────
  //
  // Sweeps the same `/` page through eight phone-class viewports
  // (iPhone-SE 320 → just under the 576 breakpoint 575) and asserts
  // two structural invariants on every width:
  //
  //   1. The navbar wraps to at most 2 rows. Visible flex children of
  //      `.navbar` are bucketed by their `getBoundingClientRect().top`;
  //      the distinct bucket count = the row count. ≤ 2 means the
  //      `<div class="navbar-row-break">` is still doing its job and
  //      no element is overflowing its row.
  //
  //   2. No child extends past the navbar's right edge. Catches the
  //      case where a long label (e.g. a Filtry button decorated with
  //      every active filter) would push the row's last item past the
  //      viewport — visually invisible because of the
  //      `max-width: 40%; overflow: hidden` belt-and-braces, but the
  //      assertion is the durable invariant.
  //
  // This exists because the layout was previously assembled out of
  // ~15 per-property `clamp(min, vw-formula, max)` rules — each with
  // its own cap viewport. Some hit their ceiling at ~410 px, others
  // at ~540 px. At intermediate widths (Samsung S10 360, iPhone 17
  // Pro Max ~430) the ratios between navbar and grid sizes drifted.
  // The refactor collapsed every per-mobile size to
  // `calc(BASE * var(--mobile-scale))` against a single shared scale
  // variable; this test pins the resulting layout against regressions.

  "the mobile navbar" should "wrap to ≤ 2 rows with zero horizontal overflow at every common phone width" in {
    onPath("/") { page =>
      // beforeAll renders the corpus with `oauthProviders = Set.empty`,
      // which leaves `.navbar-auth` empty (no Zaloguj-się pill). Inject
      // the prod-shaped child so the layout assertions exercise the
      // realistic anonymous-user navbar (same as the test above does).
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

      // Common phone viewports (CSS px). 360 = Samsung Galaxy S10/S20/
      // S22 + the narrowest Android in current circulation; 375 =
      // iPhone SE 2/3 + iPhone 12 mini; 390 = iPhone 12/13/14/15;
      // 412 = Pixel 6/7/8; 430 = iPhone 14/15/16/17 Pro Max; 540 = a
      // wider phone landscape / small tablet portrait; 575 = the
      // @@media breakpoint top, where the mobile rules hand back to
      // the desktop defaults.
      //
      // iPhone SE 1st gen (320 px CSS, released 2016, EOL 2018) is
      // intentionally NOT in the list — at that width the row 1
      // cluster (logo + 3 nav-tabs + search input + Zaloguj-się pill)
      // is just wider than the content area regardless of font scale,
      // so the layout wraps to 3 rows. Apple's narrowest currently-
      // supported device is iPhone SE 2/3 at 375 px; accept this as
      // the design's narrow limit.
      val viewports = Seq(360, 375, 390, 412, 430, 540, 575)

      // Snapshot the layout invariants once per viewport and assert
      // afterwards so a failure prints the full table of (width, rows,
      // overflowPx) for every viewport, not just the first one to fail.
      // Easier to diagnose a regression that only bites one width.
      case class Row(width: Int, rows: Int, overflow: Int, scale: Double)
      val measured: Seq[Row] = viewports.map { w =>
        page.setViewport(w, 800)
        // CDP's setDeviceMetricsOverride triggers a relayout, but
        // resize-listeners and font-driven flex-shrink can settle on a
        // second tick. A short pause + a forced reflow read is enough.
        Thread.sleep(60L)

        val rowCount = page.evalInt(
          "(() => { const nav = document.querySelector('.navbar');" +
          "          const tops = new Set();" +
          "          for (const c of nav.children) {" +
          "            const r = c.getBoundingClientRect();" +
          "            if (r.width === 0 || r.height === 0) continue;" +
          "            tops.add(Math.round(r.top));" +
          "          }" +
          "          return tops.size; })()"
        )
        val overflowPx = page.evalInt(
          "(() => { const nav = document.querySelector('.navbar');" +
          "          const navRight = nav.getBoundingClientRect().right;" +
          "          let maxOver = 0;" +
          "          for (const c of nav.children) {" +
          "            const r = c.getBoundingClientRect();" +
          "            if (r.width === 0 || r.height === 0) continue;" +
          "            const over = Math.ceil(r.right - navRight);" +
          "            if (over > maxOver) maxOver = over;" +
          "          }" +
          "          return maxOver; })()"
        )
        // Sanity: --mobile-scale is monotone-nondecreasing in viewport
        // and lands exactly on 1.0 at the breakpoint top.
        // `getComputedStyle.getPropertyValue` returns the *declared* CSS
        // expression (the literal `clamp(...calc(...)...)`) for unregistered
        // custom properties — not the resolved number — so reading the
        // var via a `<div style="width: calc(1000px * var(--mobile-scale))">`
        // probe and dividing the rendered width by 1000 is the way to get
        // the browser's evaluated scale as a Double.
        val scale = page.evalString(
          "(() => { const p = document.createElement('div');" +
          "          p.style.position = 'fixed';" +
          "          p.style.visibility = 'hidden';" +
          "          p.style.width = 'calc(1000px * var(--mobile-scale))';" +
          "          document.body.appendChild(p);" +
          "          const w = p.getBoundingClientRect().width;" +
          "          document.body.removeChild(p);" +
          "          return String(w / 1000); })()"
        ).toDouble
        Row(w, rowCount, overflowPx, scale)
      }

      // Reset emulation so a later test in this spec doesn't inherit the
      // last viewport from this loop.
      page.send("Emulation.clearDeviceMetricsOverride", play.api.libs.json.Json.obj())

      // Print the table for the test log so a future reader / CI run
      // can see the actual measured values at each width — matches the
      // project rule that investigation scripts print what they touched.
      val table = measured.map(r =>
        f"  ${r.width}%3d px → rows=${r.rows}  overflow=${r.overflow}%3d px  scale=${r.scale}%.3f"
      ).mkString("\n")
      info(s"Mobile navbar layout sweep:\n$table")

      withClue(s"layout sweep:\n$table\n") {
        all (measured.map(_.rows))     should be <= 2
        all (measured.map(_.overflow)) shouldBe 0
        // Scale boundary: 1.0 at the 575 px breakpoint top, where the
        // mobile overrides land exactly on the desktop ceiling. At the
        // narrow end the analytic floor is 0.85 (at viewport ≤ 320),
        // but the sweep starts at 360 so the head value here is the
        // formula's evaluation at 360, not the floor. The durable
        // invariant is "monotone non-decreasing across the sweep and
        // lands on 1.0 at 575" — not byte-exact equality with the
        // analytical value at any specific width.
        measured.last.scale shouldBe (1.0 +- 0.001)
        measured.map(_.scale) shouldBe sorted
      }
    }
  }

  // ── helpers ──────────────────────────────────────────────────────────────

  /** Click the pill whose `data-cinema` matches `cinema`. Asserts the
   *  pill was found so a typo'd cinema name fails the test with a clear
   *  message instead of silently no-op'ing. Wrapped in an IIFE so the
   *  internal `const` doesn't bleed into the per-page evaluation scope
   *  (every `Runtime.evaluate` shares one execution context — two calls
   *  with the same top-level `const` would `SyntaxError`). */
  private def clickPill(page: CdpPage, cinema: String): Unit = {
    val js =
      s"(() => { const p = [...document.querySelectorAll('#cinema-pills .cinema-pill')]" +
        s".find(el => el.dataset.cinema === ${jsString(cinema)}); " +
        s"if (!p) throw new Error('no pill for ' + ${jsString(cinema)}); p.click(); return true; })()"
    page.evalBool(js) shouldBe true
  }

  /** Quote a string for embedding inside a JS expression. Round-trips
   *  Polish diacritics + apostrophes via `JSON.stringify`-compatible
   *  escaping. */
  private def jsString(s: String): String =
    play.api.libs.json.JsString(s).toString
}
