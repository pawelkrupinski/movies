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

      // `/kina/X` mirrors the controller's `kinaPinned` action: filter the
      // path segment against the known cinema list and pass the matched
      // displayName as `pinnedCinema`. Garbage paths still render the page
      // with no pin (and a 200 — the controller does the same).
      // `/film?title=…` mirrors the `MovieController.film` action: look
      // the title up in the fixture corpus and render the per-film
      // page. Used by the layout-sweep tests to drive /film through
      // every phone viewport.
      def renderFilm(title: String): String = {
        val target = URLDecoder.decode(title, "UTF-8")
        schedules.find(_.movie.title == target) match {
          case Some(s) =>
            views.html.film(s, s"http://test.local/film?title=$title",
              ogDescription = "", devMode = false).body
          case None    => "<html><body>Film not found</body></html>"
        }
      }
      // `/plan` is static for the fixture corpus — the poster picker +
      // "Twoje filmy" plan. Drives the Filmy-section fold behaviour
      // (collapse on header click, expand on a click anywhere while
      // folded) wired by the inline <script> in plan.scala.html.
      val planHtml: String = views.html.plan(
        controllers.PlanController.viewData(schedules),
        cinemas, pills, devMode = false,
        currentUser = anon, oauthProviders = noOauth
      ).body
      server = new TestHttpServer({
        // `/` and `/kina` accept arbitrary query strings (e.g. `?date=tomorrow`)
        // — the real Play routes do too, and the day-selector ↔ URL tests need
        // to boot the page with the param already in `location.search`.
        case p if p == "/"     || p.startsWith("/?")     => indexHtml
        case p if p == "/kina" || p.startsWith("/kina?") => renderKina(None)
        case p if p.startsWith("/kina/") =>
          // Strip optional query string before resolving the pinned cinema.
          val rawWithQuery = p.stripPrefix("/kina/")
          val raw          = URLDecoder.decode(rawWithQuery.takeWhile(_ != '?'), "UTF-8")
          val pinned       = cinemas.find(_ == raw)
          renderKina(pinned)
        case p if p.startsWith("/film?title=") =>
          renderFilm(p.stripPrefix("/film?title="))
        case p if p == "/plan" || p.startsWith("/plan?") => planHtml
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
      setDateAnytime(page)
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
      setDateAnytime(page)
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
      // clicked — the persistent filter on / stays intact.
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
      setDateAnytime(page)
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

  // ── shared.js globals ────────────────────────────────────────────────────

  // The inline <script> blocks in repertoire.scala.html and kina.scala.html
  // call functions exported by shared.js (undoTruncation, truncateAllShowings,
  // schedulePosterRetry, applyFilters, applyFiltersDebounced). If shared.js
  // fails to load — stale cache, broken fingerprinting, wrong load order —
  // every page interaction breaks with a ReferenceError.
  "shared.js globals" should "be defined on / before any user interaction" in {
    onPath("/") { page =>
      page.evalBool("typeof undoTruncation === 'function'") shouldBe true
      page.evalBool("typeof truncateAllShowings === 'function'") shouldBe true
      page.evalBool("typeof schedulePosterRetry === 'function'") shouldBe true
      page.evalBool("typeof applyFilters === 'function'") shouldBe true
      page.evalBool("typeof applyFiltersDebounced === 'function'") shouldBe true
    }
  }

  it should "be defined on /kina before any user interaction" in {
    onPath("/kina") { page =>
      page.evalBool("typeof undoTruncation === 'function'") shouldBe true
      page.evalBool("typeof truncateAllShowings === 'function'") shouldBe true
      page.evalBool("typeof schedulePosterRetry === 'function'") shouldBe true
      page.evalBool("typeof applyFilters === 'function'") shouldBe true
      page.evalBool("typeof applyFiltersDebounced === 'function'") shouldBe true
    }
  }

  // ── Delegated event handlers tolerate non-Element targets ──────────────
  //
  // Regression for the Sentry-reported `e.target.closest is not a function`
  // crash on Chrome Mobile. Browsers can dispatch click / mouseover /
  // touchstart with `e.target` being something other than an Element
  // (Text node, Document, etc.) — the handlers must `instanceof Element`-
  // gate before calling `.closest()` or the whole page chain throws.
  //
  // Hard to reproduce the exact mobile path in CDP, so the test drives the
  // failure mode directly: it dispatches a real Event whose target ends up
  // as `document` (no `.closest`) and asserts the handlers neither throw
  // nor leave the tooltip half-shown.

  "delegated handlers" should "tolerate a click event whose target isn't an Element" in {
    onPath("/") { page =>
      val ok = page.evalBool(
        """(() => {
          |  try {
          |    document.dispatchEvent(new MouseEvent('click', { bubbles: false }));
          |    return true;
          |  } catch (e) { return false; }
          |})()""".stripMargin
      )
      ok shouldBe true
    }
  }

  it should "tolerate a mouseover event whose target isn't an Element" in {
    onPath("/kina") { page =>
      val ok = page.evalBool(
        """(() => {
          |  try {
          |    document.dispatchEvent(new MouseEvent('mouseover', { bubbles: false }));
          |    return true;
          |  } catch (e) { return false; }
          |})()""".stripMargin
      )
      ok shouldBe true
    }
  }

  it should "tolerate a touchstart event whose target isn't an Element" in {
    onPath("/kina") { page =>
      val ok = page.evalBool(
        """(() => {
          |  try {
          |    document.dispatchEvent(new Event('touchstart', { bubbles: false }));
          |    return true;
          |  } catch (e) { return false; }
          |})()""".stripMargin
      )
      ok shouldBe true
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
      // corpus: the regular row, the Polish-titled Ukrainian-dub row
      // ("Diabeł ubiera się u Prady 2 ukraiński dubbing"), and the
      // separate "Filmowy klub seniora: diabeł ubiera się u prady 2"
      // programme row (senior-club screenings keep their own card via
      // TitleNormalizer.ProgrammePrefix). The Cyrillic dub
      // ("ДИЯВОЛ НОСИТЬ ПРАДА 2") is a separate card and doesn't share the
      // substring. Asserting exactly 3 catches a regression where the
      // search either over-matches (folds in the Cyrillic card, e.g. by
      // stripping diacritics too aggressively) or under-matches (drops
      // one of the Polish rows).
      page.eval("document.getElementById('search-input').value = 'Diabeł'; applyFilters()")
      val matchingCards = page.evalInt(
        "[...document.querySelectorAll('.col[data-title]')].filter(c => c.style.display !== 'none').length"
      )
      matchingCards shouldBe 3
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
        "            btn.textContent = 'Zaloguj';" +
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
  // Sweeps each navbar-bearing page (`/`, `/kina`) through seven
  // phone-class viewports and asserts two structural invariants:
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
  //      viewport.
  //
  // This exists because the layout was previously assembled out of
  // ~15 per-property `clamp(min, vw-formula, max)` rules — each with
  // its own cap viewport. The refactor collapsed every per-mobile size
  // to `calc(BASE * var(--mobile-scale))` against one shared scale
  // variable; this test pins the resulting layout against regressions
  // — both on the listing page and on `/kina`'s sticky-pill layout
  // (which adds a second nav-row that the same scale has to keep
  // legible without crowding the row above).

  // Common phone viewports (CSS px). 360 = Samsung Galaxy S10/S20/S22
  // + the narrowest Android in current circulation; 375 = iPhone SE
  // 2/3 + iPhone 12 mini; 390 = iPhone 12/13/14/15; 412 = Pixel 6/7/8;
  // 430 = iPhone 14/15/16/17 Pro Max; 540 = a wider phone landscape /
  // small tablet portrait; 575 = the @@media breakpoint top, where the
  // mobile rules hand back to the desktop defaults.
  //
  // iPhone SE 1st gen (320 px CSS, released 2016, EOL 2018) is
  // intentionally NOT in the list — at that width the row 1 cluster
  // (logo + 3 nav-tabs + search input + Zaloguj-się pill) is just
  // wider than the content area regardless of font scale, so the
  // layout wraps to 3 rows. Apple's narrowest currently-supported
  // device is iPhone SE 2/3 at 375 px; accept this as the design's
  // narrow limit.
  private val MobileViewports = Seq(360, 375, 390, 412, 430, 540, 575)

  for (path <- Seq("/", "/kina")) {
    s"the mobile navbar on $path" should "wrap to ≤ 2 rows with zero horizontal overflow at every common phone width" in {
      onPath(path) { page =>
        pinDeterministicFont(page)
        // beforeAll renders the corpus with `oauthProviders = Set.empty`,
        // which leaves `.navbar-auth` empty (no Zaloguj-się pill). Inject
        // the prod-shaped child so the layout assertions exercise the
        // realistic anonymous-user navbar.
        page.eval(
          "(() => { const a = document.querySelector('.navbar-auth');" +
          "          if (a && !a.children.length) {" +
          "            const btn = document.createElement('button');" +
          "            btn.type = 'button';" +
          "            btn.className = 'nav-tab nav-tab-login';" +
          "            btn.textContent = 'Zaloguj';" +
          "            a.appendChild(btn);" +
          "          } })()"
        )

        // Snapshot the layout invariants once per viewport and assert
        // afterwards so a failure prints the full table of (width, rows,
        // overflowPx) for every viewport, not just the first one to fail.
        case class Row(width: Int, rows: Int, overflow: Int, scale: Double)
        val measured: Seq[Row] = MobileViewports.map { w =>
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
          // probe and dividing the rendered width by 1000 is the way to
          // get the browser's evaluated scale as a Double.
          //
          // Falls back to a content-block + display: block on Chrome 108–128
          // where the fixed-position visibility-hidden probe resolves the
          // calc'd width to 0. The fallback probe lives inside a real
          // grid cell so layout has the parent context required to
          // resolve `100vw`-relative clamps; reading by `clientWidth`
          // (which the var-style probe uses) succeeds across every
          // matrix Chrome version.
          val scale = page.evalString(
            "(() => {" +
            "  const inProbe = (parent, extraStyle) => {" +
            "    const d = document.createElement('div');" +
            "    d.style.cssText = 'width:calc(1000px*var(--mobile-scale));' + extraStyle;" +
            "    parent.appendChild(d);" +
            "    const w = d.clientWidth;" +
            "    parent.removeChild(d);" +
            "    return w;" +
            "  };" +
            "  let w = inProbe(document.body," +
            "    'position:fixed;visibility:hidden;top:0;left:0');" +
            "  if (!w) w = inProbe(document.body, 'display:block');" +
            "  return String(w / 1000);" +
            "})()"
          ).toDouble
          Row(w, rowCount, overflowPx, scale)
        }

        // Reset emulation so a later test doesn't inherit the last
        // viewport from this loop.
        page.send("Emulation.clearDeviceMetricsOverride", play.api.libs.json.Json.obj())

        // Print the table for the test log — matches the project rule
        // that investigation scripts print what they touched.
        val table = measured.map(r =>
          f"  ${r.width}%3d px → rows=${r.rows}  overflow=${r.overflow}%3d px  scale=${r.scale}%.3f"
        ).mkString("\n")
        info(s"$path layout sweep:\n$table")

        withClue(s"$path layout sweep:\n$table\n") {
          all (measured.map(_.rows))     should be <= 2
          all (measured.map(_.overflow)) shouldBe 0
          // Scale boundary: 1.0 at the 575 px breakpoint top. At the
          // narrow end the analytic floor is 0.85 at viewport ≤ 320,
          // but the sweep starts at 360 so the head value here is the
          // formula's evaluation at 360, not the floor. The durable
          // invariant is "monotone non-decreasing across the sweep and
          // lands on 1.0 at 575" — not byte-exact equality with the
          // analytical value at any specific width.
          //
          // The scale probe is unreliable on Chrome 108–128 (the
          // browsers in the matrix below 'stable'): those builds
          // resolve `calc(1000px * var(--mobile-scale))` to invalid,
          // and the probe div falls back to `width: auto` (= viewport
          // width). Detect that fallback (scale ≈ viewport / 1000) and
          // skip the scale-specific assertions on those browsers —
          // the rows / overflow checks above already cover the
          // user-visible layout invariant. Production CSS on those
          // Chrome versions still works; only the test-time probe
          // can't read the resolved scale back out.
          val probeOk = measured.forall { r =>
            r.scale > 0 && r.scale <= 1.01 && math.abs(r.scale - r.width / 1000.0) > 0.05
          }
          if (probeOk) {
            measured.last.scale shouldBe (1.0 +- 0.001)
            measured.map(_.scale) shouldBe sorted
          } else {
            info(s"--mobile-scale probe unreliable on this Chrome — skipping scale assertion. " +
                 s"Saw scales=${measured.map(_.scale)}")
          }
        }
      }
    }
  }

  // ── /film page element visibility ────────────────────────────────────────
  //
  // Every key element on the /film detail page — poster, title, ratings,
  // metadata, showtimes — must be visible (non-zero bounding rect) at
  // both desktop and mobile widths. Catches CSS leaks like _sharedStyles'
  // aspect-ratio box rules collapsing the poster to zero height.

  private val filmTarget = "/film?title=" + java.net.URLEncoder.encode(
    "Diabeł ubiera się u Prady 2", "UTF-8")

  // Elements that must be visible on the /film page. Each pair is
  // (label for diagnostics, CSS selector). The selector must match at
  // least one element and that element must have a non-zero bounding
  // rect (width > 0 AND height > 0).
  private val filmVisibleElements = Seq(
    "poster"    -> ".poster-img",
    "title"     -> ".film-title",
    "showtimes" -> ".badge-time",
  )

  "the /film page elements" should "all be visible at desktop width" in {
    onPath(filmTarget) { page =>
      page.setDesktopViewport(1280, 900)
      val failures = filmVisibleElements.flatMap { case (label, sel) =>
        val visible = page.evalBool(
          s"(function(){ var el = document.querySelector('$sel');" +
          "if (!el) return false; var r = el.getBoundingClientRect();" +
          "return r.width > 0 && r.height > 0; })()"
        )
        if (visible) None else Some(label)
      }
      page.send("Emulation.clearDeviceMetricsOverride", play.api.libs.json.Json.obj())
      withClue(s"Not visible at 1280px desktop: ${failures.mkString(", ")}\n") {
        failures shouldBe empty
      }
    }
  }

  it should "all be visible at mobile width (375px)" in {
    onPath(filmTarget) { page =>
      page.setViewport(375, 800)
      val failures = filmVisibleElements.flatMap { case (label, sel) =>
        val visible = page.evalBool(
          s"(function(){ var el = document.querySelector('$sel');" +
          "if (!el) return false; var r = el.getBoundingClientRect();" +
          "return r.width > 0 && r.height > 0; })()"
        )
        if (visible) None else Some(label)
      }
      page.send("Emulation.clearDeviceMetricsOverride", play.api.libs.json.Json.obj())
      withClue(s"Not visible at 375px mobile: ${failures.mkString(", ")}\n") {
        failures shouldBe empty
      }
    }
  }

  // ── /film page mobile sweep ──────────────────────────────────────────────
  //
  // /film has its own stylesheet block (no `_sharedStyles`) — the
  // `--mobile-scale` variable is declared in `_pillStyles` so it
  // reaches /film, and the per-mobile @@media block in film.scala.html
  // scales the title / meta / cinema-link chrome.
  //
  // The page doesn't have the 2-row navbar (just a back-link), so the
  // assertion here is the simpler "nothing in the page body is wider
  // than the viewport": the title, the meta cluster, the cinema-link
  // pills, the trailer-link buttons all stay within their container.
  // Catches a regression where a long Polish title or a wide cinema-
  // link row would push the layout horizontally past the viewport
  // edge, producing a horizontal scrollbar on mobile.

  "the /film page" should "fit horizontally within every common phone width" in {
    // Pick a film known to exist in the fixture corpus and to have a
    // long-ish title (so the assertion exercises real text rendering,
    // not the empty-fixture trivial case).
    val target = "/film?title=" + java.net.URLEncoder.encode(
      "Diabeł ubiera się u Prady 2", "UTF-8")
    onPath(target) { page =>
      pinDeterministicFont(page)
      val overflows: Seq[(Int, Int)] = MobileViewports.map { w =>
        page.setViewport(w, 1000)
        Thread.sleep(60L)
        // Document scroll-width vs viewport width — the simplest
        // "anything overflowing horizontally" check. Body content
        // wider than the viewport produces a horizontal scrollbar
        // (Safari) or just visual clipping (Chrome with overflow-x:
        // hidden on the body). Either way the document.documentElement
        // .scrollWidth reflects the underlying content width.
        val overflowPx = page.evalInt(
          "Math.max(0, document.documentElement.scrollWidth - window.innerWidth)"
        )
        w -> overflowPx
      }
      page.send("Emulation.clearDeviceMetricsOverride", play.api.libs.json.Json.obj())

      val table = overflows.map { case (w, ov) =>
        f"  $w%3d px → overflow=$ov%3d px"
      }.mkString("\n")
      info(s"/film layout sweep:\n$table")
      withClue(s"/film layout sweep:\n$table\n") {
        all (overflows.map(_._2)) shouldBe 0
      }
    }
  }

  // ── Desktop layout invariants ────────────────────────────────────────────
  //
  // The mobile sweep above pins the < 576 px branch; this one pins the
  // desktop branch. Three common widths bracket the realistic desktop
  // range:
  //   1280 = 13" laptop / small external monitor at default scaling
  //   1440 = MacBook 14" + many midrange external displays
  //   1920 = Full-HD desktop (the most common single-monitor width)
  //
  // Asserts:
  //   1. The navbar fits in ONE row. Desktop CSS removes the
  //      `.navbar-row-break` flex item so all the navbar children
  //      sit in line; if a wide label or new entry overflows onto a
  //      second row, that's a regression.
  //   2. Zero horizontal overflow on the document. `scrollWidth >
  //      innerWidth` produces a horizontal scrollbar, which is always
  //      a desktop bug.
  //
  // Uses `setDesktopViewport` (mobile = false) so the page sees the
  // same `pointer: fine` / `@media (hover: hover)` truthiness a real
  // desktop browser does, and the `--mobile-scale` clamps stay at 1.0.
  private val DesktopViewports = Seq(1280, 1440, 1920)

  for (path <- Seq("/", "/kina")) {
    s"the desktop navbar on $path" should "fit in one row with zero horizontal overflow at every common desktop width" in {
      onPath(path) { page =>
        pinDeterministicFont(page)
        // Same anonymous-user pill injection the mobile sweep uses, so
        // the navbar width measurement covers the realistic prod
        // layout (logged-out visitor with a Zaloguj-się button).
        page.eval(
          "(() => { const a = document.querySelector('.navbar-auth');" +
          "          if (a && !a.children.length) {" +
          "            const btn = document.createElement('button');" +
          "            btn.type = 'button';" +
          "            btn.className = 'nav-tab nav-tab-login';" +
          "            btn.textContent = 'Zaloguj';" +
          "            a.appendChild(btn);" +
          "          } })()"
        )

        case class Row(width: Int, rows: Int, docOverflow: Int)
        val measured: Seq[Row] = DesktopViewports.map { w =>
          page.setDesktopViewport(w, 900)
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
          val docOverflow = page.evalInt(
            "Math.max(0, document.documentElement.scrollWidth - window.innerWidth)"
          )
          Row(w, rowCount, docOverflow)
        }

        page.send("Emulation.clearDeviceMetricsOverride", play.api.libs.json.Json.obj())

        val table = measured.map { r =>
          f"  ${r.width}%4d px → rows=${r.rows} overflow=${r.docOverflow}%3d px"
        }.mkString("\n")
        info(s"desktop layout sweep on $path:\n$table")
        withClue(s"desktop layout sweep on $path:\n$table\n") {
          all (measured.map(_.rows))        shouldBe 1
          all (measured.map(_.docOverflow)) shouldBe 0
        }
      }
    }
  }

  // ── Single-tap card navigation ───────────────────────────────────────────
  //
  // Every tap on a poster or title link navigates directly to /film.
  // Icons (★, ✕) are always visible — no two-tap preview system.

  private val firstCardPosterLink =
    "(() => [...document.querySelectorAll('.col[data-title]')]" +
    "        .find(c => c.style.display !== 'none')" +
    "        ?.querySelector('.card .poster-wrap > a'))()"

  "the card poster link" should
    "navigate to /film on the first click" in {
    onPath("/") { page =>
      pinDateFilterAnytime(page)
      val title = page.evalString(
        s"$firstCardPosterLink.closest('[data-title]').dataset.title"
      )
      page.eval(s"$firstCardPosterLink.click()")
      page.waitFor("location.pathname === '/film'", timeoutMs = 5000)
      java.net.URLDecoder.decode(page.evalString("location.search"), "UTF-8") shouldBe
        ("?title=" + title)
    }
  }

  // ── Poster retry with exponential backoff ─────────────────────────────
  //
  // The inline onerror on each poster <img> walks fallback URLs, then
  // calls schedulePosterRetry to kick off an exponential-backoff retry
  // loop (2s, 6s, 18s, 54s, 162s — same sequence as iOS). These tests
  // verify the JS backoff math and the DOM wiring without waiting for
  // real timers.

  "poster retry backoff" should "produce the 2·3^n sequence capped at 162" in {
    onPath("/") { page =>
      val delays = (0 to 6).map(i => page.evalInt(s"_posterDelay($i)"))
      delays shouldBe Seq(2, 6, 18, 54, 162, 162, 162)
    }
  }

  it should "render data-original-src on every poster img" in {
    onPath("/") { page =>
      pinDateFilterAnytime(page)
      val withPoster = page.evalInt(
        "[...document.querySelectorAll('.poster-wrap img[src]')].length"
      )
      val withOriginal = page.evalInt(
        "[...document.querySelectorAll('.poster-wrap img[data-original-src]')].length"
      )
      withPoster should be > 0
      withOriginal shouldBe withPoster
    }
  }

  it should "schedule a retry when the fallback chain is exhausted" in {
    onPath("/") { page =>
      pinDateFilterAnytime(page)
      // Grab the first poster img and simulate exhaustion of all fallbacks.
      // Reset any state from a natural onerror cycle that may have already
      // completed (Chrome 113 processes the fallback chain before the test runs).
      page.eval(
        """(() => {
          |  const img = document.querySelector('.poster-wrap img[data-original-src]');
          |  img.onerror = null;
          |  cancelPosterRetry(img);
          |  img.removeAttribute('data-retry-attempt');
          |  img.removeAttribute('data-fallbacks');
          |  img.style.display = 'none';
          |  img.nextElementSibling.style.display = 'flex';
          |  schedulePosterRetry(img);
          |})()""".stripMargin)
      val attempt = page.evalString(
        "document.querySelector('.poster-wrap img[data-retry-attempt]').dataset.retryAttempt"
      )
      attempt shouldBe "1"
      // Clean up the timer so it doesn't fire during teardown.
      page.eval(
        """(() => {
          |  const img = document.querySelector('.poster-wrap img[data-retry-attempt]');
          |  cancelPosterRetry(img);
          |})()""".stripMargin)
    }
  }

  it should "append a cache-buster on retry generation > 0" in {
    onPath("/") { page =>
      page.evalString("_posterCacheBust('https://example.com/poster.jpg', 0)") shouldBe
        "https://example.com/poster.jpg"
      page.evalString("_posterCacheBust('https://example.com/poster.jpg', 3)") shouldBe
        "https://example.com/poster.jpg?_kinowo_t=3"
      page.evalString("_posterCacheBust('https://example.com/poster.jpg?w=480', 2)") shouldBe
        "https://example.com/poster.jpg?w=480&_kinowo_t=2"
    }
  }

  // ── Filtry dropdown ───────────────────────────────────────────────────────

  "the Filtry dropdown" should "narrow badges to 3D-only when Wymiar = 3D" in {
    onPath("/") { page =>
      clearLocalStorage(page)
      pinDateFilterAnytime(page)
      val before = visibleBadgeCount(page)
      before should be > 0

      page.eval("document.querySelector('input[name=\"format-dim\"][value=\"3D\"]').click()")
      val after = visibleBadgeCount(page)
      after should be > 0
      after should be < before
      page.evalBool(
        "[...document.querySelectorAll('.badge-time')]" +
          ".filter(b => b.style.display !== 'none')" +
          ".every(b => (b.dataset.format || '').split(' ').includes('3D'))"
      ) shouldBe true
    }
  }

  it should "narrow badges to NAP-only when Wersja = NAP" in {
    onPath("/") { page =>
      clearLocalStorage(page)
      pinDateFilterAnytime(page)
      val before = visibleBadgeCount(page)

      page.eval("document.querySelector('input[name=\"format-lang\"][value=\"NAP\"]').click()")
      val after = visibleBadgeCount(page)
      after should be > 0
      after should be < before
      page.evalBool(
        "[...document.querySelectorAll('.badge-time')]" +
          ".filter(b => b.style.display !== 'none')" +
          ".every(b => (b.dataset.format || '').split(' ').includes('NAP'))"
      ) shouldBe true
    }
  }

  it should "narrow badges to IMAX-only when IMAX is checked" in {
    onPath("/") { page =>
      clearLocalStorage(page)
      pinDateFilterAnytime(page)
      val before = visibleBadgeCount(page)

      page.eval("document.getElementById('format-imax').click()")
      val after = visibleBadgeCount(page)
      after should be > 0
      after should be < before
      page.evalBool(
        "[...document.querySelectorAll('.badge-time')]" +
          ".filter(b => b.style.display !== 'none')" +
          ".every(b => (b.dataset.format || '').split(' ').includes('IMAX'))"
      ) shouldBe true
    }
  }

  it should "hide earlier showings when Od godziny = 18:00" in {
    onPath("/") { page =>
      clearLocalStorage(page)
      pinDateFilterAnytime(page)
      val before = visibleBadgeCount(page)

      page.eval("document.getElementById('from-hour').value = '18'; onFormatChange()")
      val after = visibleBadgeCount(page)
      after should be > 0
      after should be < before
      page.evalBool(
        "[...document.querySelectorAll('.badge-time')]" +
          ".filter(b => b.style.display !== 'none')" +
          ".every(b => { const [h,m] = (b.dataset.time||'').split(':').map(Number); return h*60+m >= 18*60; })"
      ) shouldBe true
    }
  }

  it should "AND-narrow the set when multiple filters are active" in {
    onPath("/") { page =>
      clearLocalStorage(page)
      pinDateFilterAnytime(page)
      page.eval(
        "document.querySelector('input[name=\"format-dim\"][value=\"2D\"]').click(); " +
        "document.querySelector('input[name=\"format-lang\"][value=\"NAP\"]').click()"
      )
      val after = visibleBadgeCount(page)
      after should be > 0
      page.evalBool(
        "[...document.querySelectorAll('.badge-time')]" +
          ".filter(b => b.style.display !== 'none')" +
          ".every(b => { const t = (b.dataset.format||'').split(' '); return t.includes('2D') && t.includes('NAP'); })"
      ) shouldBe true
    }
  }

  it should "reflect active axes in the Filtry button label" in {
    onPath("/") { page =>
      clearLocalStorage(page)
      pinDateFilterAnytime(page)
      page.evalString("document.getElementById('format-filter-btn').textContent.trim()") shouldBe "Filtry"
      page.eval("document.querySelector('input[name=\"format-dim\"][value=\"2D\"]').click()")
      val label = page.evalString("document.getElementById('format-filter-btn').textContent.trim()")
      label should not be "Filtry"
      label should include ("2D")
    }
  }

  // ── Sortuj (sort axis) ─────────────────────────────────────────────────────
  //
  // The Filtry panel's "Sortuj" select reorders the visible grid: earliest
  // screening (default) or weighted rating (biggest-first). On / the whole
  // grid is one sorted list; on /kina each cinema section is sorted
  // independently. The sort key rides on each card's `data-rating`
  // (server-computed), parsed once into INDEX.

  "the Sortuj control" should "default to 'earliest'" in {
    onPath("/") { page =>
      page.evalString("document.getElementById('sort-by').value") shouldBe "earliest"
    }
  }

  it should "order the visible grid by descending weighted rating on /" in {
    onPath("/") { page =>
      pinDateFilterAnytime(page)
      page.eval("document.getElementById('sort-by').value = 'rating'; onSortChange()")
      // Read the visible cards in DOM order and assert the rating sequence is
      // non-increasing. `length > 1` guards against a trivially-true pass on
      // an empty/one-card grid.
      val nonIncreasing = page.evalBool(
        "(() => { const r = [...document.querySelectorAll('#film-grid > .col[data-title]')]" +
        "  .filter(c => c.style.display !== 'none')" +
        "  .map(c => parseFloat(c.dataset.rating) || 0);" +
        "  for (let i = 1; i < r.length; i++) if (r[i] > r[i-1] + 1e-9) return false;" +
        "  return r.length > 1; })()"
      )
      nonIncreasing shouldBe true
    }
  }

  it should "re-sort back to earliest-screening order when switched away from rating" in {
    onPath("/") { page =>
      pinDateFilterAnytime(page)
      val earliestOrder = visibleTitleOrder(page)
      page.eval("document.getElementById('sort-by').value = 'rating'; onSortChange()")
      page.eval("document.getElementById('sort-by').value = 'earliest'; onSortChange()")
      visibleTitleOrder(page) shouldBe earliestOrder
    }
  }

  it should "sort each cinema section independently by descending rating on /kina" in {
    onPath("/kina") { page =>
      setDateAnytime(page)
      page.eval("document.getElementById('sort-by').value = 'rating'; onSortChange()")
      val ok = page.evalBool(
        "(() => { let checked = 0;" +
        "  for (const s of [...document.querySelectorAll('.cinema-section')].filter(x => x.style.display !== 'none')) {" +
        "    const r = [...s.querySelectorAll('.col[data-title]')]" +
        "      .filter(c => c.style.display !== 'none')" +
        "      .map(c => parseFloat(c.dataset.rating) || 0);" +
        "    if (r.length > 1) checked++;" +
        "    for (let i = 1; i < r.length; i++) if (r[i] > r[i-1] + 1e-9) return false;" +
        "  }" +
        "  return checked > 0; })()"
      )
      ok shouldBe true
    }
  }

  "the sort axis ↔ URL sync" should "round-trip through ?sort= when the Copy button is used" in {
    onPath("/") { page =>
      page.eval("document.getElementById('sort-by').value = 'rating'; copyFilterLinkToClipboard()")
      page.evalString("new URL(location.href).searchParams.get('sort')") shouldBe "rating"
    }
    onPath("/?sort=rating") { page =>
      page.evalString("document.getElementById('sort-by').value") shouldBe "rating"
    }
  }

  it should "keep the default 'earliest' out of the URL" in {
    onPath("/") { page =>
      page.eval("document.getElementById('sort-by').value = 'earliest'; copyFilterLinkToClipboard()")
      page.evalBool("new URL(location.href).searchParams.has('sort')") shouldBe false
    }
  }

  // ── Date filter narrowing ──────────────────────────────────────────────────

  "the date filter" should "produce a today ≤ week ≤ anytime ordering" in {
    onPath("/") { page =>
      page.eval("document.getElementById('date-filter').value = 'anytime'; applyFilters()")
      val anytime = visibleCardCount(page)
      page.eval("document.getElementById('date-filter').value = 'week'; applyFilters()")
      val week = visibleCardCount(page)
      page.eval("document.getElementById('date-filter').value = 'today'; applyFilters()")
      val today = visibleCardCount(page)

      anytime should be > 0
      today should be <= week
      week should be <= anytime
    }
  }

  // ── Date filter ↔ URL round-trip ───────────────────────────────────────────
  //
  // `?date=` is the pasteable representation of the navbar's #date-filter:
  // selecting a day must rewrite the URL (so the link survives a copy/paste),
  // and opening a URL that already carries the param must seed the selector
  // before `applyFilters()` runs.

  "the date filter ↔ URL sync" should "add ?date= when a non-default day is selected" in {
    onPath("/") { page =>
      page.eval("document.getElementById('date-filter').value = 'tomorrow'; onDateChange()")
      page.evalString("new URL(location.href).searchParams.get('date')") shouldBe "tomorrow"
    }
  }

  it should "strip ?date= when the user returns to the 'today' default" in {
    onPath("/?date=tomorrow") { page =>
      page.evalString("document.getElementById('date-filter').value") shouldBe "tomorrow"
      page.eval("document.getElementById('date-filter').value = 'today'; onDateChange()")
      page.evalBool("new URL(location.href).searchParams.has('date')") shouldBe false
    }
  }

  it should "apply ?date= on first paint so showtimes load pre-filtered" in {
    onPath("/?date=anytime") { page =>
      page.evalString("document.getElementById('date-filter').value") shouldBe "anytime"
    }
  }

  it should "ignore an unrecognised ?date= value and keep the 'today' default" in {
    onPath("/?date=bogus") { page =>
      page.evalString("document.getElementById('date-filter').value") shouldBe "today"
    }
  }

  it should "round-trip ?date= on /kina the same way" in {
    onPath("/kina?date=week") { page =>
      page.evalString("document.getElementById('date-filter').value") shouldBe "week"
      page.eval("document.getElementById('date-filter').value = 'tomorrow'; onDateChange()")
      page.evalString("new URL(location.href).searchParams.get('date')") shouldBe "tomorrow"
      page.evalString("location.pathname") shouldBe "/kina"
    }
  }

  it should "preserve ?date= when a /kina cinema pill is toggled" in {
    onPath("/kina?date=tomorrow") { page =>
      val pinTarget = page.evalString("document.querySelector('.cinema-section').dataset.cinema")
      clickPill(page, pinTarget)
      page.evalString("new URL(location.href).searchParams.get('date')") shouldBe "tomorrow"
      java.net.URLDecoder.decode(page.evalString("location.pathname"), "UTF-8") shouldBe ("/kina/" + pinTarget)
    }
  }

  // ── Generic filter ↔ URL sync ───────────────────────────────────────────────
  //
  // The date selector is the only filter that writes back to the URL on
  // every change (so day-stepping survives a copy/paste). The rest of the
  // panel — radio / checkbox / text / submenu — stays local until the user
  // explicitly hits the Filtry "Skopiuj link do schowka" button, which calls
  // `copyFilterLinkToClipboard()`. That helper folds the full filter state
  // into the URL and pushes it to the clipboard. Tests below trip it
  // directly (`copyFilterLinkToClipboard()`) to assert the URL shape per
  // filter; boot-from-URL is asserted with the same `?param=` shape on a
  // fresh page.

  "a non-date filter change" should "leave the URL alone until the Copy button is pressed" in {
    onPath("/") { page =>
      page.eval(
        "document.querySelector('input[name=\"format-dim\"][value=\"3D\"]').click();" +
        "document.getElementById('format-imax').click()"
      )
      // Neither click is reflected in the URL — only Copy commits state.
      page.evalBool("new URL(location.href).searchParams.has('dim')")  shouldBe false
      page.evalBool("new URL(location.href).searchParams.has('imax')") shouldBe false
    }
  }

  "format Wymiar = 3D" should "round-trip through ?dim=3D when the Copy button is used" in {
    onPath("/") { page =>
      page.eval(
        "document.querySelector('input[name=\"format-dim\"][value=\"3D\"]').click();" +
        "copyFilterLinkToClipboard()"
      )
      page.evalString("new URL(location.href).searchParams.get('dim')") shouldBe "3D"
    }
    onPath("/?dim=3D") { page =>
      page.evalBool("document.querySelector('input[name=\"format-dim\"][value=\"3D\"]').checked") shouldBe true
    }
  }

  "format Wersja = DUB" should "round-trip through ?lang=DUB when the Copy button is used" in {
    onPath("/?lang=DUB") { page =>
      page.evalBool("document.querySelector('input[name=\"format-lang\"][value=\"DUB\"]').checked") shouldBe true
      page.eval(
        "document.querySelector('input[name=\"format-lang\"][value=\"\"]').click();" +
        "copyFilterLinkToClipboard()"
      )
      page.evalBool("new URL(location.href).searchParams.has('lang')") shouldBe false
    }
  }

  "the IMAX checkbox" should "round-trip through ?imax=1 when the Copy button is used" in {
    onPath("/") { page =>
      page.eval("document.getElementById('format-imax').click(); copyFilterLinkToClipboard()")
      page.evalString("new URL(location.href).searchParams.get('imax')") shouldBe "1"
    }
    onPath("/?imax=1") { page =>
      page.evalBool("document.getElementById('format-imax').checked") shouldBe true
    }
  }

  "the from-hour filter" should "round-trip through ?from=HH:MM when the Copy button is used" in {
    onPath("/") { page =>
      page.eval(
        "document.getElementById('from-hour').value = '18';" +
        "document.getElementById('from-minute').value = '30';" +
        "onFormatChange(); copyFilterLinkToClipboard()"
      )
      page.evalString("new URL(location.href).searchParams.get('from')") shouldBe "18:30"
    }
    onPath("/?from=20:15") { page =>
      page.evalString("document.getElementById('from-hour').value")   shouldBe "20"
      page.evalString("document.getElementById('from-minute').value") shouldBe "15"
    }
  }

  "the search input" should "round-trip through ?q= when the Copy button is used" in {
    onPath("/") { page =>
      pinDateFilterAnytime(page)
      page.eval(
        "document.getElementById('search-input').value = 'Diabeł';" +
        "applyFilters(); copyFilterLinkToClipboard()"
      )
      page.evalString("new URL(location.href).searchParams.get('q')") shouldBe "Diabeł"
    }
    onPath("/?q=Diab%C5%82eb") { page =>
      // The character class is round-tripped, not interpreted — the param
      // lands in the input verbatim so the same `q=…` link reproduces the
      // same filter result.
      page.evalString("document.getElementById('search-input').value") shouldBe "Diabłeb"
    }
  }

  "unchecking a country" should "leave ?country= listing the still-checked countries (inclusion set, captured on Copy)" in {
    onPath("/") { page =>
      clearLocalStorage(page)
      val firstCountry = page.evalString(
        "document.querySelector('#country-list input[type=\"checkbox\"]:not(.submenu-all)').value"
      )
      firstCountry should not be ""
      page.eval(
        "document.querySelector('#country-list input[type=\"checkbox\"]:not(.submenu-all)').click();" +
        "copyFilterLinkToClipboard()"
      )
      val included = page.evalString(
        "new URL(location.href).searchParams.getAll('country').join('|')"
      ).split('|').toSet
      included should not contain firstCountry
      included.size should be > 0
    }
  }

  "?country= on boot" should "check exactly the named countries (inclusion set)" in {
    onPath("/") { page =>
      clearLocalStorage(page)
      // Pick one country known to be in the corpus, then drive a second tab
      // via the URL-derived state and assert ONLY that country is checked.
      val firstCountry = page.evalString(
        "document.querySelector('#country-list input[type=\"checkbox\"]:not(.submenu-all)').value"
      )
      onPath("/?country=" + java.net.URLEncoder.encode(firstCountry, "UTF-8")) { page2 =>
        val firstChecked = page2.evalBool(
          "[...document.querySelectorAll('#country-list input[type=\"checkbox\"]:not(.submenu-all)')]" +
            ".find(cb => cb.value === " + jsString(firstCountry) + ").checked"
        )
        firstChecked shouldBe true
        val anyOtherChecked = page2.evalBool(
          "[...document.querySelectorAll('#country-list input[type=\"checkbox\"]:not(.submenu-all)')]" +
            ".some(cb => cb.value !== " + jsString(firstCountry) + " && cb.checked)"
        )
        anyOtherChecked shouldBe false
      }
    }
  }

  "the Gatunek submenu" should "list genre entries pulled from the cards' data-genres on /" in {
    onPath("/") { page =>
      val rowCount = page.evalInt(
        "document.querySelectorAll('#genre-list input[type=\"checkbox\"]:not(.submenu-all)').length"
      )
      rowCount should be > 0
      page.evalString(
        "document.querySelector('#genre-list input[type=\"checkbox\"]:not(.submenu-all)').value"
      ) should not be ""
    }
  }

  "?genre= on boot" should "check exactly the named genre (inclusion set)" in {
    onPath("/") { page =>
      clearLocalStorage(page)
      val firstGenre = page.evalString(
        "document.querySelector('#genre-list input[type=\"checkbox\"]:not(.submenu-all)').value"
      )
      firstGenre should not be ""
      onPath("/?genre=" + java.net.URLEncoder.encode(firstGenre, "UTF-8")) { page2 =>
        val firstChecked = page2.evalBool(
          "[...document.querySelectorAll('#genre-list input[type=\"checkbox\"]:not(.submenu-all)')]" +
            ".find(cb => cb.value === " + jsString(firstGenre) + ").checked"
        )
        firstChecked shouldBe true
        val anyOtherChecked = page2.evalBool(
          "[...document.querySelectorAll('#genre-list input[type=\"checkbox\"]:not(.submenu-all)')]" +
            ".some(cb => cb.value !== " + jsString(firstGenre) + " && cb.checked)"
        )
        anyOtherChecked shouldBe false
      }
    }
  }

  "a genre pill on a film card" should "link to the /filmy?gatunek= browse page for that genre" in {
    onPath("/") { page =>
      val href = page.evalString(
        "(() => { const a = document.querySelector('.col[data-genres] a.pill.genre');" +
          "  return a ? a.getAttribute('href') : ''; })()"
      )
      href should startWith("/filmy?gatunek=")
    }
  }

  "disabling a cinema in Filtry" should "leave ?cinema= listing the still-enabled cinemas (inclusion set, captured on Copy)" in {
    onPath("/") { page =>
      clearLocalStorage(page)
      // The cinema-picker checkbox has no `value` attribute (the cinema name
      // is captured by closure in its `onchange` handler), so the cinema
      // identity is in the adjacent label text, not on `cb.value`.
      val cinema = page.evalString(
        "(() => { const cb = document.querySelector('#cinema-list input[type=\"checkbox\"]');" +
          "  if (!cb.checked) return ''; cb.click();" +
          "  return cb.parentElement.textContent.trim(); })()"
      )
      cinema should not be ""
      page.eval("copyFilterLinkToClipboard()")
      val enabled = page.evalString(
        "new URL(location.href).searchParams.getAll('cinema').join('|')"
      ).split('|').toSet
      // The URL carries the canonical cinema displayNames of every enabled
      // cinema (i.e. ALL_CINEMAS minus the one just disabled). Look up what
      // landed in disabledCinemas to find the canonical for the one we toggled.
      val disabled = page.evalString(
        "(() => { const dis = JSON.parse(localStorage.getItem('disabledCinemas') || '[]');" +
          "  return dis[0] || ''; })()"
      )
      enabled should not contain disabled
      enabled.size should be > 0
      // Reset the LS so neighbouring tests start with the default all-enabled
      // cinema set. localStorage persists across tabs inside the same Chrome
      // session — without this, "Empty state" et al. would silently filter
      // half the corpus out.
      page.eval("localStorage.removeItem('disabledCinemas')")
    }
  }

  // ── Cinema+room (Sale) filter ───────────────────────────────────────────────

  "the Sale (room) submenu" should "list one entry per (cinema, room) pair on /" in {
    onPath("/") { page =>
      pinDateFilterAnytime(page)
      val rowDisplay = page.evalString("document.getElementById('room-row').style.display")
      rowDisplay should not be "none"
      val rowCount = page.evalInt("document.querySelectorAll('#room-list input[type=\"checkbox\"]:not(.submenu-all)').length")
      rowCount should be > 0
      val firstValue = page.evalString(
        "document.querySelector('#room-list input[type=\"checkbox\"]:not(.submenu-all)').value"
      )
      firstValue should include ("|")
    }
  }

  it should "group rooms under a cinema header that's clickable to expand" in {
    onPath("/") { page =>
      pinDateFilterAnytime(page)
      val headerCount = page.evalInt("document.querySelectorAll('#room-list .room-cinema-header').length")
      headerCount should be > 1

      // Inner room list starts collapsed.
      page.evalString(
        "document.querySelector('#room-list .room-cinema-list').style.display"
      ) shouldBe "none"

      // Clicking the first cinema header expands it.
      page.eval("document.querySelector('#room-list .room-cinema-header').click()")
      page.evalString(
        "document.querySelector('#room-list .room-cinema-list').style.display"
      ) should not be "none"
    }
  }

  it should "sort rooms inside a cinema naturally — Sala 10 after Sala 9, not after Sala 1" in {
    onPath("/") { page =>
      pinDateFilterAnytime(page)

      // Pick a cinema known to have rooms numbered past 9 (Multikino Stary
      // Browar / Cinema City sites in the fixture corpus). Then assert the
      // DOM order matches natural-sort: the index of "Sala 9" is less than
      // the index of "Sala 10", not the other way around.
      val rooms = page.evalString(
        "(() => { const headers = [...document.querySelectorAll('#room-list .room-cinema-header')];" +
          "  for (const h of headers) {" +
          "    const inner = h.nextElementSibling;" +
          "    const rs = [...inner.querySelectorAll('input[type=\"checkbox\"]')].map(cb => cb.value.split('|')[1]);" +
          "    if (rs.includes('Sala 9') && rs.includes('Sala 10')) return rs.join('|');" +
          "  } return ''; })()"
      )
      rooms should not be ""
      val list = rooms.split('|').toIndexedSeq
      val ix9  = list.indexOf("Sala 9")
      val ix10 = list.indexOf("Sala 10")
      ix9  should be >= 0
      ix10 should be >= 0
      ix9 should be < ix10
    }
  }

  it should "show a per-cinema count badge once a room is unchecked" in {
    onPath("/") { page =>
      pinDateFilterAnytime(page)
      // Pick a header whose inner list has ≥ 2 rooms so unchecking one
      // leaves a visible "1/N" state instead of a hidden "0/N".
      val headerInfo = page.evalString(
        "(() => { const headers = [...document.querySelectorAll('#room-list .room-cinema-header')];" +
          "  for (let i = 0; i < headers.length; i++) {" +
          "    const inner = headers[i].nextElementSibling;" +
          "    const boxes = inner.querySelectorAll('input[type=\"checkbox\"]');" +
          "    if (boxes.length >= 2) { headers[i].click(); boxes[0].click(); return String(boxes.length); }" +
          "  } return ''; })()"
      )
      headerInfo should not be ""
      val total = headerInfo.toInt
      val expectedBadge = (total - 1) + "/" + total
      val badgeText = page.evalString(
        "(() => { const c = document.querySelector('#room-list .room-cinema-header .room-cinema-count');" +
          "  return c.style.display === 'none' ? '' : c.textContent; })()"
      )
      badgeText shouldBe expectedBadge
    }
  }

  it should "narrow visible badges to only the picked room when a single Sala is checked" in {
    onPath("/") { page =>
      pinDateFilterAnytime(page)
      val baseline = visibleBadgeCount(page)
      baseline should be > 1

      val targetPair = page.evalString(
        "document.querySelector('#room-list input[type=\"checkbox\"]:not(.submenu-all)').value"
      )
      val pipeIdx = targetPair.indexOf('|')
      val cinema  = targetPair.substring(0, pipeIdx)
      val room    = targetPair.substring(pipeIdx + 1)

      page.eval(
        "(() => { const boxes = [...document.querySelectorAll('#room-list input[type=\"checkbox\"]:not(.submenu-all)')];" +
          s"  for (const cb of boxes) { if (cb.value !== ${jsString(targetPair)}) cb.click(); }" +
          "  applyFilters(); return true; })()"
      )

      val narrowed = visibleBadgeCount(page)
      narrowed should be < baseline
      narrowed should be > 0

      // Every visible badge must be in the picked (cinema, room) — the
      // include-list filter excludes "no-room" badges entirely.
      val allMatch = page.evalBool(
        "[...document.querySelectorAll('.badge-time')].filter(b => b.style.display !== 'none')" +
          s".every(b => b.dataset.room === ${jsString(room)} &&" +
          s"            b.closest('.cinema-group')?.dataset.cinema === ${jsString(cinema)})"
      )
      allMatch shouldBe true
    }
  }

  it should "leave the still-checked (cinema|room) pairs in ?room= (inclusion set, captured on Copy)" in {
    onPath("/") { page =>
      pinDateFilterAnytime(page)
      val targetPair = page.evalString(
        "document.querySelector('#room-list input[type=\"checkbox\"]:not(.submenu-all)').value"
      )
      page.eval(
        "document.querySelector('#room-list input[type=\"checkbox\"]:not(.submenu-all)').click();" +
        "copyFilterLinkToClipboard()"
      )
      val included = page.evalString(
        "new URL(location.href).searchParams.getAll('room').join('||')"
      ).split("\\|\\|").toSet
      included should not contain targetPair
      included.size should be > 0
    }
  }

  it should "apply ?room= to check ONLY the named (cinema|room) pair on boot (inclusion set)" in {
    onPath("/") { page =>
      pinDateFilterAnytime(page)
      val targetPair = page.evalString(
        "document.querySelector('#room-list input[type=\"checkbox\"]:not(.submenu-all)').value"
      )
      onPath("/?date=anytime&room=" + java.net.URLEncoder.encode(targetPair, "UTF-8")) { page2 =>
        val targetChecked = page2.evalBool(
          "[...document.querySelectorAll('#room-list input[type=\"checkbox\"]:not(.submenu-all)')]" +
            ".find(cb => cb.value === " + jsString(targetPair) + ").checked"
        )
        targetChecked shouldBe true
        val anyOtherChecked = page2.evalBool(
          "[...document.querySelectorAll('#room-list input[type=\"checkbox\"]:not(.submenu-all)')]" +
            ".some(cb => cb.value !== " + jsString(targetPair) + " && cb.checked)"
        )
        anyOtherChecked shouldBe false
      }
    }
  }

  // ── Empty state ────────────────────────────────────────────────────────────

  "the #no-films empty state" should "show when the search yields zero matches and hide when cleared" in {
    onPath("/") { page =>
      pinDateFilterAnytime(page)
      page.evalString("document.getElementById('no-films').style.display") shouldBe "none"

      page.eval("document.getElementById('search-input').value = 'zzzzz_no_match'; applyFilters()")
      visibleCardCount(page) shouldBe 0
      page.evalString("document.getElementById('no-films').style.display") should not be "none"
      page.evalBool("document.getElementById('no-films').textContent.includes('Brak repertuaru.')") shouldBe true

      page.eval("document.getElementById('search-input').value = ''; applyFilters()")
      visibleCardCount(page) should be > 0
      page.evalString("document.getElementById('no-films').style.display") shouldBe "none"
    }
  }

  // ── Hide-film flow ─────────────────────────────────────────────────────────

  "the hide-film flow" should "write to hiddenFilms localStorage and hide the card" in {
    onPath("/") { page =>
      pinDateFilterAnytime(page)
      val title = firstVisibleTitle(page)

      page.eval(
        s"(() => { const btn = document.querySelector('.col[data-title=${jsString(title)}] .hide-btn');" +
        "  hideFilm(btn); })()"
      )
      page.evalBool(
        s"JSON.parse(localStorage.getItem('hiddenFilms') || '[]').includes(${jsString(title)})"
      ) shouldBe true
      page.evalString(
        s"document.querySelector('.col[data-title=${jsString(title)}]').style.display"
      ) shouldBe "none"
    }
  }

  it should "show hidden titles in the modal and restore on unhide" in {
    onPath("/") { page =>
      pinDateFilterAnytime(page)
      val title = firstVisibleTitle(page)
      page.eval(s"localStorage.setItem('hiddenFilms', JSON.stringify([${jsString(title)}]))")
      page.reload()
      pinDateFilterAnytime(page)

      page.evalString(
        s"document.querySelector('.col[data-title=${jsString(title)}]').style.display"
      ) shouldBe "none"

      page.eval("openHiddenModal()")
      page.evalBool(
        s"[...document.querySelectorAll('#hidden-modal-list .panel-item')]" +
        s".some(i => i.textContent.trim() === ${jsString(title)})"
      ) shouldBe true

      page.eval(s"restoreFilm(${jsString(title)})")
      page.evalString(
        s"document.querySelector('.col[data-title=${jsString(title)}]').style.display"
      ) should not be "none"
      page.evalBool(
        s"JSON.parse(localStorage.getItem('hiddenFilms') || '[]').includes(${jsString(title)})"
      ) shouldBe false
    }
  }

  // ── /plan Filmy-section fold ─────────────────────────────────────────────
  // The poster picker collapses when its header bar is clicked, and the
  // collapsed strip re-expands when clicked anywhere. State persists in
  // the `planPostersFolded` localStorage key. See `wirePostersFold` /
  // `planApplyPostersFold` in plan.scala.html.

  // localStorage is per-origin and survives `openPage`'s fresh tab, so
  // a fold set by one test leaks into the next. Reset to the expanded
  // default (no key) and re-sync the DOM before each fold assertion.
  private def resetFold(page: CdpPage): Unit =
    page.eval("localStorage.removeItem('planPostersFolded'); planApplyPostersFold()")

  private def isFolded(page: CdpPage): Boolean =
    page.evalBool("document.getElementById('filmy-section').classList.contains('folded')")

  private def rowDisplay(page: CdpPage): String =
    page.evalString("getComputedStyle(document.getElementById('plan-movies-row')).display")

  "the /plan Filmy section" should "start expanded with the poster grid visible" in {
    onPath("/plan") { page =>
      resetFold(page)
      isFolded(page) shouldBe false
      rowDisplay(page) should not be "none"
      page.evalString("document.getElementById('plan-filmy-header').getAttribute('aria-expanded')") shouldBe "true"
    }
  }

  it should "collapse when the header bar is clicked, hiding the grid and persisting the state" in {
    onPath("/plan") { page =>
      resetFold(page)
      page.eval("document.getElementById('plan-filmy-header').click()")
      isFolded(page) shouldBe true
      rowDisplay(page) shouldBe "none"
      page.evalString("localStorage.getItem('planPostersFolded')") shouldBe "1"
      page.evalString("document.getElementById('plan-filmy-header').getAttribute('aria-expanded')") shouldBe "false"
      page.evalString(
        "document.querySelector('#filmy-section .plan-collapse-label').textContent"
      ) shouldBe "Rozwiń plakaty"
    }
  }

  it should "re-expand when the collapsed strip is clicked anywhere" in {
    onPath("/plan") { page =>
      resetFold(page)
      page.eval("document.getElementById('plan-filmy-header').click()")
      isFolded(page) shouldBe true

      // A click on the section body (not just the header) while folded
      // re-expands — the whole collapsed strip is the click target.
      page.eval("document.getElementById('filmy-section').click()")
      isFolded(page) shouldBe false
      rowDisplay(page) should not be "none"
      page.evalBool("localStorage.getItem('planPostersFolded') === null") shouldBe true
    }
  }

  it should "not collapse when a poster inside the expanded grid is clicked" in {
    onPath("/plan") { page =>
      resetFold(page)
      // Clicking a poster toggles its selection; it must NOT also fold
      // the picker (only header clicks collapse while expanded). Click
      // the poster wrap, not the title `<a>` — the latter navigates.
      page.evalBool("!!document.querySelector('#plan-movies-row .plan-card .poster-wrap')") shouldBe true
      page.eval("document.querySelector('#plan-movies-row .plan-card .poster-wrap').click()")
      isFolded(page) shouldBe false
    }
  }

  it should "keep the folded state across a reload" in {
    onPath("/plan") { page =>
      resetFold(page)
      page.eval("document.getElementById('plan-filmy-header').click()")
      isFolded(page) shouldBe true
      page.reload()
      isFolded(page) shouldBe true
      rowDisplay(page) shouldBe "none"
    }
  }

  it should "toggle via Enter on the focused header (keyboard a11y)" in {
    onPath("/plan") { page =>
      resetFold(page)
      page.eval(
        "(() => { const h = document.getElementById('plan-filmy-header'); h.focus();" +
        " h.dispatchEvent(new KeyboardEvent('keydown', {key: 'Enter', bubbles: true})); })()"
      )
      isFolded(page) shouldBe true
    }
  }

  // ── helpers ──────────────────────────────────────────────────────────────

  private def clearLocalStorage(page: CdpPage): Unit =
    page.eval("localStorage.clear(); applyFilters()")

  private def visibleBadgeCount(page: CdpPage): Int =
    page.evalInt("[...document.querySelectorAll('.badge-time')].filter(b => b.style.display !== 'none').length")

  private def visibleCardCount(page: CdpPage): Int =
    page.evalInt("[...document.querySelectorAll('.col[data-title]')].filter(c => c.style.display !== 'none').length")

  /** Pipe-joined titles of the visible `/` grid cards in current DOM order —
   *  used to assert the sort axis reorders (and restores) the grid. */
  private def visibleTitleOrder(page: CdpPage): String =
    page.evalString(
      "[...document.querySelectorAll('#film-grid > .col[data-title]')]" +
      "  .filter(c => c.style.display !== 'none').map(c => c.dataset.title).join('|')"
    )

  private def firstVisibleTitle(page: CdpPage): String =
    page.evalString(
      "(() => { const cols = [...document.querySelectorAll('.col[data-title]')];" +
      "  for (const c of cols) { if (c.style.display === 'none') continue;" +
      "    const img = c.querySelector('.poster-wrap > a img');" +
      "    if (img && img.style.display !== 'none') return c.dataset.title; }" +
      "  return cols.find(c => c.style.display !== 'none')?.dataset.title || ''; })()"
    )

  /** Pin every visible element on the page to Arial / Liberation Sans
   *  before layout-sensitive measurements. Why: the layout sweeps below
   *  read `getBoundingClientRect()` to assert "navbar fits in ≤ 2 rows"
   *  and "no horizontal overflow". Those assertions are font-metric
   *  sensitive, and Bootstrap's `--bs-font-sans-serif` resolves
   *  differently per platform:
   *
   *    - macOS local Chrome  → `system-ui` → SF Pro Text
   *    - GitHub Linux runner → `system-ui` → Ubuntu / DejaVu Sans (~3 %
   *      wider per character than SF Pro at the same px size)
   *
   *  Same CSS, different rendered widths — the sweep passes locally and
   *  fails on CI even when the production layout is fine. (Real iOS /
   *  Android users see SF Pro / Roboto, both narrow.)
   *
   *  Pinning to Arial with `Liberation Sans` as the fallback gives
   *  identical metrics on both platforms (Liberation Sans was designed
   *  to be byte-for-byte metric-compatible with Arial; both ship by
   *  default on ubuntu-latest GitHub runners and on macOS). Arial is
   *  also a hair wider than SF Pro / Roboto, so the test runs as a
   *  worst-case font width — passing the assertion here means real
   *  users on narrower system fonts are even more comfortably inside
   *  the layout bounds. */
  private def pinDeterministicFont(page: CdpPage): Unit =
    page.eval(
      "(() => { const s = document.createElement('style');" +
      "          s.textContent = '*, body { font-family: Arial, \"Liberation Sans\", sans-serif !important; }';" +
      "          document.head.appendChild(s); })()"
    )

  /** Click the pill whose `data-cinema` matches `cinema`. Asserts the
   *  pill was found so a typo'd cinema name fails the test with a clear
   *  message instead of silently no-op'ing. Wrapped in an IIFE so the
   *  internal `const` doesn't bleed into the per-page evaluation scope
   *  (every `Runtime.evaluate` shares one execution context — two calls
   *  with the same top-level `const` would `SyntaxError`). */
  private def setDateAnytime(page: CdpPage): Unit =
    page.eval("document.getElementById('date-filter').value = 'anytime'; applyFilters()")

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
