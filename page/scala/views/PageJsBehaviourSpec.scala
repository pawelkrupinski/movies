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
      // 414 × 896 — iPhone XR / 11 / 12 / 13 / 14 / 14 Pro default
      // viewport. The sub-iPhone-SE width (375) can't fit
      // [logo+tabs] [search] [auth] on a single row even with a 70-px
      // search input, so the spec's "search next to auth on row 1"
      // can only be verified at 380+ px widths; 414 is the modal
      // iPhone viewport since 2018.
      // `mobile: true` switches CDP's emulation to mobile mode, which on
      // Linux headless Chrome (CI) ignores the explicit width override
      // and falls back to whatever the launch-flag default is. Sticking
      // to desktop-shaped emulation with just the width + height
      // override applies cleanly on both macOS (local) and CI Chrome.
      page.send("Emulation.setDeviceMetricsOverride", play.api.libs.json.Json.obj(
        "width" -> 414, "height" -> 896, "deviceScaleFactor" -> 1.0, "mobile" -> false
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
        viewportWidth shouldBe 414
        math.abs(searchTop - authTop) should be < 4.0
        searchLeft should be < authLeft
      }

      // Row 2: date and filtry share the same top, below row 1.
      math.abs(dateTop - filtryTop) should be < 4.0
      dateTop should be > authTop
      dateLeft should be < filtryLeft

      // Reset emulation so the next test starts at the default viewport.
      page.send("Emulation.clearDeviceMetricsOverride", play.api.libs.json.Json.obj())
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
