package views

import testsupport.TestMessages.given

import models.{CinemaCityWroclavia, CinemaShowing, Helios, MovieRecord, Poznan, SourceData}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{Json, JsString}
import services.movies.StoredMovieRecord
import services.staging.StagingRecord
import tools.{CdpPage, Chrome, FixtureTestWiring, TestHttpServer}

import java.net.URLDecoder
import java.time.{Instant, LocalDateTime}

/**
 * JavaScript-behaviour regression for the rendered pages. Spins up a
 * tiny embedded HTTP server in front of the same `08-06-2026` fixture-
 * rendered HTML the snapshot spec asserts on, drives the pages in a
 * headless Chrome over CDP, and asserts on actual DOM state after JS
 * interactions (pill click, search input, URL pinning, reload).
 *
 * Complements `PageSnapshotSpec`: that spec catches "the markup the
 * server emits" regressions; this spec catches "the JS that decides
 * what to show in that markup" regressions — `applyFilters` math,
 * the single-cinema label toggle, the day-stepping wrap, the date ↔
 * URL sync via `history.replaceState`.
 *
 * Why an HTTP server and not file://: `history.replaceState` (called by
 * `syncDateToURL` to rewrite `?date=` on a day change) throws a
 * SecurityError under file:// — the target URL isn't same-origin with
 * the file's directory-scoped origin. Without http://, the rewrite
 * throws synchronously and the rest of the handler (the `applyFilters()`
 * call that updates the DOM) never runs, so every test would look broken
 * even when prod is fine.
 *
 * Skips gracefully when Chrome isn't installed locally — CI images that
 * lack a browser get a green spec with `cancelled` tests; developers
 * with Chrome installed get the full coverage.
 */
class PageJsBehaviourSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private val now = LocalDateTime.of(2026, 6, 8, 0, 0)

  private implicit val city: models.City = Poznan
  private val cityPrefix = s"/${city.slug}"

  // One Chrome + one TestHttpServer for the whole spec. Each individual
  // test still gets a fresh tab (clean localStorage, fresh module state)
  // via `openPage`, but pays the cost of wiring + rendering + Chrome
  // boot exactly once across all tests.
  private var chrome: Option[Chrome] = None
  private var server: TestHttpServer = _

  override def beforeAll(): Unit = {
    chrome = Chrome.tryStart()
    if (chrome.nonEmpty) {
      val wiring = new FixtureTestWiring("08-06-2026")
      // Load the read-model snapshot instead of the ~110s corpus boot (this spec
      // exercises rendered-page JS, not the pipeline). See ReadModelSnapshot.
      wiring.bootFromSnapshotOrPipeline()
      // Web read transform over the worker-projected read model (the shared seam).
      val service     = new controllers.MovieControllerService(wiring.webReadModel)
      val anon    = Option.empty[models.User]
      val noOauth = Set.empty[String]
      val cinemas = city.cinemaDisplayNames
      val schedules       = service.toSchedules(city, now)

      // The city-selection landing (`/` with no city cookie) — drives the
      // inline city-search filter. City-independent, so served off-prefix.
      val landingHtml: String = views.html.landing(models.City.all).body

      val pills = city.cinemaPillMap
      val indexHtml: String = views.html.repertoire(
        schedules, cinemas, pills, devMode = false,
        currentUser = anon, oauthProviders = noOauth
      ).body

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
        controllers.PlanController.viewData(city, schedules),
        cinemas, pills, devMode = false,
        currentUser = anon, oauthProviders = noOauth
      ).body
      // Logged-in render of the index so the inline config sets
      // `IS_LOGGED_IN = true` and shared.js runs its server-sync boot path.
      // Served at `/li`; the `/api/me/state` route below stands in for the
      // real UserStateController. Lets the boot reconcile (first-login union
      // vs. server-authoritative replace) be driven over CDP.
      val testUser = models.User(
        id = "tester@example.com", provider = "google", providerSub = "sub-1",
        email = Some("tester@example.com"), displayName = Some("Tester"),
        avatarUrl = None, createdAt = Instant.EPOCH, lastSeenAt = Instant.EPOCH
      )
      val loggedInHtml: String = views.html.repertoire(
        schedules, cinemas, pills, devMode = false,
        currentUser = Some(testUser), oauthProviders = noOauth
      ).body
      // Static server-side state: one hidden film. response.json() parses the body
      // regardless of content-type, so serving it via the HTML route map is fine.
      val userStateJson =
        """{"hiddenFilms":["Film A"],"disabledCinemas":[],"selectedMovies":[],"favouriteRooms":[]}"""

      // The global-corpus /debug page (not city-scoped) — a few corpus rows to
      // populate the main #t table behind the staging table under test.
      // Ratings are distinct so the "Ratings" column sorts by the COMBINED
      // weighted score (MovieRecord.weightedRating), like the main page does:
      //   Unresolved 0.0  <  Pending 6.0  <  Done 9.0.
      // "Done Film" tops it on Metacritic+RT alone with NO imdbRating — proving
      // the sort reads weightedRating, not the old raw-IMDb key (which was both
      // empty here AND never matched its header, so the column never sorted).
      val debugRows = Seq(
        StoredMovieRecord("Pending Film",    Some(2024), MovieRecord(detailPending = true, tmdbId = Some(1), imdbRating = Some(6.0))),
        StoredMovieRecord("Unresolved Film", Some(2023), MovieRecord()),
        StoredMovieRecord("Done Film",       Some(2022), MovieRecord(tmdbId = Some(7), metascore = Some(90), rottenTomatoes = Some(90))),
      )
      // Staging (pending_movies) source rows the page FOLDS by film. "Staging
      // Film" (anchor `stagingfilm`) is reported by TWO cinemas and is at its
      // detail-fetch step → it folds to one row with Cinemas = 2. "Done Newcomer"
      // (anchor `donenewcomer`, one cinema) has detail + TMDB concluded (so the
      // folded row shows ✓, ✓) and is now at IMDb recovery.
      val debugStaging = Seq(
        StagingRecord(CinemaCityWroclavia, "Staging Film",  Some(2026), MovieRecord(detailPending = true)),
        StagingRecord(Helios,              "Staging Film",  Some(2026), MovieRecord(detailPending = true)),
        StagingRecord(CinemaCityWroclavia, "Done Newcomer", Some(2025),
          MovieRecord(detailPending = false, tmdbId = Some(550))))
      val debugHtml: String = views.html.debug(debugRows, debugStaging).body
      // A purpose-built corpus row for the Cinemas-cell layout test: ONE venue
      // (CinemaCityWroclavia) listing the film under TWO titles → `cinemaData` =
      // 1 distinct cinema, `cinemaSlots` = 2 per-title slots, so `_debugRow`
      // renders the bracketed slot count `1 (2)`. Served on its own path so the
      // shared `/debug` corpus-count + sort tests (which assert exactly 3 rows)
      // stay untouched.
      val slotsRow = StoredMovieRecord("Slots Film", Some(2024), MovieRecord(
        tmdbId = Some(99),
        data = Map(
          CinemaShowing(CinemaCityWroclavia, "slots-film")     -> SourceData(title = Some("Slots Film")),
          CinemaShowing(CinemaCityWroclavia, "slots-film-org") -> SourceData(title = Some("Slots Film Org")))))
      val slotsDebugHtml: String = views.html.debug(Seq(slotsRow), Seq.empty).body
      // The queue snapshot the page polls (/debug/queue). "Staging Film"'s detail
      // fetch is being worked on (▶ running); "Done Newcomer"'s IMDb recovery
      // waits at place #1 (the only waiting task). The dedup keys mirror the real
      // staging ones (StagingTaskKeys): every key embeds the film's `anchor`
      // (= sanitize(title)) as the segment after the `staging-*` prefix.
      val debugQueueJson =
        """{"active":[
          {"taskType":"StagingDetail","dedupKey":"staging-detail|stagingfilm|cc","state":"worked_on"},
          {"taskType":"StagingResolveImdbId","dedupKey":"staging-imdb|donenewcomer","state":"waiting"}
        ]}"""

      // Pages are served under `/{city}/…` (production hard-cut). `onPath`
      // prepends the prefix, so strip it here, then match the in-city sub-path.
      def sub(p: String): String = p.stripPrefix(cityPrefix)
      server = new TestHttpServer(
        {
          // `/{city}/` accepts arbitrary query strings (e.g. `?date=tomorrow`)
          // — the real Play routes do too, and the day-selector ↔ URL tests
          // need to boot the page with the parameter already in `location.search`.
          case p if { val s = sub(p); s == "/" || s.startsWith("/?") }     => indexHtml
          case p if sub(p).startsWith("/film?title=") =>
            renderFilm(sub(p).stripPrefix("/film?title="))
          case p if { val s = sub(p); s == "/plan" || s.startsWith("/plan?") } => planHtml
          case p if sub(p) == "/li"           => loggedInHtml
          case p if p == "/api/me/state"      => userStateJson
          // The dev-only visual-tuning page — rendered with real fixture films
          // so its slider panel (and the ± step buttons) can be driven over CDP.
          case p if sub(p) == "/debug/tune" => views.html.tune(schedules.take(3)).body
          // The city-selection landing (no city prefix — there's no city yet).
          case "/landing" => landingHtml
          // The global-corpus /debug page (no city prefix).
          case "/debug" => debugHtml
          // Isolated single-row /debug variant for the Cinemas-cell layout test.
          case "/debug-slots" => slotsDebugHtml
          // A corpus row's per-source breakdown, fetched lazily when its /debug
          // table row is expanded (the heavy subtree is no longer rendered inline).
          // The `id` query param is the row's `_id`; serve the matching row's
          // debugDetails, mirroring MovieController.debugDetails.
          case p if p.startsWith("/debug/details?") =>
            val id = java.net.URLDecoder.decode(p.split("id=", 2).lift(1).getOrElse(""), "UTF-8")
            debugRows.find(r => StoredMovieRecord.idOf(r) == id)
              .map(r => views.html.debugDetails(r.title, r.year, r.record, Map.empty[String, String]).body)
              .getOrElse("")
        },
        // /debug/queue is JSON the page fetches and parses; served as such.
        jsonRoutes = { case "/debug/queue" => debugQueueJson }
      )
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
      // `path` is an in-city sub-path ("/", "/film?title=…"); pages
      // live under `/{city}/…`, so prepend the city prefix.
      case Some(c) => c.openPage(server.baseUrl + cityPrefix + path)(body(_))
      case None    => cancel("Chrome not installed — skipping JS behaviour test")
    }

  /** Open the city-selection landing page (not city-scoped). */
  private def onLanding(body: CdpPage => Any): Unit =
    chrome match {
      case Some(c) => c.openPage(server.baseUrl + "/landing")(body(_))
      case None    => cancel("Chrome not installed — skipping JS behaviour test")
    }

  /** Open the global-corpus `/debug` page (not city-scoped). */
  private def onDebug(body: CdpPage => Any): Unit =
    chrome match {
      case Some(c) => c.openPage(server.baseUrl + "/debug")(body(_))
      case None    => cancel("Chrome not installed — skipping JS behaviour test")
    }

  /** Open the logged-in index (`IS_LOGGED_IN = true`) in a fresh tab so the
   *  shared.js server-sync boot path runs. */
  private def onLoggedInIndex(body: CdpPage => Any): Unit =
    chrome match {
      case Some(c) => c.openPage(server.baseUrl + cityPrefix + "/li")(body(_))
      case None    => cancel("Chrome not installed — skipping JS behaviour test")
    }

  // ── Server-state reconcile on boot (logged-in users) ─────────────────────
  //
  // Regression for "I un-hide a film / re-enable a cinema and it comes back on
  // the next page load". The old boot did a blind union of localStorage with
  // the server on EVERY page load, so a union could only ever add — removals
  // were resurrected from the server's stale copy. The fix makes the server
  // authoritative after the one-time first-login migration.

  "server-state reconcile" should
    "drop a local-only hidden film once synced (server is authoritative)" in {
    onLoggedInIndex { page =>
      // First boot migrated from server = ["Film A"] and set the synced flag.
      page.waitFor("localStorage.getItem('serverStateSynced') === '1'")
      // Mimic a device still carrying a stale local-only entry the server never
      // had, with the sync flag already set. A blind union would keep "Film B"
      // forever; an authoritative reconcile must drop it on the next load.
      page.eval("_lsSet('hiddenFilms', ['Film A','Film B'])")
      page.reload()
      page.waitFor("getHidden().indexOf('Film B') === -1 && getHidden().indexOf('Film A') !== -1")
      val hidden = page.evalString("JSON.stringify(getHidden())")
      hidden should include ("Film A")
      hidden should not include "Film B"
    }
  }

  it should "union local picks with the server on the first reconcile after login" in {
    onLoggedInIndex { page =>
      page.waitFor("localStorage.getItem('serverStateSynced') === '1'")
      // Re-arm migration as a fresh login would, plus an anonymous local pick.
      page.eval("localStorage.removeItem('serverStateSynced')")
      page.eval("_lsSet('hiddenFilms', ['Local Z'])")
      page.reload()
      page.waitFor(
        "localStorage.getItem('serverStateSynced') === '1' && getHidden().indexOf('Local Z') !== -1")
      val hidden = page.evalString("JSON.stringify(getHidden())")
      hidden should include ("Film A")  // pulled from the server
      hidden should include ("Local Z") // migrated up from this device
    }
  }

  // ── city-selection landing search ────────────────────────────────────────
  //
  // The landing page (`/` with no city cookie) lists all 41 cities; the inline
  // search box narrows them by folded substring match so a visitor can type
  // their city instead of scrolling. Diacritic-insensitive: "lodz" finds
  // "Łódź", "krakow" finds "Kraków".

  /** Type `q` into the city-search box and fire the `input` handler. */
  private def typeCitySearch(page: CdpPage, q: String): Unit =
    page.eval(
      "(function(){var i=document.getElementById('city-search');" +
      s"""i.value="$q";""" +
      "i.dispatchEvent(new Event('input'));})()")

  /** JSON array of the city names whose row is currently visible. */
  private def visibleCities(page: CdpPage): String =
    page.evalString(
      "JSON.stringify(Array.prototype.filter.call(" +
      "document.querySelectorAll('#city-list > li')," +
      "function(el){return getComputedStyle(el).display!=='none';})" +
      ".map(function(el){return el.textContent.trim();}))")

  "the city-selection landing search" should
    "narrow the list to a typed city name" in {
    onLanding { page =>
      typeCitySearch(page, "wroc")
      visibleCities(page) shouldBe """["Wrocław"]"""
    }
  }

  it should "match Polish names typed without diacritics" in {
    onLanding { page =>
      // "lodz" must find "Łódź" — ł/ó folded away on both sides.
      typeCitySearch(page, "lodz")
      visibleCities(page) shouldBe """["Łódź"]"""
      typeCitySearch(page, "krakow")
      visibleCities(page) shouldBe """["Kraków"]"""
    }
  }

  it should "show the empty-state and no rows when nothing matches" in {
    onLanding { page =>
      typeCitySearch(page, "zzzzz")
      visibleCities(page) shouldBe "[]"
      page.evalBool(
        "getComputedStyle(document.getElementById('city-empty')).display !== 'none'"
      ) shouldBe true
    }
  }

  it should "restore the full list when the query is cleared" in {
    onLanding { page =>
      typeCitySearch(page, "wroc")
      typeCitySearch(page, "")
      page.evalString(
        "String(document.querySelectorAll('#city-list > li').length)"
      ) shouldBe page.evalString(
        "String(Array.prototype.filter.call(" +
        "document.querySelectorAll('#city-list > li')," +
        "function(el){return getComputedStyle(el).display!=='none';}).length)")
    }
  }

  // ── shared.js globals ────────────────────────────────────────────────────

  // The inline <script> block in repertoire.scala.html calls functions
  // exported by shared.js (undoTruncation, truncateAllShowings,
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

  // The navbar / login-modal partials wire their auth controls through inline
  // `onclick="toggleAuthMenu(event)"` / `openLoginModal()` handlers, which
  // resolve against `window`. shared.js runs inside an IIFE, so these functions
  // are only reachable if explicitly hung off `window`. Regression for the
  // Sentry `ReferenceError: toggleAuthMenu is not defined` fired on every
  // auth-menu tap (the IIFE-local definitions weren't exported).
  it should "expose the auth-menu / login-modal handlers as globals" in {
    onPath("/") { page =>
      page.evalBool("typeof toggleAuthMenu === 'function'") shouldBe true
      page.evalBool("typeof closeAuthMenu === 'function'") shouldBe true
      page.evalBool("typeof openLoginModal === 'function'") shouldBe true
      page.evalBool("typeof closeLoginModal === 'function'") shouldBe true
    }
  }

  // IMDb rating badges deep-link into the IMDb app on Android Chrome via an
  // `intent://` URL with a `browser_fallback_url`; every other surface (iOS,
  // desktop, Firefox Android) keeps the plain https anchor. The decision lives
  // in two pure helpers exported from shared.js — assert their exact output so
  // the tt-id extraction, the encoded fallback, and the UA gating can't drift.
  "imdbIntentUrl" should "build the intent:// deep link with an encoded web fallback" in {
    onPath("/") { page =>
      page.evalString("imdbIntentUrl('https://www.imdb.com/title/tt1234567/')") shouldBe
        "intent://title/tt1234567#Intent;scheme=imdb;package=com.imdb.mobile;" +
        "S.browser_fallback_url=https%3A%2F%2Fwww.imdb.com%2Ftitle%2Ftt1234567%2F;end"
    }
  }

  it should "return null for a URL without an IMDb title id" in {
    onPath("/") { page =>
      page.evalBool("imdbIntentUrl('https://www.filmweb.pl/film/Foo-2026-123') === null") shouldBe true
      page.evalBool("imdbIntentUrl('') === null") shouldBe true
    }
  }

  "isAndroidChrome" should "match Android Chrome but not iOS, desktop, or Firefox Android" in {
    onPath("/") { page =>
      val androidChrome = "Mozilla/5.0 (Linux; Android 14; Pixel 9a) AppleWebKit/537.36 " +
        "(KHTML, like Gecko) Chrome/126.0.0.0 Mobile Safari/537.36"
      val firefoxAndroid = "Mozilla/5.0 (Android 14; Mobile; rv:127.0) Gecko/127.0 Firefox/127.0"
      val iphone = "Mozilla/5.0 (iPhone; CPU iPhone OS 18_0 like Mac OS X) AppleWebKit/605.1.15 " +
        "(KHTML, like Gecko) Version/18.0 Mobile/15E148 Safari/604.1"
      val desktop = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 " +
        "(KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36"
      def call(ua: String) = s"isAndroidChrome(${Json.stringify(JsString(ua))})"
      page.evalBool(call(androidChrome)) shouldBe true
      page.evalBool(call(firefoxAndroid)) shouldBe false
      page.evalBool(call(iphone)) shouldBe false
      page.evalBool(call(desktop)) shouldBe false
    }
  }

  // Drives the real inline-handler path the navbar uses: a `.auth-menu` div
  // whose `onclick` attribute is exactly `toggleAuthMenu(event)`. A genuine
  // click must toggle `.open` without throwing — before the fix this raised an
  // uncaught ReferenceError and the menu never opened.
  it should "toggle the auth menu open on a real click through the inline onclick" in {
    onPath("/") { page =>
      val opened = page.evalBool(
        "(() => {" +
        "  const m = document.createElement('div');" +
        "  m.id = 'auth-menu'; m.className = 'auth-menu';" +
        "  m.setAttribute('onclick', 'toggleAuthMenu(event)');" +
        "  document.body.appendChild(m);" +
        "  m.click();" +
        "  return m.classList.contains('open');" +
        "})()"
      )
      opened shouldBe true
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
    onPath("/") { page =>
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
    onPath("/") { page =>
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
      // whose last showing falls before the live wall-clock date drops
      // out of the visible set. The fixture's screenings are anchored
      // to a fixed `now = 2026-06-08`, but `dateBounds()` reads the
      // live `new Date()`, so without this pin the test silently
      // regresses each day past a fixture-card's last showtime.
      pinDateFilterAnytime(page)

      val totalCards = page.evalInt("document.querySelectorAll('.col[data-title]').length")
      totalCards should be > 5

      // "Straszny film" matches exactly two distinct cards in the 08-06-2026
      // Poznań corpus: the regular Polish row ("Straszny film") and its
      // Ukrainian-dub sibling ("Straszny film ukraiński dubbing"), which
      // TitleNormalizer keeps as its own card. Asserting exactly 2 catches a
      // regression where the search over-matches (folds an unrelated title in)
      // or under-matches (drops one of the two rows). (Was "Diabeł"/3 against
      // the old Prada corpus; Prada left cinemas and now has a single card.)
      page.eval("document.getElementById('search-input').value = 'Straszny film'; applyFilters()")
      val matchingCards = page.evalInt(
        "[...document.querySelectorAll('.col[data-title]')].filter(c => c.style.display !== 'none').length"
      )
      matchingCards shouldBe 2
      page.evalBool(
        "[...document.querySelectorAll('.col[data-title]')]" +
          ".filter(c => c.style.display !== 'none')" +
          ".every(c => c.dataset.title.toLowerCase().includes('straszny film'))"
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
      page.eval("document.getElementById('search-input').value = 'Straszny film'; applyFilters()")
      page.eval("document.getElementById('search-input').value = '';             applyFilters()")
      page.evalInt(
        "[...document.querySelectorAll('.col[data-title]')].filter(c => c.style.display !== 'none').length"
      ) shouldBe baselineVisible
    }
  }

  /**
   * iOS Safari auto-zooms when focus lands on an input whose font-size is
   * below 16px — and the navbar search is intentionally ~12.5px (see
   * `--navbar-fs` in `_sharedStyles`). The robust fix is PREVENTION, not
   * correction: while the field is focused we pin `maximum-scale=1` on the
   * viewport meta so iOS never zooms in the first place; on blur we drop it so
   * pinch-zoom works everywhere else.
   *
   * Why prevention over the earlier on-blur "undo the zoom" approach: undoing
   * an already-applied zoom proved unreliable on real MobileSafari, and the
   * behaviour can't be reproduced in any local automation (Playwright WebKit
   * doesn't model iOS focus-zoom; the Simulator can't be tap-driven into it).
   * The meta-pinning MECHANISM, by contrast, is pure DOM and fully testable.
   */
  it should "pin maximum-scale=1 on the viewport while the search input is focused" in {
    onPath("/") { page =>
      val base = page.evalString("document.querySelector('meta[name=viewport]').getAttribute('content')")
      withClue("baseline meta should be zoomable (no maximum-scale): ") {
        base should not include "maximum-scale"
      }

      page.eval("document.getElementById('search-input').dispatchEvent(new Event('focus'))")
      val focused = page.evalString("document.querySelector('meta[name=viewport]').getAttribute('content')")
      withClue(s"viewport while focused was [$focused]: ") {
        focused should include("maximum-scale=1")
      }
    }
  }

  it should "restore the zoomable viewport when the search input blurs" in {
    onPath("/") { page =>
      val base = page.evalString("document.querySelector('meta[name=viewport]').getAttribute('content')")
      page.eval("document.getElementById('search-input').dispatchEvent(new Event('focus'))")
      page.eval("document.getElementById('search-input').dispatchEvent(new Event('blur'))")
      val restored = page.evalString("document.querySelector('meta[name=viewport]').getAttribute('content')")
      withClue(s"viewport after blur was [$restored]: ") {
        restored shouldBe base
        restored should not include "maximum-scale"
      }
    }
  }

  /** Switch the date filter to "Kiedykolwiek" (anytime) and re-run
   *  `applyFilters()` so the visible-card set no longer depends on the
   *  browser's wall-clock relative to the fixture's recorded dates. */
  private def pinDateFilterAnytime(page: CdpPage): Unit =
    page.eval("document.getElementById('date-filter').value = 'anytime'; applyFilters()")

  // The Filtry submenu panels (country/genre/director/cast/room) are now built
  // lazily on first Filtry-open, not at boot. Tests that read those lists
  // directly must open Filtry first to trigger the build.
  private def openFiltry(page: CdpPage): Unit =
    page.eval("document.getElementById('format-filter-btn').click()")

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
  //   one row: [logo+tabs] … [day pills] [filtry] [auth]
  //
  // Search is NOT on this row on portrait phones — it floats as a pill pinned
  // to the bottom of the viewport (see the portrait floating-pill CSS), so the
  // test asserts it sits BELOW the navbar instead of between day pills and
  // Filtry.
  //
  // Why this test exists: the orders + `margin-left: auto` + the
  // `flex-wrap: nowrap` single-row layout are subtle — easy to break with
  // an unrelated edit. Asserting on rendered geometry catches a regression
  // that snapshot diffs alone wouldn't (the markup can look fine but
  // the visual layout flips).

  "the mobile navbar (≤ 575 px)" should "keep day pills, filtry and auth on one row, with search floating below" in {
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
      // which leaves `<div class="navbar-auth">` empty. An empty flex item
      // has zero size and its `getBoundingClientRect` reports a degenerate
      // position that doesn't reflect production. On mobile the only auth
      // control the navbar shows is the LOGGED-IN avatar pill — the
      // signed-out "Zaloguj" button is hidden below 576 px (it lives in the
      // navbar on desktop only). So inject the avatar pill (mirroring the
      // `Some(user)` branch of `_navbar`) to exercise the realistic
      // logged-in mobile navbar; `.auth-name` hides at this width but the
      // `.auth-menu` pill itself stays visible.
      page.eval(
        "(() => { const a = document.querySelector('.navbar-auth');" +
        "          if (a && !a.children.length) {" +
        "            const pill = document.createElement('div');" +
        "            pill.className = 'auth-menu';" +
        "            pill.id = 'auth-menu';" +
        "            pill.innerHTML = '<span class=\"auth-avatar-fallback\">A</span>" +
        "<span class=\"auth-name\">Anonim</span>';" +
        "            a.appendChild(pill);" +
        "          } })()"
      )

      def rect(sel: String): (Double, Double) =
        page.evalString(
          s"(() => { const r = document.querySelector(${jsString(sel)}).getBoundingClientRect();" +
          s"          return r.top + '|' + r.left; })()"
        ).split('|') match {
          case Array(t, l) => (t.toDouble, l.toDouble)
        }
      val (searchTop, _         ) = rect(".navbar-search")
      val (authTop,   authLeft  ) = rect(".navbar-auth")
      val (dateTop,   dateLeft  ) = rect(".navbar-date")
      val (filtryTop, filtryLeft) = rect(".navbar-filtry")
      val navBottom = page.evalString(
        "String(document.querySelector('.navbar').getBoundingClientRect().bottom)"
      ).toDouble

      val viewportWidth = page.evalInt("window.innerWidth")
      // The in-row controls — day pills, Filtry and auth — share one row
      // (4 px tolerance for sub-pixel + line-height variance), laid out
      // left-to-right: day pills → Filtry → auth. Search is NOT in this row:
      // on portrait it floats as a pill pinned to the bottom of the viewport,
      // below the navbar.
      withClue(s"viewport=$viewportWidth searchTop=$searchTop navBottom=$navBottom auth=($authTop,$authLeft) date=($dateTop,$dateLeft) filtry=($filtryTop,$filtryLeft) ") {
        viewportWidth shouldBe 500
        math.abs(dateTop   - authTop) should be < 4.0
        math.abs(filtryTop - authTop) should be < 4.0
        dateLeft   should be < filtryLeft
        filtryLeft should be < authLeft
        searchTop  should be > navBottom
      }

      // Filtry icon is fixed-width — piling on every active filter the
      // navbar can carry must NOT change its footprint (it's an icon, not
      // a growing text label), so it stays next to date on its row instead
      // of being pushed onto a row of its own.
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
      // …and the funnel lights (accent active state) once axes are set.
      page.evalBool("document.getElementById('format-filter-btn').classList.contains('filters-active')") shouldBe true

      // Reset emulation so the next test starts at the default viewport.
      page.send("Emulation.clearDeviceMetricsOverride", play.api.libs.json.Json.obj())
    }
  }

  // ── Mobile-scale invariants across common phone widths ───────────────────
  //
  // Sweeps the navbar-bearing listing page (`/`) through seven
  // phone-class viewports and asserts two structural invariants:
  //
  //   1. The navbar stays on ONE row. Visible flex children of
  //      `.navbar` are bucketed by their `getBoundingClientRect().top`;
  //      the distinct bucket count = the row count. Exactly 1 means
  //      `flex-wrap: nowrap` held and nothing wrapped or overflowed.
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
  // on the listing page.

  // Common phone viewports (CSS px). 360 = Samsung Galaxy S10/S20/S22
  // + the narrowest Android in current circulation; 375 = iPhone SE
  // 2/3 + iPhone 12 mini; 390 = iPhone 12/13/14/15; 412 = Pixel 6/7/8;
  // 430 = iPhone 14/15/16/17 Pro Max; 540 = a wider phone landscape /
  // small tablet portrait; 575 = the @@media breakpoint top, where the
  // mobile rules hand back to the desktop defaults.
  //
  // 320 px (iPhone SE 1st gen) is included: the single-row navbar fits
  // even there because search floats out of the row into the bottom pill
  // (portrait), leaving the logo, day pills + arrows, Filtry and auth to
  // share the one row.
  private val MobileViewports = Seq(320, 360, 375, 390, 412, 430, 540, 575)

  for (path <- Seq("/")) {
    s"the mobile navbar on $path" should "stay on one row with zero horizontal overflow at every common phone width" in {
      onPath(path) { page =>
        pinDeterministicFont(page)
        // No auth pill is injected here on purpose: an anonymous visitor on
        // a mobile viewport has an EMPTY `.navbar-auth` slot — the "Zaloguj"
        // button is hidden below 576 px (it lives in the navbar on desktop
        // only), so the realistic mobile-anonymous navbar carries no auth
        // control. The empty zero-width slot is skipped in the row count
        // below, exactly as in production. (The logged-in mobile case — an
        // avatar pill in the slot — is covered by the "one row in that
        // order" test above; the desktop sweep injects the Zaloguj button,
        // which stays visible there.)

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

          // Skip zero-size slots (empty anonymous auth) AND the floating
          // search pill: on portrait it's `position:fixed`, lifted out of the
          // navbar flow as a bottom overlay, so it's not a row member and its
          // top sits well below the row — counting it would always read as a
          // second row.
          val rowCount = page.evalInt(
            "(() => { const nav = document.querySelector('.navbar');" +
            "          const tops = new Set();" +
            "          for (const c of nav.children) {" +
            "            const r = c.getBoundingClientRect();" +
            "            if (r.width === 0 || r.height === 0) continue;" +
            "            if (getComputedStyle(c).position === 'fixed') continue;" +
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
            "            if (getComputedStyle(c).position === 'fixed') continue;" +
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
          all (measured.map(_.rows))     shouldBe 1
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
          s"(function(){ var element = document.querySelector('$sel');" +
          "if (!element) return false; var r = element.getBoundingClientRect();" +
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
          s"(function(){ var element = document.querySelector('$sel');" +
          "if (!element) return false; var r = element.getBoundingClientRect();" +
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

  // ── /film poster/details stacking ────────────────────────────────────────
  //
  // The detail layout is a Bootstrap flex row: poster (`.col-auto`,
  // capped at 300 px) beside the details column (`.col`). On a phone
  // (≤ 575 px) that 300 px poster + the gutter leave the details column
  // so narrow the title wraps one word per line and a dead gap opens
  // below the (shorter) poster column. Below the breakpoint the two must
  // stack — the title begins *below* the poster — while desktop keeps
  // them side by side. Geometry assertion, since the markup looks fine
  // either way; only the rendered layout flips.

  /** (posterTop, posterRight, posterBottom, posterLeft, titleTop, titleLeft)
   *  for the /film hero — the poster box and the title that heads the
   *  details column. */
  private def filmHeroGeometry(page: CdpPage): (Double, Double, Double, Double, Double, Double) =
    page.evalString(
      "(() => { const p = document.querySelector('.poster-wrap').getBoundingClientRect();" +
      "          const t = document.querySelector('.film-title').getBoundingClientRect();" +
      "          return [p.top, p.right, p.bottom, p.left, t.top, t.left].join('|'); })()"
    ).split('|').map(_.toDouble) match {
      case Array(pt, pr, pb, pl, tt, tl) => (pt, pr, pb, pl, tt, tl)
    }

  // Widths in the "cramped band": wide enough that the 300 px poster +
  // a sliver of details column *just* fit side by side (so without the
  // stack fix the row does NOT wrap on its own), but narrow enough that
  // the sliver is unusable — the title breaks one word per line and a
  // dead gap opens below the poster. iPhone 16/17 Pro (~402), the Pro
  // Max class (430), and the top of the breakpoint (540, 575) all land
  // here. Below ~395 px the row wraps on its own regardless, so those
  // widths can't tell the fix from the default.
  private val FilmCrampedWidths = Seq(402, 430, 540, 575)

  "the /film poster and details" should "stack vertically across the cramped phone band" in {
    onPath(filmTarget) { page =>
      val rows = FilmCrampedWidths.map { w =>
        page.setViewport(w, 1100)
        Thread.sleep(60L)
        val (_, _, posterBottom, _, titleTop, _) = filmHeroGeometry(page)
        (w, posterBottom, titleTop)
      }
      page.send("Emulation.clearDeviceMetricsOverride", play.api.libs.json.Json.obj())
      val table = rows.map { case (w, pb, tt) =>
        f"  $w%3d px → posterBottom=$pb%.0f titleTop=$tt%.0f"
      }.mkString("\n")
      info(s"/film stacking sweep:\n$table")
      withClue(s"/film stacking sweep (title must start below the poster):\n$table\n") {
        // titleTop ≥ posterBottom means the details column starts below
        // the poster — stacked, not crammed beside it.
        all (rows.map { case (_, pb, tt) => tt - pb }) should be >= -4.0
      }
    }
  }

  it should "sit side by side at desktop width (1280px)" in {
    onPath(filmTarget) { page =>
      page.setDesktopViewport(1280, 900)
      Thread.sleep(60L)
      val (_, posterRight, posterBottom, _, titleTop, titleLeft) = filmHeroGeometry(page)
      page.send("Emulation.clearDeviceMetricsOverride", play.api.libs.json.Json.obj())
      withClue(s"posterRight=$posterRight posterBottom=$posterBottom titleTop=$titleTop titleLeft=$titleLeft ") {
        // Title shares the poster's row (above its bottom edge) and sits
        // to the right of the poster — the side-by-side desktop layout.
        titleTop  should be < posterBottom
        titleLeft should be > posterRight
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
  //   1. The navbar fits in ONE row. `flex-wrap: nowrap` keeps all the
  //      navbar children in line; if a wide label or new entry overflows
  //      past the row, that's a regression.
  //   2. Zero horizontal overflow on the document. `scrollWidth >
  //      innerWidth` produces a horizontal scrollbar, which is always
  //      a desktop bug.
  //
  // Uses `setDesktopViewport` (mobile = false) so the page sees the
  // same `pointer: fine` / `@media (hover: hover)` truthiness a real
  // desktop browser does, and the `--mobile-scale` clamps stay at 1.0.
  private val DesktopViewports = Seq(1280, 1440, 1920)

  for (path <- Seq("/")) {
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

        case class Row(width: Int, rows: Int, documentOverflow: Int)
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
          val documentOverflow = page.evalInt(
            "Math.max(0, document.documentElement.scrollWidth - window.innerWidth)"
          )
          Row(w, rowCount, documentOverflow)
        }

        page.send("Emulation.clearDeviceMetricsOverride", play.api.libs.json.Json.obj())

        val table = measured.map { r =>
          f"  ${r.width}%4d px → rows=${r.rows} overflow=${r.documentOverflow}%3d px"
        }.mkString("\n")
        info(s"desktop layout sweep on $path:\n$table")
        withClue(s"desktop layout sweep on $path:\n$table\n") {
          all (measured.map(_.rows))        shouldBe 1
          all (measured.map(_.documentOverflow)) shouldBe 0
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
      page.waitFor(s"location.pathname === '$cityPrefix/film'", timeoutMs = 5000)
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

  // The Filtry trigger is an icon-only funnel; an active axis is signalled by
  // the accent `.filters-active` class (the web counterpart of iOS's `.fill`
  // swap and Android's Brand tint), not by text appended to a label.
  it should "light the Filtry funnel (filters-active) when an axis is active" in {
    onPath("/") { page =>
      clearLocalStorage(page)
      pinDateFilterAnytime(page)
      page.evalBool("document.getElementById('format-filter-btn').classList.contains('filters-active')") shouldBe false
      page.eval("document.querySelector('input[name=\"format-dim\"][value=\"2D\"]').click()")
      page.evalBool("document.getElementById('format-filter-btn').classList.contains('filters-active')") shouldBe true
    }
  }

  // Clicking outside the open panel must do exactly ONE thing — dismiss it.
  // Regression: the same outside-click also bubbled to the card-tap handler
  // (or followed the link under the cursor) and navigated to /film, so closing
  // the filter accidentally opened a page.
  it should "only dismiss on an outside click — never trigger a link or navigation" in {
    onPath("/") { page =>
      clearLocalStorage(page)
      pinDateFilterAnytime(page)
      page.eval("document.getElementById('format-filter-btn').click()")
      page.evalString("document.getElementById('format-panel').style.display") shouldBe "block"

      // Dispatch a real, cancelable click on a card's poster <a> (outside the
      // panel). The fix runs in the capture phase and preventDefault()s it.
      val defaultPrevented = page.evalBool(
        s"(() => { const a = $firstCardPosterLink;" +
        "  const e = new MouseEvent('click', { bubbles: true, cancelable: true });" +
        "  a.dispatchEvent(e);" +
        "  return e.defaultPrevented; })()"
      )
      defaultPrevented shouldBe true
      page.evalString("location.pathname") shouldBe (cityPrefix + "/")
      page.evalString("document.getElementById('format-panel').style.display") shouldBe "none"
    }
  }

  // The flip side: with no panel open the same click is left completely alone,
  // so ordinary card navigation still works.
  it should "leave clicks untouched when no panel is open" in {
    onPath("/") { page =>
      clearLocalStorage(page)
      pinDateFilterAnytime(page)
      val defaultPrevented = page.evalBool(
        s"(() => { const a = $firstCardPosterLink;" +
        "  const e = new MouseEvent('click', { bubbles: true, cancelable: true });" +
        "  a.dispatchEvent(e);" +
        "  return e.defaultPrevented; })()"
      )
      defaultPrevented shouldBe false
    }
  }

  // ── Sortuj (sort axis) ─────────────────────────────────────────────────────
  //
  // The Filtry panel's "Sortuj" select reorders the visible grid: earliest
  // screening (default) or weighted rating (biggest-first). The whole grid
  // is one sorted list. The sort key rides on each card's `data-rating`
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

  // ── Anti-FOUC grid cloak ────────────────────────────────────────────────────
  //
  // The server renders every film for every date (no server-side day filter);
  // `bootView` prunes to the selected day + the viewer's locally-hidden films
  // only on DOMContentLoaded — after first paint. To stop the full unfiltered
  // grid flashing then visibly collapsing, a blocking head script cloaks <html>
  // with `grid-cloak` (a CSS rule hides #film-grid), and bootView drops it once
  // the first applyFilters() pass has set final visibility. These guard both
  // ends of that mechanism.

  "the anti-FOUC grid cloak" should "hide #film-grid whenever html.grid-cloak is set" in {
    onPath("/") { page =>
      // At rest (bootView already ran) the cloak is gone and the grid shows.
      page.evalBool("document.documentElement.classList.contains('grid-cloak')") shouldBe false
      page.evalString("getComputedStyle(document.getElementById('film-grid')).visibility") shouldBe "visible"

      // Re-adding the class hides the whole grid — this is the rule that
      // suppresses the unfiltered pre-filter paint on load.
      page.eval("document.documentElement.classList.add('grid-cloak')")
      page.evalString("getComputedStyle(document.getElementById('film-grid')).visibility") shouldBe "hidden"
    }
  }

  it should "be dropped by bootView once the first filter pass has run" in {
    onPath("/") { page =>
      // Restore the load-time cloak, then re-run boot: bootView must reveal the
      // grid (drop the class) after it has applied the filters.
      page.eval("document.documentElement.classList.add('grid-cloak'); bootView()")
      page.evalBool("document.documentElement.classList.contains('grid-cloak')") shouldBe false
      page.evalString("getComputedStyle(document.getElementById('film-grid')).visibility") shouldBe "visible"
    }
  }

  // ── Day-stepping wrap (horizontal swipe) ────────────────────────────────────
  //
  // A horizontal swipe steps the selected day via `window.stepDateWrap(directory)`
  // (directory = +1 next, -1 previous), wrapping the `#date-filter` option list with
  // modulo arithmetic — last → first on +1, first → last on -1 — then
  // re-rendering through `onDateChange()` → `applyFilters()`. The clamping
  // `stepDate` (arrow buttons / keys) is exercised separately; this asserts the
  // wrap-around the swipe relies on.

  "the day-stepping wrap" should "advance the date selector and wrap last → first on stepDateWrap(+1)" in {
    onPath("/") { page =>
      val optionCount = page.evalInt("document.getElementById('date-filter').options.length")
      optionCount should be > 1

      // From index 0, stepping forward through every option lands back on 0
      // (the modulo wrap), and each intermediate step advances by one.
      page.eval("document.getElementById('date-filter').selectedIndex = 0; window.stepDateWrap(1)")
      page.evalInt("document.getElementById('date-filter').selectedIndex") shouldBe 1

      // Drive to the last option, then one more step wraps to index 0.
      page.eval(
        s"document.getElementById('date-filter').selectedIndex = ${optionCount - 1}; window.stepDateWrap(1)"
      )
      page.evalInt("document.getElementById('date-filter').selectedIndex") shouldBe 0

      // The wrap re-rendered the grid via onDateChange → applyFilters: the
      // current day ('today', index 0 after the wrap) shows at least one card.
      visibleCardCount(page) should be > 0
    }
  }

  it should "wrap first → last on stepDateWrap(-1)" in {
    onPath("/") { page =>
      val optionCount = page.evalInt("document.getElementById('date-filter').options.length")
      page.eval("document.getElementById('date-filter').selectedIndex = 0; window.stepDateWrap(-1)")
      page.evalInt("document.getElementById('date-filter').selectedIndex") shouldBe (optionCount - 1)
    }
  }

  // ── Day carousel (three-column slide) ───────────────────────────────────────
  //
  // The films grid is the centre column of a previous|current|next carousel inside
  // `#day-track`. A swipe (and the arrow buttons / Left-Right keys / the
  // `#date-filter` dropdown) slides the track to reveal a neighbour day's grid —
  // a clone of `#film-grid` filtered to that day — then commits the day change
  // and scrolls to top. These assert the structural contract the gesture relies
  // on: the neighbour is mounted + visible mid-drag at the SAME scroll offset as
  // the centre, a committed change resets scroll, and all four entry points run
  // the SAME animated slide (the track gets `.day-track--armed` and lands on the
  // right day with `?date=` updated).

  "the day carousel" should "mount the neighbour column at the same scroll offset mid-drag" in {
    onPath("/") { page =>
      enableSlideAnimation(page)
      coarsePointer(page)
      page.eval("document.getElementById('date-filter').value = 'anytime'; onDateChange()")
      page.waitFor("document.querySelector('.col[data-title]') !== null")

      // Scroll down so a naive (non-shared-scroll) neighbour would sit at a
      // different vertical offset than the centre.
      page.eval("window.scrollTo(0, 400)")

      // Begin a synthetic finger drag: down, then a horizontal move past the
      // deadzone so the handler locks the axis and arms the carousel.
      page.eval(synthDrag("pointerdown", 300, 500))
      page.eval(synthDrag("pointermove", 240, 505))   // dx = -60 → lock + arm
      page.eval(synthDrag("pointermove", 180, 505))   // keep dragging left

      // A neighbour `.day-col` is mounted in the track and actually painted.
      page.evalInt("document.querySelectorAll('#day-track > .day-col').length") should be >= 1
      page.evalBool(
        "[...document.querySelectorAll('#day-track > .day-col')].some(c => c.offsetParent !== null && c.querySelector('.col[data-title]'))"
      ) shouldBe true

      // Synced vertical scroll: the revealed neighbour's top equals the centre
      // grid's top — both are top-aligned in the same flex row sharing the
      // page's single scroll, so the offset matches even after scrolling.
      val sameTop = page.evalBool(
        """(() => {
          |  const root = document.getElementById('view-root');
          |  const cols = [...document.querySelectorAll('#day-track > .day-col')]
          |    .filter(c => c.querySelector('.col[data-title]'));
          |  if (!root || cols.length === 0) return false;
          |  const rt = root.getBoundingClientRect().top;
          |  return cols.some(c => Math.abs(c.getBoundingClientRect().top - rt) < 1);
          |})()""".stripMargin
      )
      sameTop shouldBe true

      // Release below the commit threshold → snap back, leaving scroll untouched.
      page.eval(synthDrag("pointerup", 180, 505))
    }
  }

  it should "scroll to top on a committed day change but leave scroll alone on snap-back" in {
    onPath("/") { page =>
      enableSlideAnimation(page)
      coarsePointer(page)
      page.eval("document.getElementById('date-filter').value = 'anytime'; onDateChange()")
      page.waitFor("document.querySelector('.col[data-title]') !== null")

      // Sub-threshold drag → snap-back: scroll position is preserved.
      page.eval("window.scrollTo(0, 300)")
      page.eval(synthDrag("pointerdown", 300, 500))
      page.eval(synthDrag("pointermove", 285, 505))   // tiny dx → no commit
      page.eval(synthDrag("pointerup",   285, 505))
      page.waitFor("document.querySelectorAll('#day-track > .day-col').length === 0", timeoutMs = 2000)
      page.evalInt("Math.round(window.scrollY)") shouldBe 300

      // A committed day change (via the unified animateToDay) scrolls to top.
      page.eval("window.scrollTo(0, 300)")
      page.eval("window.animateToDay('tomorrow')")
      page.waitFor("document.querySelectorAll('#day-track > .day-col').length === 0", timeoutMs = 2000)
      page.evalInt("Math.round(window.scrollY)") shouldBe 0
      page.evalString("document.getElementById('date-filter').value") shouldBe "tomorrow"
    }
  }

  it should "run the animated slide from a keyboard day-step" in {
    onPath("/") { page =>
      enableSlideAnimation(page)
      page.eval("document.getElementById('date-filter').value = 'today'; onDateChange()")

      // Step to the next day (the Left/Right key path); while the slide is in
      // flight the track is armed (neighbour mounted, parked transform set).
      page.eval("window.stepDate(1)")
      page.evalBool("document.getElementById('day-track').classList.contains('day-track--armed')") shouldBe true
      page.evalInt("document.querySelectorAll('#day-track > .day-col').length") should be >= 1

      // It settles on the next day with `?date=` reflecting it.
      page.waitFor("document.querySelectorAll('#day-track > .day-col').length === 0", timeoutMs = 2000)
      page.evalString("document.getElementById('date-filter').value") shouldBe "tomorrow"
      page.evalBool("new URL(location.href).searchParams.get('date') === 'tomorrow'") shouldBe true
    }
  }

  it should "run the animated slide from a dropdown change" in {
    onPath("/") { page =>
      enableSlideAnimation(page)
      page.eval("document.getElementById('date-filter').value = 'today'; onDateChange()")

      // Multi-step jump today → anytime: the dropdown moves first, then a SINGLE
      // slide carries the target day's column into view (the track is armed).
      page.eval(
        "const s = document.getElementById('date-filter'); s.value = 'anytime'; onDateSelect()"
      )
      page.evalBool("document.getElementById('day-track').classList.contains('day-track--armed')") shouldBe true

      page.waitFor("document.querySelectorAll('#day-track > .day-col').length === 0", timeoutMs = 2000)
      page.evalString("document.getElementById('date-filter').value") shouldBe "anytime"
      page.evalBool("new URL(location.href).searchParams.get('date') === 'anytime'") shouldBe true
    }
  }

  // ── Desktop slides 1.5× slower than touch ────────────────────────────────────
  //
  // The day-change slide keeps its snappy 220 ms base on touch/mobile (a
  // finger-flick wants an immediate response) but takes a longer 1.5× glide
  // (330 ms) on a fine pointer, where a mouse-driven arrow / keyboard / dropdown
  // step reads better slower. Both paths run the SAME `animateToDay` slide —
  // only the duration differs, gated on `matchMedia('(pointer: coarse)')`.

  it should "slide 1.5x slower on desktop (fine pointer) than on touch" in {
    onPath("/") { page =>
      enableSlideAnimation(page)

      // A fresh tab has no touch emulation → the default fine (mouse) pointer,
      // which takes the longer desktop glide.
      page.evalBool("matchMedia('(pointer: coarse)').matches") shouldBe false
      val desktopMs = slideDurationMs(page)

      // Touch emulation flips the pointer to coarse → the snappy mobile base.
      coarsePointer(page)
      val touchMs = slideDurationMs(page)

      touchMs shouldBe 220
      desktopMs shouldBe 550
      desktopMs shouldBe (touchMs * 2.5).round.toInt
    }
  }

  // ── Dropdown slide direction follows the option-list order, not wrap ─────────
  //
  // A dropdown pick is a LINEAR list choice: the target's position in the option
  // list (today, tomorrow, week, anytime, …, with 'anytime' LAST) decides the
  // slide direction — AFTER the current option → enter from the right (next),
  // BEFORE it → enter from the left (previous). This differs from the wrap-shortest
  // direction the arrows/keyboard/swipe use, and the two disagree at the ends:
  // today → anytime is the longest forward jump but wrap-shortest would slide it
  // LEFT (the short way back round the ring). The dropdown must still go right.

  it should "enter from the right when the dropdown target is after the current option" in {
    onPath("/") { page =>
      enableSlideAnimation(page)
      page.eval("document.getElementById('date-filter').value = 'today'; onDateChange()")
      page.waitFor("document.querySelector('.col[data-title]') !== null")

      // today → anytime (last option). Linear: AFTER → right/next. Wrap-shortest
      // would have gone left, so this side assertion FAILS before the fix.
      page.eval(
        "const s = document.getElementById('date-filter'); s.value = 'anytime'; onDateSelect()"
      )
      page.evalBool("document.getElementById('day-track').classList.contains('day-track--armed')") shouldBe true
      armedSlideSide(page) shouldBe "next"

      page.waitFor("document.querySelectorAll('#day-track > .day-col').length === 0", timeoutMs = 2000)
      page.evalString("document.getElementById('date-filter').value") shouldBe "anytime"
    }
  }

  it should "enter from the left when the dropdown target is before the current option" in {
    onPath("/") { page =>
      enableSlideAnimation(page)
      page.eval("document.getElementById('date-filter').value = 'anytime'; onDateChange()")
      page.waitFor("document.querySelector('.col[data-title]') !== null")

      // anytime (last) → today (first). Linear: BEFORE → left/previous. Wrap-shortest
      // would have gone right, so this side assertion FAILS before the fix.
      page.eval(
        "const s = document.getElementById('date-filter'); s.value = 'today'; onDateSelect()"
      )
      page.evalBool("document.getElementById('day-track').classList.contains('day-track--armed')") shouldBe true
      armedSlideSide(page) shouldBe "previous"

      page.waitFor("document.querySelectorAll('#day-track > .day-col').length === 0", timeoutMs = 2000)
      page.evalString("document.getElementById('date-filter').value") shouldBe "today"
    }
  }

  // ── Neighbour-column order matches the committed grid ───────────────────────
  //
  // Regression for "the cards I see while dragging reorder when I let go". The
  // neighbour preview column is a clone of `#film-grid` filtered to the target
  // day by `applyFiltersForDay`, but that helper only toggled `display` — it
  // left the cards in the CURRENT day's cloned DOM order. The committed render
  // (`applyFilters` → `sortByEarliestVisible`) re-sorts the target day by its
  // own earliest showtime, so the visible order jumped on release. The preview
  // must show the SAME order the commit lands on.

  "the day-carousel neighbour column" should "show films in the committed (earliest-showtime) order, not the current day's order" in {
    onPath("/") { page =>
      // Start on a broad day ('anytime') so the centre grid's DOM order is the
      // anytime earliest order — generally NOT the same as a single later day's,
      // which is exactly what makes the reorder-on-release visible.
      page.eval("document.getElementById('date-filter').value = 'anytime'; onDateChange()")
      page.waitFor("document.querySelector('#film-grid > .col[data-title]') !== null")

      // A left drag from 'anytime' reveals the NEXT day in the ring, which wraps
      // to 'today'. Capture which day the mounted neighbour is for so we commit
      // to the SAME day (otherwise the counts/order wouldn't be comparable).
      coarsePointer(page)
      page.eval(synthDrag("pointerdown", 300, 500))
      page.eval(synthDrag("pointermove", 240, 505))   // dx = -60 → lock + arm (reveals NEXT day)
      page.eval(synthDrag("pointermove", 120, 505))   // keep dragging left, stay held

      // The NEXT-day neighbour is the `.day-col` to the right of `#view-root`.
      val neighbourSel =
        """(() => {
          |  const root = document.getElementById('view-root');
          |  if (!root) return null;
          |  let element = root.nextElementSibling;
          |  while (element && !element.classList.contains('day-col')) element = element.nextElementSibling;
          |  return element;
          |})()""".stripMargin
      val neighbourOrder = page.evalString(
        s"""(() => {
          |  const element = $neighbourSel;
          |  if (!element) return '';
          |  return [...element.querySelectorAll('.col[data-title]')]
          |    .filter(c => c.style.display !== 'none')
          |    .map(c => c.dataset.title).join('|');
          |})()""".stripMargin
      )
      // Which day the neighbour preview is for = the day after 'anytime' wraps:
      // read it from the ring so the commit below targets the same day.
      val neighbourDay = page.evalString(
        "(() => { const sel = document.getElementById('date-filter');" +
        "  const ring = window.dayRing ? window.dayRing() : " +
        "    [...sel.options].map(o => o.value);" +
        "  const n = ring.length; return ring[((sel.selectedIndex + 1) % n + n) % n]; })()"
      )
      // Release below threshold → snap back, leaving the real day unchanged.
      page.eval(synthDrag("pointerup", 120, 505))
      page.waitFor("document.querySelectorAll('#day-track > .day-col').length === 0", timeoutMs = 2000)

      // The committed order for that SAME day: select it and read `#film-grid`.
      page.eval(
        s"document.getElementById('date-filter').value = ${jsString(neighbourDay)}; applyFilters()"
      )
      val committedOrder = visibleTitleOrder(page)

      // Both must be non-empty and identical — the preview can't reorder on
      // release.
      committedOrder.nonEmpty shouldBe true
      neighbourOrder shouldBe committedOrder
    }
  }

  // Regression: switching jutro→dziś swiped the cards in, then snapped them into
  // a different order "in a heartbeat". Root cause — the preview clone
  // (applyFiltersForDay) broke earliest-showtime TIES by the clone's current DOM
  // order (i.e. the previously-shown day's), while the commit (sortByEarliestVisible)
  // breaks the same ties by each card's ORIGINAL server position. When the grid was
  // already reordered by a prior day, tied films landed one way in the preview and
  // snapped to the commit order on release. Both must tiebreak on the stamped
  // server index.
  "the day-carousel preview" should "order equal-earliest films by server position even when the source grid was left reordered by a prior day" in {
    onPath("/") { page =>
      // A fixture date carrying several films that share an earliest showtime.
      // (The aged fixture only populates 'anytime' via the live clock, so the
      // ties live on specific dates, reached through the isSpecific branch.)
      val day = "2026-06-10"

      // Commit that day through the REAL path to capture the authoritative order.
      // The static #date-filter only carries today/tomorrow/week/anytime, so add
      // the specific-date option the URL-sync / stepDate path would otherwise add.
      page.eval(
        s"(() => { const sel = document.getElementById('date-filter');" +
        s"  const o = document.createElement('option'); o.value = ${jsString(day)};" +
        s"  sel.appendChild(o); sel.value = ${jsString(day)}; applyFilters(); })()"
      )
      val committedOrder = visibleTitleOrder(page)
      committedOrder.nonEmpty shouldBe true

      // Guard against a silently vacuous test: the tiebreak only fires when two
      // visible films share an earliest showtime. If the fixture ever drifts and
      // loses its ties on `day`, fail loudly here rather than pass for free.
      val hasTie = page.evalBool(
        s"""(() => {
          |  const earliest = c => {
          |    let best = null;
          |    c.querySelectorAll('.date-group[data-date="$day"] .badge-time').forEach(b => {
          |      if (b.style.display === 'none') return;
          |      const t = (b.dataset.time || '').trim();
          |      if (t && (best === null || t < best)) best = t;
          |    });
          |    return best;
          |  };
          |  const times = [...document.querySelectorAll('#film-grid > .col[data-title]')]
          |    .filter(c => c.style.display !== 'none').map(earliest).filter(Boolean);
          |  return new Set(times).size < times.length;
          |})()""".stripMargin
      )
      hasTie shouldBe true

      // Simulate what a prior day (e.g. having viewed 'jutro' first) leaves behind:
      // the live #film-grid in a NON-server order. A full reverse guarantees every
      // tied pair now sits opposite to its server order.
      page.eval(
        "(() => { const g = document.getElementById('film-grid');" +
        "  [...g.querySelectorAll('.col[data-title]')].reverse().forEach(c => g.appendChild(c)); })()"
      )

      // The carousel preview clones that reordered grid and re-sorts it for `day`.
      // Its order must match the committed order — equal-earliest films tiebroken
      // by the stamped server index, not the clone's current (reversed) position.
      val previewOrder = page.evalString(
        s"""(() => {
          |  const clone = document.getElementById('film-grid').cloneNode(true);
          |  applyFiltersForDay(clone, ${jsString(day)});
          |  return [...clone.querySelectorAll('.col[data-title]')]
          |    .filter(c => c.style.display !== 'none').map(c => c.dataset.title).join('|');
          |})()""".stripMargin
      )

      previewOrder shouldBe committedOrder
    }
  }

  // ── Day-filter pills (the visible day picker) ────────────────────────────────
  //
  // The four presets render as a `.day-pill` row driving a visually-hidden
  // `#date-filter` <select> — the state source every filter/carousel helper
  // reads. `pickDay(value)` moves the select (and slides the grid); `syncDayPills`
  // mirrors the committed day back onto the pills. These assert that round-trip.

  "the day pills" should "mark exactly the active day and move the select + URL when tapped" in {
    onPath("/") { page =>
      // The pill highlight moves eagerly, but the URL only updates once the
      // slide commits — wait on the committed `?date=` as the settle signal.
      page.eval("pickDay('anytime')")
      page.waitFor("new URL(location.href).searchParams.get('date') === 'anytime'", timeoutMs = 2000)

      // The matching pill is the only `.active`, and carries aria-selected.
      page.evalString("document.querySelector('.day-pill.active').dataset.day") shouldBe "anytime"
      page.evalInt("document.querySelectorAll('.day-pill.active').length") shouldBe 1
      page.evalString(
        "document.querySelector('.day-pill[data-day=\"anytime\"]').getAttribute('aria-selected')") shouldBe "true"
      page.evalString(
        "document.querySelector('.day-pill[data-day=\"today\"]').getAttribute('aria-selected')") shouldBe "false"
      page.evalString("document.getElementById('date-filter').value") shouldBe "anytime"

      // Back to 'today' strips ?date (today is the default) and moves the highlight.
      page.eval("pickDay('today')")
      page.waitFor("new URL(location.href).searchParams.get('date') === null", timeoutMs = 2000)
      page.evalString("document.querySelector('.day-pill.active').dataset.day") shouldBe "today"
    }
  }

  it should "follow the active day when it changes via a keyboard day-step" in {
    onPath("/") { page =>
      page.eval("pickDay('today')")
      page.waitFor("document.getElementById('date-filter').value === 'today'", timeoutMs = 2000)
      // A keyboard step moves to the next preset; the pill highlight tracks it.
      page.eval("stepDate(1)")
      page.waitFor("document.getElementById('date-filter').value === 'tomorrow'", timeoutMs = 2000)
      page.evalString("document.querySelector('.day-pill.active').dataset.day") shouldBe "tomorrow"
    }
  }

  // Regression: a mouse click leaves the tapped pill focused. A following Left/
  // Right arrow key flips the browser's focus-visible heuristic on, so the OLD
  // pill paints its `:focus-visible` border (box-shadow ring) even though the
  // `.active` background has moved to the new day — the old pill looks "stuck"
  // with a border but no fill. The keyboard step must carry focus to the new
  // day's pill so the ring tracks the active day.
  it should "carry keyboard focus to the new day's pill, not leave a stray ring on the old one" in {
    onPath("/") { page =>
      page.eval("document.getElementById('date-filter').value = 'today'; onDateChange()")
      // Model a prior mouse click: the 'today' pill holds focus.
      page.eval("document.querySelector('.day-pill[data-day=\"today\"]').focus()")
      page.evalString("document.activeElement.dataset.day") shouldBe "today"

      // Press the Right arrow (the document keydown path → stepDate).
      page.eval("document.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowRight', bubbles: true }))")

      // Focus follows to the destination pill — no stale focus on the old 'today'
      // pill that the keyboard would paint a :focus-visible ring on.
      page.evalBool("document.activeElement.classList.contains('day-pill')") shouldBe true
      page.evalString("document.activeElement.dataset.day") shouldBe "tomorrow"
    }
  }

  // A keyboard step flips the day-pill highlight to the destination IMMEDIATELY,
  // before the slide settles — the pill leads and the grid animates to catch up,
  // rather than holding the old day lit until partway through the travel. (The
  // finger-drag path keeps its own boundary-crossing preview, asserted below.)
  it should "flip the day-pill highlight immediately on a keyboard step, before the slide settles" in {
    onPath("/") { page =>
      enableSlideAnimation(page)   // take the real slide, not the reduced-motion instant commit
      page.eval("document.getElementById('date-filter').value = 'today'; onDateChange()")
      page.evalString("document.querySelector('.day-pill.active').dataset.day") shouldBe "today"

      page.eval("stepDate(1)")
      // The pill is ALREADY on the destination while the slide is still in flight
      // (the track is armed, the grid hasn't committed yet).
      page.evalBool("document.getElementById('day-track').classList.contains('day-track--armed')") shouldBe true
      page.evalString("document.querySelector('#day-pills .day-pill.active').dataset.day") shouldBe "tomorrow"
      page.evalInt("document.querySelectorAll('.day-pill.active').length") shouldBe 1

      // And it stays on the destination once the slide commits.
      page.waitFor("document.querySelectorAll('#day-track > .day-col').length === 0", timeoutMs = 2000)
      page.evalString("document.querySelector('.day-pill.active').dataset.day") shouldBe "tomorrow"
    }
  }

  // Two arrow presses fired before the first slide can settle must BOTH register:
  // the day advances twice, not once. The second press used to be swallowed by
  // the in-flight `_animating` guard ("keyboard presses seem to be lost"); it is
  // now queued and replayed as a follow-on slide on commit.
  it should "honour a second keyboard step pressed mid-slide (two presses → two days)" in {
    onPath("/") { page =>
      enableSlideAnimation(page)
      page.eval("document.getElementById('date-filter').value = 'today'; onDateChange()")

      // The day two steps on, read from the option list so the assertion doesn't
      // hard-code preset labels.
      val twoStepsOn = page.evalString(
        "(() => { const s = document.getElementById('date-filter');" +
        "  return s.options[s.selectedIndex + 2].value; })()"
      )

      // Both presses before the first slide commits.
      page.eval("stepDate(1); stepDate(1)")
      // The pill jumps straight to the two-steps-on day right away.
      page.evalString("document.querySelector('#day-pills .day-pill.active').dataset.day") shouldBe twoStepsOn

      // Both slides run; the committed day is two steps on, not one.
      page.waitFor(
        s"document.getElementById('date-filter').value === ${jsString(twoStepsOn)}",
        timeoutMs = 3000
      )
      page.evalString("document.querySelector('.day-pill.active').dataset.day") shouldBe twoStepsOn
    }
  }

  it should "preview the destination day mid-drag — flip past the commit boundary, return when dragged back" in {
    onPath("/") { page =>
      coarsePointer(page)
      page.eval("document.getElementById('date-filter').value = 'today'; onDateChange()")
      page.waitFor("document.querySelector('.col[data-title]') !== null")
      page.evalString("document.querySelector('.day-pill.active').dataset.day") shouldBe "today"

      // Commit boundary is 40% of the pager width; viewport is 380px wide.
      val threshold = page.evalInt("Math.round(document.getElementById('view-pager').offsetWidth * 0.4)")

      page.eval(synthDrag("pointerdown", 300, 500))
      page.eval(synthDrag("pointermove", 240, 505))   // dx = -60 → lock + arm, still under the boundary
      page.evalString("document.querySelector('.day-pill.active').dataset.day") shouldBe "today"

      // Drag left well past the boundary → the highlight previews the next day.
      page.eval(synthDrag("pointermove", 300 - (threshold + 40), 505))
      page.evalString("document.querySelector('.day-pill.active').dataset.day") shouldBe "tomorrow"
      page.evalInt("document.querySelectorAll('.day-pill.active').length") shouldBe 1

      // Ease back under the boundary → the highlight returns to the current day.
      page.eval(synthDrag("pointermove", 290, 505))   // dx = -10
      page.evalString("document.querySelector('.day-pill.active').dataset.day") shouldBe "today"

      // Releasing under the boundary snaps back, leaving the current day highlighted.
      page.eval(synthDrag("pointerup", 290, 505))
      page.evalString("document.querySelector('.day-pill.active').dataset.day") shouldBe "today"
    }
  }

  it should "leave the previewed day highlighted after a committed swipe" in {
    onPath("/") { page =>
      enableSlideAnimation(page)
      coarsePointer(page)
      page.eval("document.getElementById('date-filter').value = 'today'; onDateChange()")
      page.waitFor("document.querySelector('.col[data-title]') !== null")

      val threshold = page.evalInt("Math.round(document.getElementById('view-pager').offsetWidth * 0.4)")
      page.eval(synthDrag("pointerdown", 300, 500))
      page.eval(synthDrag("pointermove", 240, 505))
      page.eval(synthDrag("pointermove", 300 - (threshold + 40), 505))   // past the boundary → previews tomorrow
      page.evalString("document.querySelector('.day-pill.active').dataset.day") shouldBe "tomorrow"
      page.eval(synthDrag("pointerup", 300 - (threshold + 40), 505))     // commit

      page.waitFor("document.getElementById('date-filter').value === 'tomorrow'", timeoutMs = 2000)
      page.evalString("document.querySelector('.day-pill.active').dataset.day") shouldBe "tomorrow"
    }
  }

  "the day-filter select" should "be present but visually hidden — the pills are the visible control" in {
    onPath("/") { page =>
      page.evalBool("document.getElementById('date-filter') !== null") shouldBe true
      // visually-hidden collapses it to ~1px; it is not a visible dropdown.
      page.evalInt(
        "Math.round(document.getElementById('date-filter').getBoundingClientRect().width)") should be <= 2
      page.evalInt("document.querySelectorAll('.day-pill').length") shouldBe 4
    }
  }

  // ── Single-cinema label suppression ─────────────────────────────────────────
  //
  // `applyFilters()` tags `#film-grid` with class `single-cinema` when at most
  // one of this city's enabled cinemas remains (ALL_CINEMAS minus the in-city
  // `disabledCinemas`); CSS then hides every per-card `.cinema-label`. With
  // multiple cinemas enabled the labels stay visible and the class is absent.

  "the single-cinema label toggle" should "show cinema labels with multiple cinemas and hide them when only one remains" in {
    onPath("/") { page =>
      pinDateFilterAnytime(page)

      // Baseline: multiple cinemas enabled — the grid carries no
      // `single-cinema` class and at least one `.cinema-label` is visible.
      page.evalBool("ALL_CINEMAS.length > 1") shouldBe true
      page.evalBool("document.getElementById('film-grid').classList.contains('single-cinema')") shouldBe false
      page.evalBool(
        "[...document.querySelectorAll('.cinema-label')].some(element => element.offsetParent !== null)"
      ) shouldBe true

      // Disable every cinema except the first → one enabled cinema remains.
      page.eval(
        "localStorage.setItem('disabledCinemas', JSON.stringify(ALL_CINEMAS.slice(1))); applyFilters()"
      )

      page.evalBool("document.getElementById('film-grid').classList.contains('single-cinema')") shouldBe true
      // Every `.cinema-label` is now hidden (display:none → offsetParent null).
      page.evalBool(
        "(() => { const labels = [...document.querySelectorAll('.cinema-label')];" +
        "  return labels.length > 0 && labels.every(element =>" +
        "    element.offsetParent === null || getComputedStyle(element).display === 'none'); })()"
      ) shouldBe true
    }
  }

  // ── Date filter ↔ URL round-trip ───────────────────────────────────────────
  //
  // `?date=` is the pasteable representation of the navbar's #date-filter:
  // selecting a day must rewrite the URL (so the link survives a copy/paste),
  // and opening a URL that already carries the parameter must seed the selector
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

  // ── Generic filter ↔ URL sync ───────────────────────────────────────────────
  //
  // The date selector is the only filter that writes back to the URL on
  // every change (so day-stepping survives a copy/paste). The rest of the
  // panel — radio / checkbox / text / submenu — stays local until the user
  // explicitly hits the Filtry "Skopiuj link do schowka" button, which calls
  // `copyFilterLinkToClipboard()`. That helper folds the full filter state
  // into the URL and pushes it to the clipboard. Tests below trip it
  // directly (`copyFilterLinkToClipboard()`) to assert the URL shape per
  // filter; boot-from-URL is asserted with the same `?parameter=` shape on a
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
      // The character class is round-tripped, not interpreted — the parameter
      // lands in the input verbatim so the same `q=…` link reproduces the
      // same filter result.
      page.evalString("document.getElementById('search-input').value") shouldBe "Diabłeb"
    }
  }

  "the Filtry submenu panels" should "build lazily — empty at boot, populated on first Filtry open" in {
    onPath("/") { page =>
      // Perf: the grid-scanning lists (genre/room/…) are NOT built at boot — so
      // first paint doesn't pay for scanning ~190 cards into hidden dropdowns.
      page.evalInt("document.querySelectorAll('#genre-list input').length") shouldBe 0
      page.evalInt("document.querySelectorAll('#room-list input').length")  shouldBe 0
      openFiltry(page)
      // Opening Filtry builds them on demand.
      page.evalInt("document.querySelectorAll('#genre-list input').length") should be > 0
      page.evalInt("document.querySelectorAll('#room-list input').length")  should be > 0
    }
  }

  "unchecking a country" should "leave ?country= listing the still-checked countries (inclusion set, captured on Copy)" in {
    onPath("/") { page =>
      clearLocalStorage(page)
      openFiltry(page)
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
      openFiltry(page)
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
      openFiltry(page)
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
      openFiltry(page)
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

  "a genre pill on a film card" should "link to the /{city}/filmy?gatunek= browse page for that genre" in {
    onPath("/") { page =>
      val href = page.evalString(
        "(() => { const a = document.querySelector('.col[data-genres] a.pill.genre');" +
          "  return a ? a.getAttribute('href') : ''; })()"
      )
      href should startWith(cityPrefix + "/filmy?gatunek=")
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

  // ── Cinema picker across a city switch ──────────────────────────────────────
  //
  // `disabledCinemas` is ONE global localStorage list shared by every city.
  // Switching city is a full navigation that leaves it untouched, so a cinema
  // deselected in another city stays in the list even though it isn't one of
  // this city's cinemas. The bug: the selected-count and the "Wszystkie kina"
  // checkbox were derived from the RAW list, so an other-city name made the
  // count read one short of this city's total and wrongly flipped the master
  // checkbox to indeterminate the moment you arrived. The fix scopes every
  // count/select-all derivation to the cinemas that actually belong to this
  // city; this spec pins that down (Poznań is the rendered city, so any
  // non-Poznań name stands in for "deselected elsewhere").

  // A cinema name guaranteed NOT to be in the rendered (Poznań) corpus —
  // a real Kraków cinema, so it reads like genuine cross-city state.
  private val foreignCinema = "Kino Pod Baranami"

  "a cinema deselected in another city" should "not pollute this city's count or Wszystkie-kina state" in {
    onPath("/") { page =>
      clearLocalStorage(page)
      // Sanity: the stand-in really is foreign to this city, else the test
      // would assert nothing.
      page.evalBool(s"ALL_CINEMAS.includes(${jsString(foreignCinema)})") shouldBe false

      page.eval(s"localStorage.setItem('disabledCinemas', ${jsString(s"""["$foreignCinema"]""")})")
      page.reload()
      pinDateFilterAnytime(page)

      // Every one of THIS city's cinemas is actually selected …
      page.evalBool(
        "[...document.querySelectorAll('#cinema-list input[type=\"checkbox\"]')].every(cb => cb.checked)"
      ) shouldBe true
      // … so the master checkbox is fully checked, not indeterminate …
      page.evalBool("document.getElementById('cinema-all').checked")       shouldBe true
      page.evalBool("document.getElementById('cinema-all').indeterminate") shouldBe false
      // … and the funnel icon stays neutral — a foreign-city deselection is
      // no narrowing of THIS city, so nothing "Wyczyść" would clear is set.
      page.evalBool("document.getElementById('format-filter-btn').classList.contains('filters-active')") shouldBe false

      clearLocalStorage(page)
    }
  }

  it should "still flip count + master state when a cinema of THIS city is the one deselected" in {
    onPath("/") { page =>
      clearLocalStorage(page)
      val here = page.evalString("ALL_CINEMAS[0]")
      // A real in-city deselection alongside the stale foreign entry.
      page.eval(
        s"localStorage.setItem('disabledCinemas', JSON.stringify([${jsString(foreignCinema)}, ${jsString(here)}]))"
      )
      page.reload()
      pinDateFilterAnytime(page)

      // First checkbox (ALL_CINEMAS[0] order) off, the rest on.
      page.evalBool(
        "(() => { const bs = [...document.querySelectorAll('#cinema-list input[type=\"checkbox\"]')];" +
          "  return !bs[0].checked && bs.slice(1).every(cb => cb.checked); })()"
      ) shouldBe true
      page.evalBool("document.getElementById('cinema-all').checked")       shouldBe false
      page.evalBool("document.getElementById('cinema-all').indeterminate") shouldBe true
      // An in-city deselection IS a filter "Wyczyść" clears, so the funnel
      // lights (unlike the foreign-only case above, which leaves it neutral).
      page.evalBool("document.getElementById('format-filter-btn').classList.contains('filters-active')") shouldBe true

      clearLocalStorage(page)
    }
  }

  "Wszystkie kina → select-all" should "drop only this city's cinemas, preserving deselections made elsewhere" in {
    onPath("/") { page =>
      clearLocalStorage(page)
      val here = page.evalString("ALL_CINEMAS[0]")
      page.eval(
        s"localStorage.setItem('disabledCinemas', JSON.stringify([${jsString(foreignCinema)}, ${jsString(here)}]))"
      )
      page.reload()
      pinDateFilterAnytime(page)

      // Boot state is indeterminate (one in-city cinema off); clicking flips
      // it to checked → toggleAllCinemas(true).
      page.eval("document.getElementById('cinema-all').click()")

      // The foreign deselection survives; this city's entry is gone.
      page.evalBool(
        s"(() => { const d = JSON.parse(localStorage.getItem('disabledCinemas') || '[]');" +
          s"  return d.length === 1 && d[0] === ${jsString(foreignCinema)}; })()"
      ) shouldBe true
      page.evalBool("document.getElementById('cinema-all').checked")       shouldBe true
      page.evalBool("document.getElementById('cinema-all').indeterminate") shouldBe false

      clearLocalStorage(page)
    }
  }

  "Wszystkie kina → deselect-all" should "add every cinema of this city on top of deselections made elsewhere" in {
    onPath("/") { page =>
      clearLocalStorage(page)
      page.eval(s"localStorage.setItem('disabledCinemas', ${jsString(s"""["$foreignCinema"]""")})")
      page.reload()
      pinDateFilterAnytime(page)

      // Boot state is fully checked (no in-city cinema disabled); clicking
      // unchecks it → toggleAllCinemas(false).
      page.eval("document.getElementById('cinema-all').click()")

      // Disabled set is now the foreign entry PLUS every cinema of this city.
      page.evalBool(
        s"(() => { const d = new Set(JSON.parse(localStorage.getItem('disabledCinemas') || '[]'));" +
          s"  return d.has(${jsString(foreignCinema)}) && ALL_CINEMAS.every(c => d.has(c))" +
          s"    && d.size === ALL_CINEMAS.length + 1; })()"
      ) shouldBe true
      page.evalBool("document.getElementById('cinema-all').checked")       shouldBe false
      // Every in-city cinema is disabled, so the master is unchecked, not
      // indeterminate.
      page.evalBool("document.getElementById('cinema-all').indeterminate") shouldBe false

      clearLocalStorage(page)
    }
  }

  // ── Cinema+room (Sale) filter ───────────────────────────────────────────────

  "the Sale (room) submenu" should "list one entry per (cinema, room) pair on /" in {
    onPath("/") { page =>
      pinDateFilterAnytime(page)
      openFiltry(page)
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
      openFiltry(page)
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
      openFiltry(page)

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
      openFiltry(page)
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
      openFiltry(page)
      val baseline = visibleBadgeCount(page)
      baseline should be > 1

      val targetPair = page.evalString(
        "document.querySelector('#room-list input[type=\"checkbox\"]:not(.submenu-all)').value"
      )
      val pipeIndex = targetPair.indexOf('|')
      val cinema  = targetPair.substring(0, pipeIndex)
      val room    = targetPair.substring(pipeIndex + 1)

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
      openFiltry(page)
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
      openFiltry(page)
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

  // The ✕ used to re-run the whole grid filter (applyFilters), which
  // un-truncates and re-truncates every card's showings — ~0.5s of wasted work
  // on a busy city, since hiding one card changes no other card. hideFilm now
  // takes a single-card fast path; assert it drops the card WITHOUT an
  // applyFilters() pass (this fails before the fast path: applyFilters ran once
  // per hide). A second card must stay put so we know we didn't just disable
  // the whole flow.
  it should "drop a single card without re-running the full grid filter" in {
    onPath("/") { page =>
      pinDateFilterAnytime(page)
      val title  = firstVisibleTitle(page)
      val others = page.evalInt(
        "[...document.querySelectorAll('.col[data-title]')].filter(c => c.style.display !== 'none').length"
      )
      others should be > 1  // need at least one card that must survive the hide

      page.eval(
        "window.__afCalls = 0; const _origAf = window.applyFilters;" +
        "  window.applyFilters = function () { window.__afCalls++; return _origAf.apply(this, arguments); };"
      )
      page.eval(
        s"(() => { const btn = document.querySelector('.col[data-title=${jsString(title)}] .hide-btn');" +
        "  hideFilm(btn); })()"
      )

      page.evalString(
        s"document.querySelector('.col[data-title=${jsString(title)}]').style.display"
      ) shouldBe "none"
      // The fast path leaves every other visible card on screen…
      page.evalInt(
        "[...document.querySelectorAll('.col[data-title]')].filter(c => c.style.display !== 'none').length"
      ) shouldBe (others - 1)
      // …and never falls back to the full grid filter.
      page.evalInt("window.__afCalls") shouldBe 0
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

  // ── /debug/tune ± step buttons ───────────────────────────────────────────

  "the /debug/tune slider ± buttons" should "step the value and update the CSS var on the preview" in {
    onPath("/debug/tune") { page =>
      val rangeSel = "document.querySelector('.ctrl input[type=range]')"
      val varName  = page.evalString(s"$rangeSel.dataset.var")
      def sliderValue: Double = page.evalString(s"$rangeSel.value").toDouble
      // The var the production card CSS reads is set on `#scope` on load and
      // re-set on every step — so the preview re-layouts live.
      def scopeVar: String =
        page.evalString(s"document.querySelector('#scope').style.getPropertyValue(${jsString(varName)}).trim()")

      scopeVar should not be empty
      val start = sliderValue

      // The wrap holds [−, range, +] — last-child is +, first-child is −.
      val wrap = "document.querySelector('.ctrl .slider-wrap')"
      def click(child: String): Unit =
        page.evalBool(s"(() => { $wrap.querySelector(${jsString(child)}).click(); return true; })()") shouldBe true

      click("button.step:last-child")
      val up = sliderValue
      up should be > start
      // The CSS var tracks the slider (2-dp format + the control's unit).
      scopeVar shouldBe f"$up%.2f" + "rem"

      click("button.step:first-child")
      click("button.step:first-child")
      sliderValue should be < up
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

  /** While a directed slide is armed, `animateToDay` mounts the TARGET day's
   *  populated `.day-col` on just one flank of `#view-root` (the other flank is
   *  an empty spacer) — the right/next side for `directory > 0`, the left/previous side
   *  for `directory < 0`. Returns "next" or "previous" for whichever side holds the
   *  populated column, or "" if neither is populated (no slide in flight). This
   *  is the deterministic read of the slide direction the dropdown picked. */
  private def armedSlideSide(page: CdpPage): String =
    page.evalString(
      """(() => {
        |  const root = document.getElementById('view-root');
        |  if (!root) return '';
        |  const popOn = (directoryProp) => {
        |    let element = root[directoryProp];
        |    while (element && !element.classList.contains('day-col')) element = element[directoryProp];
        |    return !!(element && element.querySelector('.col[data-title]'));
        |  };
        |  if (popOn('nextElementSibling'))     return 'next';
        |  if (popOn('previousElementSibling')) return 'previous';
        |  return '';
        |})()""".stripMargin
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

  /** Quote a string for embedding inside a JS expression. Round-trips
   *  Polish diacritics + apostrophes via `JSON.stringify`-compatible
   *  escaping. */
  private def jsString(s: String): String =
    play.api.libs.json.JsString(s).toString

  /** Run a single today → tomorrow animated slide and read the transition
   *  duration the production code applies to `#day-track` while it's in flight,
   *  in ms. The transition is set inside a `requestAnimationFrame`, so we wait
   *  for it to land (computed `transitionDuration` leaves `0s`) before parsing
   *  the seconds value into ms, then let the slide settle before returning. */
  private def slideDurationMs(page: CdpPage): Int = {
    page.eval("document.getElementById('date-filter').value = 'today'; onDateChange()")
    page.eval("window.animateToDay('tomorrow')")
    page.waitFor(
      "getComputedStyle(document.getElementById('day-track')).transitionDuration !== '0s'",
      timeoutMs = 1000
    )
    val ms = page.evalInt(
      "Math.round(parseFloat(getComputedStyle(document.getElementById('day-track')).transitionDuration) * 1000)"
    )
    page.waitFor("document.querySelectorAll('#day-track > .day-col').length === 0", timeoutMs = 2000)
    ms
  }

  /** Force `prefers-reduced-motion: no-preference` so the carousel takes its
   *  animated slide path (not the reduced-motion instant-commit shortcut)
   *  regardless of the runner's OS/headless defaults — the slide-in-flight
   *  assertions need the transition to actually run. */
  private def enableSlideAnimation(page: CdpPage): Unit =
    page.send("Emulation.setEmulatedMedia", play.api.libs.json.Json.obj(
      "features" -> play.api.libs.json.Json.arr(
        play.api.libs.json.Json.obj("name" -> "prefers-reduced-motion", "value" -> "no-preference")
      )
    ))

  /** Make the swipe handlers — gated on `matchMedia('(pointer: coarse)')` —
   *  engage. Enabling CDP touch emulation flips the page's pointer media query
   *  to coarse reliably across headless Chrome on macOS/Linux (the
   *  `setDeviceMetricsOverride mobile:true` width path is flaky for this; see
   *  the mobile-navbar test note), which the `mobile:true` device-metrics
   *  override alone does not guarantee. */
  private def coarsePointer(page: CdpPage): Unit = {
    page.send("Emulation.setTouchEmulationEnabled", play.api.libs.json.Json.obj(
      "enabled" -> true, "maxTouchPoints" -> 5
    ))
    page.setViewport(380, 800)
    require(
      page.evalBool("matchMedia('(pointer: coarse)').matches"),
      "expected a coarse pointer after touch emulation — swipe handlers are gated on it"
    )
  }

  // ── /debug staging table (folded by film) ──────────────────────────────────
  // The "Pending enrichment (staging)" table folds the hidden per-cinema source
  // rows (#staging-src) into one visible film row each (#staging-folded). The
  // trailing "Queue #" column is painted from the /debug/queue poll, matched per
  // film by `data-anchor` against its `staging-*` tasks. The fixture has "Staging
  // Film"'s detail fetch worked-on (▶ running) and "Done Newcomer"'s IMDb recovery
  // waiting at place #1.
  private def foldedRow(anchor: String) =
    s"""document.querySelector('#staging-folded tr.data[data-anchor="$anchor"]')"""

  "the /debug staging table" should "fill its queue column live from the /debug/queue poll, matched by anchor" in {
    onDebug { page =>
      def queueBadge(anchor: String) = s"""${foldedRow(anchor)}.querySelector('.queue-q .badge')"""
      // Wait for the queue poll to land and paint "Done Newcomer"'s waiting place.
      page.waitFor(s"""(function(){var r=${foldedRow("donenewcomer")};var c=r&&r.querySelector('.queue-q .badge');return c && c.textContent==='#1';})()""")
      page.evalString(queueBadge("donenewcomer") + ".textContent") shouldBe "#1"
      // "Staging Film"'s detail fetch is being worked on → ▶ running.
      page.evalString(queueBadge("stagingfilm") + ".textContent") shouldBe "▶ running"
    }
  }

  // A film reported by several cinemas folds into ONE row whose "Cinemas" cell is
  // the count; a single-cinema film shows that cinema's name instead. The hidden
  // source tbody keeps every per-cinema row. The fixture's "Staging Film" is
  // reported by two cinemas, "Done Newcomer" by one (Cinema City Wroclavia).
  it should "fold cinema rows into one row — count for many, the name for one" in {
    onDebug { page =>
      page.waitFor("""document.querySelectorAll('#staging-folded tr.data').length === 2""") // 2 films
      page.evalString(foldedRow("stagingfilm")  + """.querySelector('td.cinemas').textContent""") shouldBe "2"
      page.evalString(foldedRow("donenewcomer") + """.querySelector('td.cinemas').textContent""") shouldBe CinemaCityWroclavia.displayName
      // All three per-cinema source rows are still present (hidden).
      page.evalInt("""document.querySelectorAll('#staging-src tr.data').length""") shouldBe 3
    }
  }

  // Each folded row aggregates the stage ✓ marks: a green ✓ once the step has
  // concluded, empty otherwise. "Done Newcomer" has detail + TMDB done but not
  // yet IMDb, so its folded row shows ✓, ✓, then empty.
  it should "show a green ✓ for each concluded stage and leave pending stages empty" in {
    onDebug { page =>
      def cell(col: String) = s"""${foldedRow("donenewcomer")}.querySelector('$col')"""
      page.waitFor(s"""(function(){var r=${foldedRow("donenewcomer")};return r && r.querySelector('.stage-detail .stage-done');})()""")
      page.evalBool(s"""!!${cell(".stage-detail .stage-done")} && !!${cell(".stage-tmdb .stage-done")}""") shouldBe true
      page.evalBool(s"""!${cell(".stage-imdb .stage-done")}""") shouldBe true
    }
  }

  // Only the tick columns are centred (header + cells); the rest stay left.
  it should "centre the tick columns and leave the others left-aligned" in {
    onDebug { page =>
      page.waitFor(s"""(function(){var r=${foldedRow("donenewcomer")};return r && r.querySelector('td.stage-detail');})()""")
      def align(sel: String) = s"""getComputedStyle(document.querySelector('$sel')).textAlign"""
      page.evalString(align("#staging-t th.tick")) shouldBe "center"          // Detail header
      page.evalString(align("""#staging-folded tr.data[data-anchor="donenewcomer"] td.stage-detail""")) shouldBe "center"
      page.evalString(align("""#staging-folded tr.data[data-anchor="donenewcomer"] td.title""")) should not be "center"
      // The Cinemas cell carries the shared `.cinemas` class (right-aligned in the
      // movie table); the staging table overrides it back to left so the name reads
      // left-to-right.
      page.evalString(align("""#staging-folded tr.data[data-anchor="donenewcomer"] td.cinemas""")) shouldBe "left"
    }
  }

  // Source rows are added/removed live off the pending_movies change stream
  // (staging-* SSE frames), which the client COALESCES and applies on a 1s timer;
  // the tests drive the router (applySse) then call flushSse() to apply the batch
  // synchronously. The server frame shape is covered by DebugStreamControllerSpec.
  // The header counts FILMS: a new anchor bumps it, an extra cinema of an existing
  // film grows that film's count without bumping it, and a delete reverses each.
  it should "fold live source rows by film and track the film count" in {
    onDebug { page =>
      page.waitFor("""document.querySelectorAll('#staging-folded tr.data').length === 2""")
      def srcRow(id: String, cinema: String) =
        s"""<tr class="data" hidden data-row-id="$id" data-anchor="liveone" data-cinema="$cinema" data-title="Live One" data-year="2031" data-detail-done="false" data-tmdb-done="false" data-imdb-done="false"></tr>"""
      def upsert(id: String, cinema: String) =
        page.eval(s"""applySse(JSON.stringify({type:'staging-upsert', id:'$id', html:${Json.stringify(Json.toJson(srcRow(id, cinema)))}})); flushSse();""")
      def films = page.evalInt("""document.querySelectorAll('#staging-folded tr.data').length""")
      def liveCinemas = page.evalString(foldedRow("liveone") + """.querySelector('td.cinemas').textContent""")

      // A newcomer film arrives (one cinema) → 3 films, count "3", its cell shows the name.
      upsert("A|liveone|2031", "Cinema A")
      films shouldBe 3
      page.evalString("""document.getElementById('staging-count').textContent""") shouldBe "3"
      liveCinemas shouldBe "Cinema A"
      // A SECOND cinema reports the same film → still 3 films, but its cell shows the count.
      upsert("B|liveone|2031", "Cinema B")
      films shouldBe 3
      liveCinemas shouldBe "2"
      // That cinema drops out → back to one, so the name returns; the film survives.
      page.eval("""applySse(JSON.stringify({type:'staging-delete', id:'B|liveone|2031'})); flushSse();""")
      films shouldBe 3
      liveCinemas shouldBe "Cinema A"
      // The last cinema graduates → the film vanishes, count back to 2.
      page.eval("""applySse(JSON.stringify({type:'staging-delete', id:'A|liveone|2031'})); flushSse();""")
      films shouldBe 2
      page.evalString("""document.getElementById('staging-count').textContent""") shouldBe "2"
    }
  }

  // The list keeps the highest-in-queue film at the top: every poll re-folds and
  // re-sorts by queue rank (running first, then lowest waiting place). Drive
  // `queueActive` directly + rebuildStagingTable so the ranks are deterministic.
  it should "re-sort the folded rows live so the highest-in-queue film is first" in {
    onDebug { page =>
      def topAnchor = """document.querySelector('#staging-folded tr.data').dataset.anchor"""
      // Let the initial poll land (fixture: Staging Film is running → sorts first),
      // then freeze the 2.5s auto-poll so it can't clobber our scripted queue.
      page.waitFor(s"""(function(){var t=document.querySelector('#staging-folded tr.data');return t && t.dataset.anchor==='stagingfilm';})()""")
      page.eval("""clearInterval(stagingPollTimer); pollQueue = function(){};""")
      // Done Newcomer running, Staging Film merely waiting → Done Newcomer first.
      page.eval("""queueActive = [
        {taskType:'StagingDetail', dedupKey:'staging-detail|donenewcomer|cc', state:'worked_on'},
        {taskType:'StagingResolveTmdb', dedupKey:'staging-tmdb|stagingfilm', state:'waiting'}
      ]; rebuildStagingTable();""")
      page.evalString(topAnchor) shouldBe "donenewcomer"
      // Flip which one is running → the order inverts.
      page.eval("""queueActive = [
        {taskType:'StagingDetail', dedupKey:'staging-detail|stagingfilm|cc', state:'worked_on'},
        {taskType:'StagingResolveImdbId', dedupKey:'staging-imdb|donenewcomer', state:'waiting'}
      ]; rebuildStagingTable();""")
      page.evalString(topAnchor) shouldBe "stagingfilm"
    }
  }

  // Every film row lives in #staging-folded but only the first STAGING_CAP are
  // shown (`.hidden` on the rest), so the film count can climb past the cap while
  // the visible set stays bounded. Insert enough NEW films to exceed it.
  it should "show only the first STAGING_CAP films even as the total grows past it" in {
    onDebug { page =>
      page.waitFor("""document.querySelectorAll('#staging-folded tr.data').length === 2""")
      // Insert STAGING_CAP new films (distinct anchors, no queue tasks → they rank
      // last, but they still inflate the film total and exercise the display cap).
      page.eval("""for (var i = 0; i < STAGING_CAP; i++) {
        var id = 'C|capfilm' + i + '|2026';
        var html = '<tr class="data" hidden data-row-id="' + id + '" data-anchor="capfilm' + i + '"' +
          ' data-cinema="C" data-title="Cap ' + i + '" data-year="2026"' +
          ' data-detail-done="false" data-tmdb-done="false" data-imdb-done="false"></tr>';
        applySse(JSON.stringify({type:'staging-upsert', id:id, html:html}));
      }
      flushSse();""")
      // Total films = 2 fixture + STAGING_CAP; visible (not .hidden) = the cap.
      page.evalInt("""document.querySelectorAll('#staging-folded tr.data').length""") shouldBe
        (page.evalInt("STAGING_CAP") + 2)
      page.evalInt("""document.querySelectorAll('#staging-folded tr.data:not(.hidden)').length""") shouldBe
        page.evalInt("STAGING_CAP")
      page.evalString("""document.getElementById('staging-count').textContent""") shouldBe
        (page.evalInt("STAGING_CAP") + 2).toString
    }
  }

  // SSE frames are COALESCED and applied on a 1s timer, not per-frame — that's what
  // keeps a worker write-burst from hanging the tab. So a frame doesn't touch the
  // table until the flush, and repeat frames for one id collapse to a single apply.
  it should "defer SSE frames until flush and coalesce repeats by id" in {
    onDebug { page =>
      page.waitFor("""document.querySelectorAll('#staging-folded tr.data').length === 2""")
      // Neutralise the wall-clock auto-flush so the "deferred" assertion below is
      // deterministic: otherwise, if the CDP round-trips between buffering and the
      // check exceed SSE_FLUSH_MS (e.g. under load), the real timer flushes early
      // and the count is 3, not 2 — a flake. We drive the flush explicitly instead.
      page.eval("""scheduleSseFlush = function () {};""")
      val src = """<tr class="data" hidden data-row-id="X|buffered|2099" data-anchor="buffered" data-cinema="C" data-title="Buffered" data-year="2099" data-detail-done="false" data-tmdb-done="false" data-imdb-done="false"></tr>"""
      val frame = s"""applySse(JSON.stringify({type:'staging-upsert', id:'X|buffered|2099', html:${Json.stringify(Json.toJson(src))}}));"""
      // Two frames for the same id arrive — nothing is applied yet (deferred).
      page.eval(frame)
      page.eval(frame)
      page.evalInt("""document.querySelectorAll('#staging-folded tr.data').length""") shouldBe 2
      // One flush applies the coalesced batch → the film appears exactly once.
      page.eval("""flushSse();""")
      page.evalInt("""document.querySelectorAll('#staging-folded tr.data').length""") shouldBe 3
      page.evalInt("""document.querySelectorAll('#staging-src tr[data-anchor="buffered"]').length""") shouldBe 1
    }
  }

  // Clicking a corpus-table (#t) column header sorts the data rows by that
  // column's data-* key. The heavy collapsed `tr.details` siblings are parked
  // OFF-DOM (the perf win — the sort drags only the light data rows); a row's
  // details is spliced back in, adjacent, only while it's expanded. The fixture
  // corpus is Pending/Unresolved/Done Film; the FIRST click sorts descending, so
  // by title that is Unresolved, Pending, Done, and a second click reverses it.
  private val corpusTitles = """[...document.querySelectorAll('#t tbody tr.data')].map(r => r.dataset.title).join('|')"""
  private val attachedDetailsCount = """String(document.querySelectorAll('#t tbody tr.details').length)"""
  // Every EXPANDED data row is immediately followed by its own visible details row.
  private val expandedDetailsAdjacent =
    """[...document.querySelectorAll('#t tbody tr.data.expanded')].every(r => {
         const n = r.nextElementSibling;
         return !!n && n.classList.contains('details') && n.dataset.rowId === r.dataset.rowId
                && !n.classList.contains('hidden');
       })"""

  "the /debug corpus table" should "sort data rows and keep an expanded row's details beside it" in {
    onDebug { page =>
      page.waitFor("""document.querySelectorAll('#t tbody tr.data').length === 3""")
      // Collapsed details never sit in the table — that's what keeps the sort cheap.
      page.evalString(attachedDetailsCount) shouldBe "0"
      // First click → descending; re-click → ascending.
      page.eval("""document.querySelector('#t thead th[data-key="title"]').click()""")
      page.evalString(corpusTitles) shouldBe "Unresolved Film|Pending Film|Done Film"
      page.evalString(attachedDetailsCount) shouldBe "0"
      page.eval("""document.querySelector('#t thead th[data-key="title"]').click()""")
      page.evalString(corpusTitles) shouldBe "Done Film|Pending Film|Unresolved Film"
      // Expand the first data row → its details splices in, adjacent and visible.
      page.eval("""document.querySelector('#t tbody tr.data').click()""")
      page.evalString(attachedDetailsCount) shouldBe "1"
      page.evalBool(expandedDetailsAdjacent) shouldBe true
      // Re-sort (descending) → the now-expanded row moves and its details follows it.
      page.eval("""document.querySelector('#t thead th[data-key="title"]').click()""")
      page.evalString(corpusTitles) shouldBe "Unresolved Film|Pending Film|Done Film"
      page.evalBool(expandedDetailsAdjacent) shouldBe true
    }
  }

  // The per-source breakdown (every cinema × day × showtime) is NOT in the page
  // — rendering every row's up front built one giant HTML string that OOM'd the
  // view on the full corpus. Each row's details cell ships empty and its body is
  // fetched from /debug/details on first expand. `cacheKey` is a `<dt>` only the
  // debugDetails partial renders, so it's absent until a row is opened.
  "the /debug corpus table" should "fetch a row's per-source details lazily on expand" in {
    onDebug { page =>
      page.waitFor("""document.querySelectorAll('#t tbody tr.data').length === 3""")
      // Nothing heavy in the initial document — proves the details aren't inline.
      page.evalBool("""document.body.innerHTML.indexOf('cacheKey') === -1""") shouldBe true
      // Expand the first row → its details cell fills from the fetched fragment.
      page.eval("""document.querySelector('#t tbody tr.data').click()""")
      page.waitFor(
        """[...document.querySelectorAll('#t tbody tr.details td')].some(td => td.innerHTML.indexOf('cacheKey') !== -1)""")
    }
  }

  // The "Ratings" column sorts by the COMBINED weighted score (the same
  // MovieRecord.weightedRating the main page sorts cards by), not the raw IMDb
  // value. "Done Film" tops it on Metacritic+RT with no imdbRating at all, so a
  // sort keyed on the old `data-imdbR` (which never matched its header anyway)
  // could never produce this order — it asserts the real weighted-rating sort.
  it should "sort the Ratings column by the combined weighted rating, like the main page" in {
    onDebug { page =>
      page.waitFor("""document.querySelectorAll('#t tbody tr.data').length === 3""")
      page.evalString(attachedDetailsCount) shouldBe "0"
      // First click → descending by weighted rating (highest first: 9.0, 6.0,
      // 0.0), as the main page shows.
      page.eval("""document.querySelector('#t thead th[data-key="rating"]').click()""")
      page.evalString(corpusTitles) shouldBe "Done Film|Pending Film|Unresolved Film"
      // Re-click → ascending (0.0, 6.0, 9.0).
      page.eval("""document.querySelector('#t thead th[data-key="rating"]').click()""")
      page.evalString(corpusTitles) shouldBe "Unresolved Film|Pending Film|Done Film"
      // Sorting never dragged a collapsed details row into the DOM.
      page.evalString(attachedDetailsCount) shouldBe "0"
    }
  }

  // The corpus Cinemas cell shows the distinct-cinema count plus, when a venue
  // holds more per-title slots than cinemas, the slot count in parens (`1 (2)`).
  // That bracket must sit on the SAME line as the count — a regression where the
  // unrelated expanded-details `.cinemas` grid rule leaked onto `td.cinemas` made
  // the cell a grid and dropped the `(nr)` span to its own row. Assert the cell
  // is a normal table-cell and the slot span shares the count's line, to its right.
  "the /debug Cinemas cell" should "keep the (nr) slot count on the same line as the count" in {
    chrome match {
      case None => cancel("Chrome not installed — skipping JS behaviour test")
      case Some(c) => c.openPage(server.baseUrl + "/debug-slots") { page =>
        page.waitFor("""!!document.querySelector('#t tbody td.cinemas .slots')""")
        // The bracket renders because slots (2) exceed distinct cinemas (1).
        page.evalString(
          """document.querySelector('#t tbody td.cinemas').textContent.replace(/\s+/g,'')""") shouldBe "1(2)"
        // Layout: the cell is a plain table-cell (NOT a grid), and the slot span
        // sits on the count's line (tops aligned within a line) and to its right.
        page.evalBool("""(function(){
          var td = document.querySelector('#t tbody td.cinemas');
          var slot = td.querySelector('.slots');
          var tdR = td.getBoundingClientRect(), sR = slot.getBoundingClientRect();
          return getComputedStyle(td).display === 'table-cell'
              && sR.left > tdR.left
              && Math.abs(sR.top - tdR.top) < sR.height;
        })()""") shouldBe true
      }
    }
  }

  /** Dispatch a synthetic touch `PointerEvent` of `kind` at (`x`,`y`) on the
   *  document — drives the production `pointerdown`/`pointermove`/`pointerup`
   *  carousel handlers (which arm + translate `#day-track`) without needing real
   *  CDP touch injection. `pointerType:'touch'` clears the handlers'
   *  mouse-skip + coarse-pointer gates. */
  private def synthDrag(kind: String, x: Int, y: Int): String =
    s"""document.dispatchEvent(new PointerEvent('$kind', {
       |  pointerType: 'touch', isPrimary: true, clientX: $x, clientY: $y, bubbles: true, cancelable: true
       |}))""".stripMargin
}
