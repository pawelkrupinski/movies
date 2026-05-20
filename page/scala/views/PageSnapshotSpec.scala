package views

import models.Cinema
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.FixtureTestWiring

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDateTime

/**
 * Whole-page HTML snapshot regression. Renders the three user-facing pages
 * (`/`, `/kina`, `/ulubione`) against the recorded `17-05-2026` fixture
 * corpus and diffs each rendered body against a checked-in expected file.
 *
 * Catches the class of regression model-level specs miss:
 *   - a Twirl change that drops a field from a card,
 *   - a poster URL that suddenly comes out double-escaped,
 *   - a navbar tab that loses its `active` class,
 *   - a missing `data-cinema` / `data-title` / `data-time` attribute the
 *     client-side filter math depends on,
 *   - the `IS_FAVOURITES_PAGE` JS flag flipping on the wrong page.
 *
 * Pairs with `FilmScheduleEndToEndSpec`: that spec asserts the data the
 * controller hands the view, this one asserts the HTML the view emits
 * from the same data. Together they cover the full server-side render
 * pipeline from fixture HTTP responses to bytes on the wire.
 *
 * Snapshots live next to `expected-schedules.txt` so the whole fixture
 * (HTTP responses + expected outputs) ships as one self-contained set.
 * On first run after a fresh fixture the snapshot doesn't exist — the
 * test writes it and fails loudly so the diff goes through code review
 * before being trusted (same protocol as the schedules snapshot).
 */
class PageSnapshotSpec extends AnyFlatSpec with Matchers {

  // Pin the clock to the fixture's capture date so the recorded showtimes
  // are still "in the future" from the test's point of view. Same `now`
  // FilmScheduleEndToEndSpec uses — any drift between the two specs
  // would mean one is asserting against a snapshot the other doesn't see.
  private val now = LocalDateTime.of(2026, 5, 17, 0, 0)

  // Anonymous browser, no OAuth providers configured. Locks the snapshot
  // to the "no auth pill, no avatar, no logout form" navbar shape — keeps
  // the snapshot stable regardless of which OAuth secrets the developer
  // happens to have in `.env.local`.
  private val anonymousUser    = Option.empty[models.User]
  private val noOauthProviders = Set.empty[String]
  private val noFavMovies      = Set.empty[String]
  private val noFavScreenings  = Set.empty[String]

  private val snapshotDir = Paths.get("test/resources/fixtures/17-05-2026")

  "the / page (repertoire view)" should "render the same HTML as the checked-in snapshot" in
    new FixtureTestWiring("17-05-2026") {
      bootStartup()
      val html: String = views.html.repertoire(
        movieControllerService.toSchedules(now),
        Cinema.all.map(_.displayName),
        devMode = false,
        currentUser = anonymousUser,
        oauthProviders = noOauthProviders,
        favouriteMovies = noFavMovies,
        favouriteScreenings = noFavScreenings,
        favouritesMode = false
      ).body
      assertSnapshot(snapshotDir.resolve("expected-index.html"), html)
    }

  "the /kina page" should "render the same HTML as the checked-in snapshot" in
    new FixtureTestWiring("17-05-2026") {
      bootStartup()
      val html: String = views.html.kina(
        movieControllerService.toCinemaSchedules(now),
        Cinema.all.map(_.displayName),
        devMode = false,
        currentUser = anonymousUser,
        oauthProviders = noOauthProviders,
        favouriteMovies = noFavMovies,
        favouriteScreenings = noFavScreenings
      ).body
      assertSnapshot(snapshotDir.resolve("expected-kina.html"), html)
    }

  "the /kina/:cinema page" should "seed _kinaPinned from the URL-path cinema label" in
    new FixtureTestWiring("17-05-2026") {
      bootStartup()
      val html: String = views.html.kina(
        movieControllerService.toCinemaSchedules(now),
        Cinema.all.map(_.displayName),
        devMode = false,
        currentUser = anonymousUser,
        oauthProviders = noOauthProviders,
        favouriteMovies = noFavMovies,
        favouriteScreenings = noFavScreenings,
        pinnedCinema = Some("Kino Apollo")
      ).body
      html should include ("""let _kinaPinned = "Kino Apollo";""")
    }

  "the /ulubione page (repertoire view, favouritesMode=true)" should
    "render the same HTML as the checked-in snapshot" in
    new FixtureTestWiring("17-05-2026") {
      bootStartup()
      val html: String = views.html.repertoire(
        movieControllerService.toSchedules(now),
        Cinema.all.map(_.displayName),
        devMode = false,
        currentUser = anonymousUser,
        oauthProviders = noOauthProviders,
        favouriteMovies = noFavMovies,
        favouriteScreenings = noFavScreenings,
        favouritesMode = true
      ).body
      assertSnapshot(snapshotDir.resolve("expected-ulubione.html"), html)
    }

  // The other /ulubione path: a logged-in user with a small favourite
  // set. The controller pre-filters `toSchedules()` to that subset, so
  // the rendered HTML should be ~100× smaller than the anonymous-/full-
  // catalogue case above. This snapshot also locks in the navbar shape
  // with no Filtry / search / date / hidden-films controls.
  "the /ulubione page (logged-in, 1 favourite)" should
    "render only the favourited film + suppress the filter UI in the navbar" in
    new FixtureTestWiring("17-05-2026") {
      bootStartup()
      val pradaSchedule = movieControllerService.toSchedules(now)
        .find(_.movie.title == "Diabeł ubiera się u Prady 2")
        .getOrElse(fail("Prada not in the rendered schedules — fixture mismatch"))
      val loggedInUser = Some(models.User(
        id          = "test-user",
        provider    = "google",
        providerSub = "test-sub",
        email       = Some("test@example.com"),
        displayName = Some("Test User"),
        avatarUrl   = None,
        createdAt   = java.time.Instant.parse("2026-01-01T00:00:00Z"),
        lastSeenAt  = java.time.Instant.parse("2026-05-17T00:00:00Z")
      ))
      val html: String = views.html.repertoire(
        Seq(pradaSchedule),                                        // server-pre-filtered
        Cinema.all.map(_.displayName),
        devMode = false,
        currentUser = loggedInUser,
        oauthProviders = noOauthProviders,
        favouriteMovies = Set("Diabeł ubiera się u Prady 2"),
        favouriteScreenings = noFavScreenings,
        favouritesMode = true
      ).body
      // Sanity-check the trimmed navbar: filter UI elements not in DOM.
      html should not include """id="format-filter-btn""""    // Filtry button
      html should not include """id="search-input""""         // search box
      html should not include """id="date-filter""""          // date select
      html should not include """id="show-hidden-btn""""      // hidden-films flyout
      // The single favourited film is in there.
      html should include ("""Diabeł ubiera się u Prady 2""")
      // …and no other film cards from the corpus made it through.
      val cardCount = "data-title=\"".r.findAllIn(html).size
      cardCount shouldBe 1  // exactly one `.col[data-title="…"]`
    }

  /** Same protocol as `FilmScheduleEndToEndSpec`'s snapshot check: when the
   *  expected file is missing, write what was rendered and fail loudly so
   *  the diff goes through code review before being trusted. */
  private def assertSnapshot(expectedPath: Path, actual: String): Unit = {
    if (!Files.exists(expectedPath)) {
      Files.write(expectedPath, actual.getBytes(StandardCharsets.UTF_8))
      fail(s"Snapshot didn't exist — wrote $expectedPath. Review the contents, commit, and re-run.")
    }
    val expected = new String(Files.readAllBytes(expectedPath), StandardCharsets.UTF_8)
    withClue(
      s"Page HTML snapshot mismatch at $expectedPath. To regenerate after an intentional change:\n" +
        s"  rm $expectedPath && sbt 'testOnly views.PageSnapshotSpec'\n"
    ) {
      actual shouldBe expected
    }
  }
}
