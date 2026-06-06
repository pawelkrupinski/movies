package views

import models.Poznan
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.FixtureTestWiring

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDateTime

/**
 * Whole-page HTML snapshot regression. Renders the two user-facing pages
 * (`/`, `/kina`) against the recorded `17-05-2026` fixture corpus and
 * diffs each rendered body against a checked-in expected file.
 */
class PageSnapshotSpec extends AnyFlatSpec with Matchers {

  private val now = LocalDateTime.of(2026, 5, 17, 0, 0)

  private implicit val city: models.City = Poznan

  private val anonymousUser    = Option.empty[models.User]
  private val noOauthProviders = Set.empty[String]

  private val snapshotDir = Paths.get("test/resources/fixtures/17-05-2026")

  // Boot the whole pipeline ONCE and render every page from the shared cache.
  // The scrape tick is the spec's dominant cost (~3s warm, ~15s cold-JIT) and
  // is identical for every page, so re-booting per test was pure waste — the
  // cache state after bootStartup is the same for all renders. (Sharing is safe
  // because the boot is deterministic; the tick stays sequential.)
  private lazy val wiring: FixtureTestWiring = {
    val w = new FixtureTestWiring("17-05-2026")
    w.bootStartup()
    w
  }
  private def svc = wiring.movieControllerService

  "the / page (repertoire view)" should "render the same HTML as the checked-in snapshot" in {
    val html = views.html.repertoire(
      svc.toSchedules(city, now), city.cinemaDisplayNames, city.cinemaPillMap,
      devMode = false, currentUser = anonymousUser, oauthProviders = noOauthProviders
    ).body
    assertSnapshot(snapshotDir.resolve("expected-index.html"), html)
  }

  "the /kina page" should "render the same HTML as the checked-in snapshot" in {
    val html = views.html.kina(
      svc.toCinemaSchedules(city, now), city.cinemaDisplayNames, city.cinemaPillMap,
      devMode = false, currentUser = anonymousUser, oauthProviders = noOauthProviders
    ).body
    assertSnapshot(snapshotDir.resolve("expected-kina.html"), html)
  }

  "the /kina/:cinema page" should "seed _kinaPinned from the URL-path cinema label" in {
    val html = views.html.kina(
      svc.toCinemaSchedules(city, now), city.cinemaDisplayNames, city.cinemaPillMap,
      devMode = false, currentUser = anonymousUser, oauthProviders = noOauthProviders,
      pinnedCinema = Some("Kino Apollo")
    ).body
    html should include ("""window._kinaPinned = "Kino Apollo";""")
  }

  // Multi-city coverage: the same corpus now carries Wrocław + Warszawa
  // cinemas, so each city's index renders its own scoped repertoire.
  "the Wrocław index" should "render the same HTML as the checked-in snapshot" in {
    val html = views.html.repertoire(
      svc.toSchedules(models.Wroclaw, now), models.Wroclaw.cinemaDisplayNames, models.Wroclaw.cinemaPillMap,
      devMode = false, currentUser = anonymousUser, oauthProviders = noOauthProviders
    )(models.Wroclaw).body
    assertSnapshot(snapshotDir.resolve("expected-wroclaw-index.html"), html)
  }

  "the Warszawa index" should "render the same HTML as the checked-in snapshot" in {
    val html = views.html.repertoire(
      svc.toSchedules(models.Warszawa, now), models.Warszawa.cinemaDisplayNames, models.Warszawa.cinemaPillMap,
      devMode = false, currentUser = anonymousUser, oauthProviders = noOauthProviders
    )(models.Warszawa).body
    assertSnapshot(snapshotDir.resolve("expected-warszawa-index.html"), html)
  }

  "the /plan page" should "render the same HTML as the checked-in snapshot" in {
    val data = controllers.PlanController.viewData(city, svc.toSchedules(city, now))
    val html = views.html.plan(
      data, city.cinemaDisplayNames, city.cinemaPillMap,
      devMode = false, currentUser = anonymousUser, oauthProviders = noOauthProviders
    ).body
    assertSnapshot(snapshotDir.resolve("expected-plan.html"), html)
  }

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
