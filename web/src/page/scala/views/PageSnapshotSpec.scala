package views

import testsupport.TestMessages.given

import models.Poznan
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.FixtureTestWiring

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDateTime

/**
 * Whole-page HTML snapshot regression. Renders the user-facing pages
 * (`/` per city, `/plan`) against the recorded `08-06-2026` fixture corpus
 * and diffs each rendered body against a checked-in expected file.
 */
class PageSnapshotSpec extends AnyFlatSpec with Matchers {

  private val now = LocalDateTime.of(2026, 6, 8, 0, 0)

  private implicit val city: models.City = Poznan

  private val anonymousUser    = Option.empty[models.User]
  private val noOauthProviders = Set.empty[String]

  private val snapshotDirectory = Paths.get("test/resources/fixtures/08-06-2026")

  // Boot the whole pipeline ONCE and render every page from the shared cache.
  // The scrape tick is the spec's dominant cost (~3s warm, ~15s cold-JIT) and
  // is identical for every page, so re-booting per test was pure waste — the
  // cache state after bootStartup is the same for all renders. (Sharing is safe
  // because the boot is deterministic; the tick stays sequential.)
  private lazy val wiring: FixtureTestWiring = {
    val w = new FixtureTestWiring("08-06-2026")
    // Load the read-model snapshot instead of the ~110s corpus boot. This spec
    // diffs the rendered HTML (read model → HTML); the snapshot's correctness
    // (pipeline → read model) is guarded by FilmScheduleEndToEndSpec, so the two
    // together still cover pipeline → HTML. See ReadModelSnapshot.
    w.bootFromSnapshotOrPipeline()
    w
  }
  // The web read transform, built from the worker-projected read model (the seam
  // the two apps share in production — web reads what worker projected).
  private def service = new controllers.MovieControllerService(wiring.webReadModel)

  "the / page (repertoire view)" should "render the same HTML as the checked-in snapshot" in {
    val html = views.html.repertoire(
      service.toSchedules(city, now), city.cinemaDisplayNames, city.cinemaPillMap,
      devMode = false, currentUser = anonymousUser, oauthProviders = noOauthProviders
    ).body
    assertSnapshot(snapshotDirectory.resolve("expected-index.html"), html)
  }

  // Multi-city coverage: the same corpus now carries Wrocław + Warszawa
  // cinemas, so each city's index renders its own scoped repertoire.
  "the Wrocław index" should "render the same HTML as the checked-in snapshot" in {
    val html = views.html.repertoire(
      service.toSchedules(models.Wroclaw, now), models.Wroclaw.cinemaDisplayNames, models.Wroclaw.cinemaPillMap,
      devMode = false, currentUser = anonymousUser, oauthProviders = noOauthProviders
    )(models.Wroclaw, summon[play.api.i18n.Messages]).body
    assertSnapshot(snapshotDirectory.resolve("expected-wroclaw-index.html"), html)
  }

  "the Warszawa index" should "render the same HTML as the checked-in snapshot" in {
    val html = views.html.repertoire(
      service.toSchedules(models.Warszawa, now), models.Warszawa.cinemaDisplayNames, models.Warszawa.cinemaPillMap,
      devMode = false, currentUser = anonymousUser, oauthProviders = noOauthProviders
    )(models.Warszawa, summon[play.api.i18n.Messages]).body
    assertSnapshot(snapshotDirectory.resolve("expected-warszawa-index.html"), html)
  }

  "the /plan page" should "render the same HTML as the checked-in snapshot" in {
    val data = controllers.PlanController.viewData(city, service.toSchedules(city, now))
    val html = views.html.plan(
      data, city.cinemaDisplayNames, city.cinemaPillMap,
      devMode = false, currentUser = anonymousUser, oauthProviders = noOauthProviders
    ).body
    assertSnapshot(snapshotDirectory.resolve("expected-plan.html"), html)
  }

  // Focused assertion for the navbar country switcher (independent of the whole
  // -page byte diff above): the PL deployment (Country.fromEnv default = Poland)
  // renders a #country-select listing every DEPLOYED country's host, with the
  // current country pre-selected. Germany (no webUrl) must NOT appear.
  "the navbar country switcher" should "render #country-select with each deployed country's host, current one selected" in {
    val html = views.html.repertoire(
      service.toSchedules(city, now), city.cinemaDisplayNames, city.cinemaPillMap,
      devMode = false, currentUser = anonymousUser, oauthProviders = noOauthProviders
    ).body

    html should include ("""id="country-select"""")
    html should include ("""onchange="onCountryChange(this.value)"""")
    // Both deployed hosts appear as option values...
    html should include ("""value="https://kinowo.fly.dev"""")
    html should include ("""value="https://showtimes-uk.fly.dev"""")
    // ...with the current country (Poland, fromEnv default) pre-selected.
    html should include ("""value="https://kinowo.fly.dev" selected""")
    // Germany has no deployment host, so it is absent from the switcher.
    html should not include "https://kinowo-de"
    html should not include ">Deutschland<"
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
